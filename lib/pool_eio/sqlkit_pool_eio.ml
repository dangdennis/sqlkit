let boundary ~operation f =
  match f () with
  | r -> r
  | exception ((Out_of_memory | Stack_overflow | Sys.Break) as e) -> raise e
  | exception (Eio.Cancel.Cancelled _ as e) -> raise e
  | exception e ->
      Error (Sqlkit.Error.Callback { operation; message = Printexc.to_string e })

module Make
    (B : Sqlkit.Driver.S)
    (H : sig
      val healthy : B.conn -> bool
    end) =
struct
  type t = {
    slots : Eio.Semaphore.t;
    mutable idle : B.conn list;
    mutable closed : bool;
    open_connection : unit -> (B.conn, Sqlkit.Error.t) result;
    clock : Eio.Time.Mono.ty Eio.Resource.t;
    acquire_timeout : float;
    observe : Sqlkit.Telemetry.observer;
  }

  let create ~sw ~clock ~max_size ~acquire_timeout ~observe open_connection =
    if max_size <= 0 || (not (Float.is_finite acquire_timeout)) || acquire_timeout <= 0.
    then
      Error
        (Sqlkit.Error.Invalid_argument
           {
             operation = "Pool.create";
             message = "pool size and acquisition timeout must be positive and finite";
           })
    else
      let pool =
        {
          slots = Eio.Semaphore.make max_size;
          idle = [];
          closed = false;
          open_connection;
          clock :> Eio.Time.Mono.ty Eio.Resource.t;
          acquire_timeout;
          observe;
        }
      in
      (* Borrowed connections are closed by their own release, which sees
         [closed] and discards instead of returning them. *)
      Eio.Switch.on_release sw (fun () ->
          pool.closed <- true;
          List.iter B.close pool.idle;
          pool.idle <- []);
      Ok pool

  let acquire pool =
    let held = ref false and transferred = ref false in
    B.with_cleanup
      ~finally:(fun () ->
        if !held && not !transferred then Eio.Semaphore.release pool.slots)
      (fun () ->
        match
          Eio.Time.Timeout.run (Eio.Time.Timeout.seconds pool.clock pool.acquire_timeout)
            (fun () ->
              Eio.Semaphore.acquire pool.slots;
              held := true;
              let rec take () =
                if pool.closed then Error Sqlkit.Error.Connection_unusable
                else
                  match pool.idle with
                  | [] -> pool.open_connection ()
                  | c :: rest ->
                      pool.idle <- rest;
                      if H.healthy c then Ok c
                      else (
                        B.close c;
                        take ())
              in
              Ok (take ()))
        with
        | Error `Timeout ->
            Error
              (Sqlkit.Error.Timeout
                 {
                   operation = "Pool.use";
                   phase = Pool_acquire;
                   seconds = pool.acquire_timeout;
                 })
        | Ok (Error _ as r) -> r
        | Ok (Ok c) ->
            transferred := true;
            Ok c)

  let release pool c =
    Fun.protect
      ~finally:(fun () -> Eio.Semaphore.release pool.slots)
      (fun () ->
        if pool.closed || not (H.healthy c) then B.close c
        else pool.idle <- c :: pool.idle)

  let seconds pool =
    Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now pool.clock)) /. 1e9

  let use pool f =
    let started = seconds pool in
    let reported = ref false in
    let report outcome =
      if not !reported then (
        reported := true;
        let duration_s = max 0. (seconds pool -. started) in
        ignore
          (boundary ~operation:"observer" (fun () ->
               pool.observe
                 {
                   Sqlkit.Telemetry.kind = Pool_wait;
                   name = "Pool.use";
                   duration_s;
                   outcome;
                 };
               Ok ())))
    in
    B.with_cleanup
      ~finally:(fun () -> report Sqlkit.Telemetry.Cancelled)
      (fun () ->
        boundary ~operation:"Pool.use" (fun () ->
            match acquire pool with
            | Error e ->
                report
                  (Sqlkit.Telemetry.Failure
                     {
                       kind =
                         (match e with Sqlkit.Error.Timeout _ -> Timeout | _ -> Connect);
                       sqlstate = Sqlkit.Error.sqlstate e;
                     });
                Error e
            | Ok c ->
                let conn = Sqlkit.Driver.make ~observe:pool.observe (module B) c in
                B.with_cleanup
                  ~finally:(fun () ->
                    Sqlkit.Driver.release conn;
                    release pool c)
                  (fun () ->
                    report Sqlkit.Telemetry.Success;
                    Sqlkit.callback conn ~operation:"Pool.use" (fun () -> f conn))))

  let transaction ?isolation ?retry pool f =
    use pool (fun conn -> Sqlkit.transaction ?isolation ?retry conn f)
end
