let () = Sqlkit_postgresql_backend.register ()

(* Caqti driver for the sqlkit runtime, over Eio.

   Two things make this worth having over the libpq driver: connection pooling,
   and a concurrency story. Eio is direct style, so nothing in [Driver.S] or in
   any generated signature changes — a query still returns a plain [result], not
   a promise.

   Everything crosses the boundary as text. Caqti's Postgres driver sends
   parameters with unspecified type OIDs and lets the server infer them, so a
   [uuid], [numeric] or enum parameter works with no cast — verified, not
   assumed. Values come back as [Value.Text] / [Value.Null] and are parsed by
   [Sqlkit.Row], which is why those decoders accept text. *)

(* ---------- dynamic Caqti types ----------

   Caqti's codecs are static, but sqlkit's model is dynamic: a list of values in,
   an array of values out, with the shape known only at codegen time. Nesting
   [t2] existentially bridges the two. Nesting rather than [tup3]/[tup4] also
   means there is no arity ceiling — a 40-column row is no harder than a 3-column
   one. *)

type row_t = Row : 'a Caqti_type.t * ('a -> Sqlkit.Value.t list) -> row_t

let text = Caqti_type.(option string)

let rec row_type n =
  if n <= 0 then Row (Caqti_type.unit, fun () -> [])
  else
    match row_type (n - 1) with
    | Row (t, f) ->
        Row
          ( Caqti_type.t2 text t,
            fun (x, rest) ->
              (match x with None -> Sqlkit.Value.Null | Some s -> Sqlkit.Value.Text s)
              :: f rest )

type arg_t = Arg : 'a Caqti_type.t * (string option list -> 'a) -> arg_t

let rec arg_type n =
  if n <= 0 then Arg (Caqti_type.unit, fun _ -> ())
  else
    match arg_type (n - 1) with
    | Arg (t, f) ->
        Arg
          ( Caqti_type.t2 text t,
            fun l -> match l with x :: tl -> (x, f tl) | [] -> (None, f []) )

(* ---------- the driver ---------- *)

(* Caqti's own [cause] enumeration is deliberately incomplete, but the Postgres
   driver carries the raw SQLSTATE in its [Result_error_msg], so failures are
   still classified.

   Note the asymmetry with the libpq driver: Result_error_msg carries only the
   message and the SQLSTATE, so detail, hint, constraint name, table and column
   are all None here. If you need to know *which* constraint was violated
   rather than merely that one was, use sqlkit-postgresql. *)
let diag_of_caqti (e : [< Caqti_error.t ]) =
  let fallback () = Sqlkit.Driver.error (Caqti_error.show e) in
  match e with
  | `Request_failed qe | `Response_failed qe -> (
      match qe.Caqti_error.msg with
      | Sqlkit_postgresql_backend.Result_error_msg { error_message; sqlstate } ->
          Sqlkit.Driver.error error_message
            ?sqlstate:
              (match String.trim sqlstate with
              | "" -> None
              | s -> Some (Sqlkit.Sqlstate.of_string s))
      | _ -> fallback ())
  | _ -> fallback ()

(* Caqti parses query strings through its own grammar, which assigns meaning
   to `?` and `$(...)` -- re-parsing sqlkit's already-rewritten SQL would
   misread a jsonb `?` operator as a placeholder, and of_string_exn raises,
   breaking the result contract. Instead, split the SQL at its own $n sites
   (only `$` followed by digits; we produced them) and build the query value
   directly as literal / parameter / literal nodes. *)
let query_of_sql sql =
  let n = String.length sql in
  let parts = ref [] in
  let lit_start = ref 0 in
  let i = ref 0 in
  let flush_lit upto =
    if upto > !lit_start then
      parts := Caqti_query.L (String.sub sql !lit_start (upto - !lit_start)) :: !parts
  in
  while !i < n do
    if sql.[!i] = '$' && !i + 1 < n && sql.[!i + 1] >= '0' && sql.[!i + 1] <= '9' then begin
      flush_lit !i;
      let j = ref (!i + 1) in
      let v = ref 0 in
      while !j < n && sql.[!j] >= '0' && sql.[!j] <= '9' do
        v := (10 * !v) + Char.code sql.[!j] - Char.code '0';
        incr j
      done;
      parts := Caqti_query.P (!v - 1) :: !parts;
      i := !j;
      lit_start := !j
    end
    else incr i
  done;
  flush_lit n;
  Caqti_query.S (List.rev !parts)

module Raw = struct
  type conn = {
    db : (module Caqti_eio.CONNECTION);
    stdenv : Caqti_eio.stdenv;
    query_timeout : float;
    mutable closed : bool;
  }

  let now c =
    Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now c.stdenv#mono_clock)) /. 1e9

  let is_cancellation = function Eio.Cancel.Cancelled _ -> true | _ -> false

  let with_cleanup ~finally f =
    Fun.protect ~finally:(fun () -> Eio.Cancel.protect finally) f

  let close c =
    if not c.closed then (
      c.closed <- true;
      let module Db = (val c.db) in
      Db.disconnect ())

  let discard c =
    Eio.Cancel.protect (fun () ->
        match close c with
        | () -> ()
        | exception ((Out_of_memory | Stack_overflow | Sys.Break) as e) -> raise e
        | exception e when is_cancellation e -> raise e
        | exception _ -> ())

  let healthy c =
    if c.closed then false
    else
      let module Db = (val c.db) in
      let result = ref false in
      Db.check (fun ok -> result := ok);
      !result

  let guard c f =
    if c.closed then Error (Sqlkit.Driver.error "connection discarded")
    else
      let run () =
        match f () with
        | Ok _ as r -> r
        | Error d as r ->
            (match d.Sqlkit.Driver.sqlstate with
            | Some s
              when let code = Sqlkit.Sqlstate.to_string s in
                   (not (String.starts_with ~prefix:"08" code))
                   && code <> "57P01" && code <> "57P02" && code <> "57P03" ->
                ()
            | _ -> discard c);
            r
        | exception e when is_cancellation e ->
            discard c;
            raise e
        | exception ((Out_of_memory | Stack_overflow | Sys.Break) as e) ->
            discard c;
            raise e
        | exception e ->
            discard c;
            Error (Sqlkit.Driver.error (Printexc.to_string e))
      in
      match
        Eio.Time.Timeout.run
          (Eio.Time.Timeout.seconds c.stdenv#mono_clock c.query_timeout) (fun () ->
            Ok (run ()))
      with
      | Ok r -> r
      | Error `Timeout ->
          Error
            (Sqlkit.Driver.error ~timeout_seconds:c.query_timeout
               "query deadline exceeded")

  (* Caqti's request model has no raw COPY-stream surface; a clear error beats
     a slow row-at-a-time emulation that would silently change the semantics
     (COPY is all-or-nothing in one round-trip). Bulk loads use the libpq
     driver. *)
  let copy _ ~sql:_ ~rows:_ =
    Error
      (Sqlkit.Driver.error
         "COPY is not supported by the Caqti driver; run bulk loads over a Sqlkit_pg \
          (libpq) connection")

  let query c ~sql ~params ~columns =
    guard c (fun () ->
        let module Db = (val c.db) in
        let (Arg (at, mk)) = arg_type (List.length params) in
        let (Row (rt, get)) = row_type columns in
        (* ~oneshot: a fresh request value is created per call, and a non-oneshot
       request would register a new entry in Caqti's per-connection prepared
       cache every time -- unbounded growth for no reuse. The libpq driver is
       the one with a real statement cache. *)
        let req =
          Caqti_request.create ~oneshot:true at rt Caqti_mult.zero_or_more (fun _ ->
              query_of_sql sql)
        in
        match Db.collect_list req (mk (List.map Sqlkit.Value.to_pg_text params)) with
        | Ok rows -> Ok (List.map (fun r -> Array.of_list (get r)) rows)
        | Error e -> Error (diag_of_caqti e))

  let exec c ~sql ~params =
    guard c (fun () ->
        let module Db = (val c.db) in
        let (Arg (at, mk)) = arg_type (List.length params) in
        let req =
          Caqti_request.create ~oneshot:true at Caqti_type.unit Caqti_mult.zero (fun _ ->
              query_of_sql sql)
        in
        let count response =
          match Db.Response.exec response with
          | Error e -> Error e
          | Ok () -> (
              (* Caqti's PostgreSQL adapter parses an empty command tag with
             int_of_string. Catch that dependency exception here, before its
             outer recovery handler can reset a healthy transaction. *)
              match Db.Response.affected_count response with
              | Ok n -> Ok (Some n)
              | Error `Unsupported -> Ok None
              | Error e -> Error e
              | exception Failure _ -> Ok None)
        in
        match Db.call ~f:count req (mk (List.map Sqlkit.Value.to_pg_text params)) with
        | Ok n -> Ok n
        | Error `Unsupported -> Ok None
        | Error (#Caqti_error.t as e) -> Error (diag_of_caqti e))
end

let boundary ~operation f =
  match f () with
  | r -> r
  | exception ((Out_of_memory | Stack_overflow | Sys.Break) as e) -> raise e
  | exception e when Raw.is_cancellation e -> raise e
  | exception e ->
      Error (Sqlkit.Error.Callback { operation; message = Printexc.to_string e })

let validate ~limits ~connect_timeout ~query_timeout =
  match Sqlkit.Limits.validate limits with
  | Error message ->
      Error (Sqlkit.Error.Invalid_argument { operation = "connect"; message })
  | Ok () ->
      if
        List.for_all
          (fun n -> Float.is_finite n && n > 0.)
          [ connect_timeout; query_timeout ]
      then Ok ()
      else
        Error
          (Sqlkit.Error.Invalid_argument
             {
               operation = "connect";
               message = "client timeouts must be finite and positive";
             })

let open_raw ~sw ~stdenv ~limits ~connect_timeout ~query_timeout uri =
  if not (List.mem (Uri.scheme uri) [ Some "postgresql"; Some "postgres" ]) then
    Error
      (Sqlkit.Error.Invalid_argument
         { operation = "connect"; message = "expected a PostgreSQL URI" })
  else
    boundary ~operation:"connect" (fun () ->
        let uri = Uri.with_scheme uri (Some "sqlkit-postgresql") in
        match
          Eio.Time.Timeout.run
            (Eio.Time.Timeout.seconds stdenv#mono_clock connect_timeout) (fun () ->
              let result =
                match Caqti_eio_unix.connect ~sw ~stdenv uri with
                | Error e -> Error (Sqlkit.Error.Connect (Caqti_error.show e))
                | Ok db ->
                    let c = { Raw.db; stdenv; query_timeout; closed = false } in
                    let ready = ref false in
                    Raw.with_cleanup
                      ~finally:(fun () -> if not !ready then Raw.discard c)
                      (fun () ->
                        let rec setup = function
                          | [] ->
                              ready := true;
                              Ok c
                          | sql :: rest -> (
                              match Raw.exec c ~sql ~params:[] with
                              | Ok _ -> setup rest
                              | Error e -> Error (Sqlkit.Error.Connect e.message))
                        in
                        setup
                          (Sqlkit.Driver.session_setup @ Sqlkit.Limits.statements limits))
              in
              Ok result)
        with
        | Ok r -> r
        | Error `Timeout ->
            Error
              (Sqlkit.Error.Timeout
                 { operation = "connect"; phase = Connect; seconds = connect_timeout }))

let uri_of_env () =
  Uri.of_string
    (match Sys.getenv_opt "DATABASE_URL" with
    | Some u when String.trim u <> "" -> u
    | _ ->
        let get k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d in
        (* PGPASSWORD is the documented variable; dropping it here caused a
           silent auth failure that the libpq path did not have. Percent-encode
           the userinfo so special characters survive URI parsing. *)
        let enc v = Uri.pct_encode ~component:`Userinfo v in
        let userinfo =
          match Sys.getenv_opt "PGPASSWORD" with
          | Some pw when pw <> "" -> enc (get "PGUSER" "postgres") ^ ":" ^ enc pw
          | _ -> enc (get "PGUSER" "postgres")
        in
        Printf.sprintf "postgresql://%s@%s:%s/%s" userinfo (get "PGHOST" "127.0.0.1")
          (get "PGPORT" "5432") (get "PGDATABASE" "postgres"))

let connect ~sw ~stdenv ?(limits = Sqlkit.Limits.default) ?(connect_timeout = 10.)
    ?(query_timeout = 35.) ?observe uri =
  Result.bind (validate ~limits ~connect_timeout ~query_timeout) (fun () ->
      Result.map
        (fun c ->
          let conn = Sqlkit.Driver.make ?observe (module Raw) c in
          Eio.Switch.on_release sw (fun () ->
              c.Raw.closed <- true;
              Sqlkit.Driver.release conn);
          conn)
        (open_raw ~sw ~stdenv ~limits ~connect_timeout ~query_timeout uri))

module Pool = struct
  type t = {
    slots : Eio.Semaphore.t;
    mutable idle : Raw.conn list;
    mutable closed : bool;
    open_connection : unit -> (Raw.conn, Sqlkit.Error.t) result;
    stdenv : Caqti_eio.stdenv;
    acquire_timeout : float;
    observe : Sqlkit.Telemetry.observer;
  }

  let create ~sw ~stdenv ?(max_size = 10) ?(acquire_timeout = 5.) ?(connect_timeout = 10.)
      ?(query_timeout = 35.) ?(limits = Sqlkit.Limits.default)
      ?(observe = Sqlkit.Telemetry.ignore) uri =
    Result.bind (validate ~limits ~connect_timeout ~query_timeout) (fun () ->
        if
          max_size <= 0
          || (not (Float.is_finite acquire_timeout))
          || acquire_timeout <= 0.
        then
          Error
            (Sqlkit.Error.Invalid_argument
               {
                 operation = "Pool.create";
                 message = "pool size and acquisition timeout must be positive and finite";
               })
        else
          boundary ~operation:"Pool.create" (fun () ->
              let pool =
                {
                  slots = Eio.Semaphore.make max_size;
                  idle = [];
                  closed = false;
                  open_connection =
                    (fun () ->
                      open_raw ~sw ~stdenv ~limits ~connect_timeout ~query_timeout uri);
                  stdenv;
                  acquire_timeout;
                  observe;
                }
              in
              Eio.Switch.on_release sw (fun () ->
                  pool.closed <- true;
                  (* Caqti registered each physical disconnect on this switch.
                 Calling it again here would double-close detached hooks. *)
                  List.iter (fun c -> c.Raw.closed <- true) pool.idle;
                  pool.idle <- []);
              Ok pool))

  let acquire pool =
    let held = ref false and transferred = ref false in
    Raw.with_cleanup
      ~finally:(fun () ->
        if !held && not !transferred then Eio.Semaphore.release pool.slots)
      (fun () ->
        match
          Eio.Time.Timeout.run
            (Eio.Time.Timeout.seconds pool.stdenv#mono_clock pool.acquire_timeout)
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
                      if Raw.healthy c then Ok c
                      else (
                        Raw.discard c;
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
        if pool.closed || not (Raw.healthy c) then Raw.discard c
        else pool.idle <- c :: pool.idle)

  let use pool f =
    let started =
      Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now pool.stdenv#mono_clock))
      /. 1e9
    in
    let reported = ref false in
    let report outcome =
      if not !reported then (
        reported := true;
        let duration_s =
          max 0.
            (Int64.to_float
               (Mtime.to_uint64_ns (Eio.Time.Mono.now pool.stdenv#mono_clock))
             /. 1e9
            -. started)
        in
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
    Raw.with_cleanup
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
                let conn = Sqlkit.Driver.make ~observe:pool.observe (module Raw) c in
                Raw.with_cleanup
                  ~finally:(fun () ->
                    Sqlkit.Driver.release conn;
                    release pool c)
                  (fun () ->
                    report Sqlkit.Telemetry.Success;
                    Sqlkit.callback conn ~operation:"Pool.use" (fun () -> f conn))))

  let transaction ?isolation ?retry pool f =
    use pool (fun conn -> Sqlkit.transaction ?isolation ?retry conn f)
end
