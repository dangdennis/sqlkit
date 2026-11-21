open Generated

let get = Sqlkit.get_exn
let ( let* ) = Result.bind
let org = Option.get (Uuidm.of_string "0c000000-0000-4000-8000-000000000001")

let cause =
  Sqlkit.Error.Invalid_argument { operation = "test"; message = "original failure" }

let code expected = function
  | Error e ->
      Option.map Sqlkit.Sqlstate.to_string (Sqlkit.Error.sqlstate e) = Some expected
  | Ok _ -> false

let counts ~check conn =
  let (Sqlkit.Driver.Conn ((module D), raw, _)) = conn in
  check "command without affected count remains unknown"
    (D.exec raw ~sql:"SET datestyle TO ISO" ~params:[] = Ok None);
  ignore (get (Db.production_delete conn ~org));
  check "affected rows: zero" (get (Db.production_update conn ~org ~label:"zero") = Some 0);
  check "affected rows: insert one"
    (get (Db.production_insert conn ~org ~count:1) = Some 1);
  check "affected rows: update one"
    (get (Db.production_update conn ~org ~label:"one") = Some 1);
  check "affected rows: delete one" (get (Db.production_delete conn ~org) = Some 1);
  check "affected rows: insert many"
    (get (Db.production_insert conn ~org ~count:3) = Some 3);
  check "affected rows: update many"
    (get (Db.production_update conn ~org ~label:"many") = Some 3);
  check "affected rows: delete many" (get (Db.production_delete conn ~org) = Some 3)

let run ~check admin =
  counts ~check admin;
  let events = ref [] in
  let observe e = events := e :: !events in
  let lost = get (Sqlkit_pg.connect ~observe (Sqlkit_pg.conninfo_of_env ())) in
  let pid = (get (Db.backend_pid lost)).pid in
  let result =
    Sqlkit.transaction ~retry:3 lost (fun _ ->
        check "terminate libpq transaction"
          (get (Db.terminate_backend admin ~pid)).terminated;
        Error cause)
  in
  check "libpq preserves original and rollback errors"
    (match result with
    | Error (Sqlkit.Error.Cleanup { primary = Some e; cleanup = Sqlkit.Error.Execute _ })
      ->
        e = cause
    | _ -> false);
  check "discarded libpq handle rejected"
    (Db.backend_pid lost = Error Sqlkit.Error.Connection_unusable);
  check "discarded connection close is idempotent" (Sqlkit.close lost = Ok ());
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stdenv = (env :> Caqti_eio.stdenv) in
  let uri = Sqlkit_caqti.uri_of_env () in
  let listener =
    Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:4
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  let stalled_uri =
    Uri.of_string
      (Printf.sprintf "postgresql://sqlkit@127.0.0.1:%d/sqlkit?sslmode=disable" port)
  in
  let timeout_result = ref None in
  Eio.Fiber.first
    (fun () ->
      timeout_result :=
        Some (Sqlkit_caqti.connect ~sw ~stdenv ~connect_timeout:0.05 stalled_uri))
    (fun () ->
      let _flow, _peer = Eio.Net.accept ~sw listener in
      Eio.Fiber.await_cancel ());
  check "connect handshake deadline is structured"
    (match !timeout_result with
    | Some (Error (Sqlkit.Error.Timeout { phase = Connect; _ })) -> true
    | _ -> false);
  check "invalid limits return an error"
    (match Sqlkit_caqti.Pool.create ~sw ~stdenv ~query_timeout:Float.nan uri with
    | Error (Sqlkit.Error.Invalid_argument _) -> true
    | _ -> false);
  let expired = ref None in
  Eio.Switch.run (fun scoped_sw ->
      expired := Some (get (Sqlkit_caqti.connect ~sw:scoped_sw ~stdenv uri)));
  check "standalone handle expires with switch"
    (Db.backend_pid (Option.get !expired) = Error Sqlkit.Error.Connection_unusable);

  let pool =
    get
      (Sqlkit_caqti.Pool.create ~sw ~stdenv ~max_size:1 ~acquire_timeout:0.1 ~observe uri)
  in
  let use f = Sqlkit_caqti.Pool.use pool f in
  get
    (use (fun c ->
         counts ~check c;
         Ok ()));
  let payload = String.make (8 * 1024 * 1024) 'x' in
  let size = get (use (fun c -> Db.payload_size c ~payload)) in
  check "large parameter upload flushes completely" (size.size = String.length payload);
  let escaped = ref None in
  get
    (use (fun c ->
         escaped := Some c;
         Ok ()));
  check "pool handle expires after borrow"
    (Db.backend_pid (Option.get !escaped) = Error Sqlkit.Error.Connection_unusable);
  let pid = get (use (fun c -> Result.map (fun r -> r.Db.pid) (Db.backend_pid c))) in
  let attempts = ref 0 in
  let result =
    Sqlkit_caqti.Pool.transaction ~retry:3 pool (fun _ ->
        incr attempts;
        check "terminate pooled transaction"
          (get (Db.terminate_backend admin ~pid)).terminated;
        Error cause)
  in
  check "pooled rollback preserves both errors"
    (match result with
    | Error (Sqlkit.Error.Cleanup { primary = Some e; _ }) -> e = cause
    | _ -> false);
  check "failed cleanup never retries body" (!attempts = 1);
  let replacement = get (use Db.backend_pid) in
  check "pool replaces discarded physical connection" (replacement.pid <> pid);
  let settings = get (use Db.session_limits) in
  check "replacement receives session limits"
    (settings.statement = "30s" && settings.lock = "5s" && settings.idle = "1min");
  (* The deadline applies to acquisition, not to the duration of the callback. *)
  get
    (use (fun _ ->
         Eio.Time.Mono.sleep env#mono_clock 0.15;
         let blocked = use Db.backend_pid in
         check "exhausted pool has acquisition deadline"
           (match blocked with
           | Error (Sqlkit.Error.Timeout { phase = Pool_acquire; _ }) -> true
           | _ -> false);
         Ok ()));
  ignore (get (use Db.backend_pid));
  check "pool usable after acquisition timeout" true;
  get
    (use (fun _ ->
         let cancelled_waiter = ref false in
         Eio.Fiber.first
           (fun () ->
             match use Db.backend_pid with
             | _ -> failwith "exhausted pool unexpectedly acquired"
             | exception (Eio.Cancel.Cancelled _ as e) ->
                 cancelled_waiter := true;
                 raise e)
           (fun () -> Eio.Time.Mono.sleep env#mono_clock 0.02);
         check "pool waiter cancellation propagates" !cancelled_waiter;
         Ok ()));
  ignore (get (use Db.backend_pid));
  check "cancelled waiter does not lose capacity" true;

  let wait_until_sleeping pid =
    let rec loop fuel =
      if (get (Db.backend_sleeping admin ~pid)).sleeping then ()
      else if fuel = 0 then failwith "backend never entered pg_sleep"
      else (
        Eio.Time.Mono.sleep env#mono_clock 0.01;
        loop (fuel - 1))
    in
    loop 500
  in
  let interrupt terminate =
    let pid = (get (use Db.backend_pid)).pid in
    let result = ref None in
    Eio.Fiber.both
      (fun () -> result := Some (use (fun c -> Db.wait_for_server c ~seconds:10.)))
      (fun () ->
        wait_until_sleeping pid;
        if terminate then
          check "terminate active query"
            (get (Db.terminate_backend admin ~pid)).terminated
        else check "cancel active query" (get (Db.cancel_backend admin ~pid)).cancelled);
    check "interrupted query reports failure without replay"
      (match !result with
      | Some (Error (Sqlkit.Error.Execute { sqlstate; _ })) when terminate ->
          sqlstate = None || Option.map Sqlkit.Sqlstate.to_string sqlstate = Some "57P01"
      | Some r when not terminate -> code "57014" r
      | _ -> false);
    let next = get (use Db.backend_pid) in
    check "pool recovers after interrupted query"
      (if terminate then next.pid <> pid else next.pid = pid)
  in
  interrupt false;
  interrupt true;
  let before = (get (use Db.backend_pid)).pid in
  let cancelled = ref false in
  Eio.Fiber.first
    (fun () ->
      match use (fun c -> Db.wait_for_server c ~seconds:10.) with
      | _ -> failwith "query unexpectedly completed"
      | exception (Eio.Cancel.Cancelled _ as e) ->
          cancelled := true;
          raise e)
    (fun () -> wait_until_sleeping before);
  check "active Eio cancellation propagates" !cancelled;
  check "cancelled connection is replaced" ((get (use Db.backend_pid)).pid <> before);
  let timed =
    get (Sqlkit_caqti.Pool.create ~sw ~stdenv ~max_size:1 ~query_timeout:0.05 uri)
  in
  let result = Sqlkit_caqti.Pool.use timed (fun c -> Db.wait_for_server c ~seconds:1.) in
  check "client query deadline is structured"
    (match result with
    | Error (Sqlkit.Error.Timeout { phase = Query; _ }) -> true
    | _ -> false);
  ignore (get (Sqlkit_caqti.Pool.use timed Db.backend_pid));
  check "client timeout discards and recovers" true;
  let limits =
    { Sqlkit.Limits.default with statement_timeout_ms = 50; lock_timeout_ms = 25 }
  in
  let limited = get (Sqlkit_caqti.Pool.create ~sw ~stdenv ~max_size:1 ~limits uri) in
  let limited_use f = Sqlkit_caqti.Pool.use limited f in
  check "server statement timeout retains SQLSTATE"
    (code "57014" (limited_use (fun c -> Db.wait_for_server c ~seconds:1.)));
  ignore (get (Db.production_delete admin ~org));
  ignore (get (Db.production_insert admin ~org ~count:1));
  get
    (Sqlkit.transaction admin (fun a ->
         let* _ = Db.production_update a ~org ~label:"held lock" in
         check "server lock timeout retains SQLSTATE"
           (code "55P03"
              (Sqlkit_caqti.Pool.transaction limited (fun c ->
                   Db.production_update c ~org ~label:"blocked")));
         Ok ()));
  ignore (get (limited_use (fun c -> Db.production_update c ~org ~label:"recovered")));
  check "lock timeout rolls back and pool recovers" true;
  ignore (get (Db.production_delete admin ~org));
  let bad_observer _ = failwith "observer unavailable" in
  let quiet = get (Sqlkit_caqti.Pool.create ~sw ~stdenv ~observe:bad_observer uri) in
  ignore (get (Sqlkit_caqti.Pool.use quiet Db.backend_pid));
  check "observer failure does not fail query" true;
  let has kind pred =
    List.exists (fun (e : Sqlkit.Telemetry.event) -> e.kind = kind && pred e) !events
  in
  check "query telemetry has name, elapsed time and SQLSTATE"
    (has Query (fun e ->
         e.name = "WaitForServer" && e.duration_s >= 0.
         &&
         match e.outcome with
         | Failure { sqlstate = Some s; _ } -> Sqlkit.Sqlstate.to_string s = "57014"
         | _ -> false));
  check "pool wait telemetry includes timeout"
    (has Pool_wait (fun e ->
         match e.outcome with
         | Failure { kind = Timeout; _ } -> e.duration_s >= 0.09
         | _ -> false));
  check "cleanup failure is observable"
    (has Cleanup (fun e -> match e.outcome with Failure _ -> true | _ -> false));
  check "cancellation is observable" (has Query (fun e -> e.outcome = Cancelled))
