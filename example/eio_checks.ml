(* Real-backend checks for the Eio drivers (libpq-based and pure OCaml). The pool half mirrors
   production_checks.ml so the two drivers are held to the same contract; the
   rest covers what only this driver has: full diagnostics over a pool, the
   pipelined statement cache, fibers that keep running during a query, and a
   server that is told to stop when a fiber is cancelled. *)

open Generated

let get = Sqlkit.get_exn
let ( let* ) = Result.bind
let org = Option.get (Uuidm.of_string "0e000000-0000-4000-8000-000000000001")

let cause =
  Sqlkit.Error.Invalid_argument { operation = "test"; message = "original failure" }

let code expected = function
  | Error e ->
      Option.map Sqlkit.Sqlstate.to_string (Sqlkit.Error.sqlstate e) = Some expected
  | Ok _ -> false

let driver_code (e : Sqlkit.Driver.error) =
  Option.map Sqlkit.Sqlstate.to_string e.sqlstate

(* Straight through Driver.S, so the driver's own behaviour is visible without
   the runtime's handling on top. *)
let driver ~check admin =
  let (Sqlkit.Driver.Conn ((module D), raw, _)) = admin in
  let exec sql = D.exec raw ~sql ~params:[] in
  let rows sql = D.query raw ~sql ~params:[] ~columns:0 in
  check "ddl runs"
    (exec "CREATE TEMP TABLE eio_checks (id int CONSTRAINT eio_checks_pk PRIMARY KEY)"
    = Ok None);
  check "insert reports one row" (exec "INSERT INTO eio_checks VALUES (1)" = Ok (Some 1));
  check "violation names constraint and table"
    (match exec "INSERT INTO eio_checks VALUES (1)" with
    | Error e ->
        driver_code e = Some "23505"
        && e.constraint_name = Some "eio_checks_pk"
        && e.table_name = Some "eio_checks"
        && e.detail <> None
    | Ok _ -> false);
  check "connection usable after a refused statement"
    (rows "SELECT id FROM eio_checks" = Ok [ [| Sqlkit.Value.Text "1" |] ]);
  check "syntax error at first use is reported, not cached"
    (match rows "SELEC 1" with Error e -> driver_code e = Some "42601" | Ok _ -> false);
  check "same bad statement fails the same way"
    (match rows "SELEC 1" with Error e -> driver_code e = Some "42601" | Ok _ -> false);
  check "NUL is refused before it reaches the server"
    (match rows "SELECT '\000'" with Error e -> e.sqlstate = None | Ok _ -> false);
  check "NUL refusal keeps the connection" (rows "SELECT 1" = Ok [ [| Text "1" |] ]);
  (* a cached plan whose result type changed underneath it *)
  ignore (rows "SELECT * FROM eio_checks");
  ignore (exec "ALTER TABLE eio_checks ADD COLUMN note text");
  check "stale plan is replaced outside a transaction"
    (rows "SELECT * FROM eio_checks" = Ok [ [| Text "1"; Null |] ]);
  (* cached plans the server has forgotten *)
  ignore (exec "DEALLOCATE ALL");
  check "forgotten statement is prepared again"
    (rows "SELECT * FROM eio_checks" = Ok [ [| Text "1"; Null |] ]);
  (* Inside a transaction the stale error aborts it. The retry is refused,
     the original error is reported, and a ROLLBACK the server has also
     forgotten still gets through. *)
  ignore (exec "BEGIN");
  ignore (exec "ROLLBACK");
  ignore (exec "BEGIN");
  ignore (exec "DEALLOCATE PREPARE ALL");
  check "inside a transaction the original error surfaces"
    (match rows "SELECT * FROM eio_checks" with
    | Error e -> driver_code e = Some "26000"
    | Ok _ -> false);
  check "a forgotten ROLLBACK still ends the transaction" (exec "ROLLBACK" = Ok None);
  check "and the statement works again afterwards"
    (rows "SELECT * FROM eio_checks" = Ok [ [| Text "1"; Null |] ]);
  (* COMMIT in a failed transaction: the server says yes and rolls back. *)
  ignore (exec "BEGIN");
  ignore (exec "INSERT INTO eio_checks VALUES (40)");
  ignore (exec "INSERT INTO eio_checks VALUES (1)");
  check "COMMIT of a failed transaction is reported as the rollback it is"
    (match exec "COMMIT" with Error e -> driver_code e = Some "25P02" | Ok _ -> false);
  check "and nothing from it was kept"
    (rows "SELECT count(*) FROM eio_checks WHERE id = 40" = Ok [ [| Text "0" |] ]);
  ignore (exec "BEGIN");
  check "an ordinary COMMIT still succeeds" (exec "COMMIT" = Ok None);
  check "copy needs a COPY statement"
    (match D.copy raw ~sql:"SELECT 1" ~rows:[] with Error _ -> true | Ok _ -> false);
  check "copy streams rows"
    (D.copy raw ~sql:"COPY eio_checks (id, note) FROM STDIN" ~rows:[ "2\ta"; "3\t\\N" ]
    = Ok 2);
  check "failed copy names the constraint"
    (match D.copy raw ~sql:"COPY eio_checks (id) FROM STDIN" ~rows:[ "4"; "2" ] with
    | Error e -> driver_code e = Some "23505" && e.constraint_name = Some "eio_checks_pk"
    | Ok _ -> false);
  check "statements run after copy, and the failed copy wrote nothing"
    (rows "SELECT count(*) FROM eio_checks" = Ok [ [| Text "3" |] ]);
  ignore (exec "DROP TABLE eio_checks")

(* What the checks need from a driver, with its environment already applied. *)
module type DRIVER = sig
  val conninfo : unit -> string

  val connect :
    sw:Eio.Switch.t ->
    ?connect_timeout:float ->
    ?observe:Sqlkit.Telemetry.observer ->
    string ->
    (Sqlkit.conn, Sqlkit.Error.t) result

  module Pool : sig
    type t

    val create :
      sw:Eio.Switch.t ->
      ?max_size:int ->
      ?acquire_timeout:float ->
      ?query_timeout:float ->
      ?limits:Sqlkit.Limits.t ->
      ?observe:Sqlkit.Telemetry.observer ->
      string ->
      (t, Sqlkit.Error.t) result

    val use :
      t -> (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) -> ('a, Sqlkit.Error.t) result

    val transaction :
      ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
      ?retry:int ->
      t ->
      (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) ->
      ('a, Sqlkit.Error.t) result
  end
end

let run (module D : DRIVER) ~check ~env ~sw admin =
  let clock = env#mono_clock in
  let conninfo = D.conninfo () in
  driver ~check admin;
  Production_checks.counts ~check admin;
  let events = ref [] in
  let observe e = events := e :: !events in

  (* a transaction whose backend is killed from under it *)
  let lost = get (D.connect ~sw ~observe conninfo) in
  let pid = (get (Db.backend_pid lost)).pid in
  let result =
    Sqlkit.transaction ~retry:3 lost (fun _ ->
        check "terminate standalone transaction"
          (get (Db.terminate_backend admin ~pid)).terminated;
        Error cause)
  in
  check "standalone preserves original and rollback errors"
    (match result with
    | Error (Sqlkit.Error.Cleanup { primary = Some e; cleanup = Sqlkit.Error.Execute _ })
      ->
        e = cause
    | _ -> false);
  check "discarded handle rejected"
    (Db.backend_pid lost = Error Sqlkit.Error.Connection_unusable);
  check "discarded connection close is idempotent" (Sqlkit.close lost = Ok ());

  (* a listener that accepts and then says nothing *)
  let listener =
    Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:4
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with `Tcp (_, port) -> port | _ -> 0
  in
  let stalled =
    Printf.sprintf "postgresql://sqlkit@127.0.0.1:%d/sqlkit?sslmode=disable" port
  in
  let timeout_result = ref None in
  Eio.Fiber.first
    (fun () -> timeout_result := Some (D.connect ~sw ~connect_timeout:0.05 stalled))
    (fun () ->
      let _flow, _peer = Eio.Net.accept ~sw listener in
      Eio.Fiber.await_cancel ());
  check "connect handshake deadline is structured"
    (match !timeout_result with
    | Some (Error (Sqlkit.Error.Timeout { phase = Connect; _ })) -> true
    | _ -> false);
  check "refused connection is a connect error"
    (match D.connect ~sw "postgresql://sqlkit@127.0.0.1:1/sqlkit?sslmode=disable" with
    | Error (Sqlkit.Error.Connect _) -> true
    | _ -> false);
  check "invalid limits return an error"
    (match D.Pool.create ~sw ~query_timeout:Float.nan conninfo with
    | Error (Sqlkit.Error.Invalid_argument _) -> true
    | _ -> false);
  let expired = ref None in
  Eio.Switch.run (fun scoped_sw ->
      expired := Some (get (D.connect ~sw:scoped_sw conninfo)));
  check "standalone handle expires with switch"
    (Db.backend_pid (Option.get !expired) = Error Sqlkit.Error.Connection_unusable);

  let pool = get (D.Pool.create ~sw ~max_size:1 ~acquire_timeout:0.1 ~observe conninfo) in
  let use f = D.Pool.use pool f in
  get
    (use (fun c ->
         Production_checks.counts ~check c;
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
    D.Pool.transaction ~retry:3 pool (fun _ ->
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

  (* a connection the server closed while it sat idle in the pool *)
  let idle_pid = (get (use Db.backend_pid)).pid in
  check "terminate idle pooled connection"
    (get (Db.terminate_backend admin ~pid:idle_pid)).terminated;
  Eio.Time.Mono.sleep clock 0.05;
  check "idle connection closed by the server is replaced before use"
    (match use Db.backend_pid with Ok r -> r.pid <> idle_pid | Error _ -> false);

  (* a borrower that leaves a transaction open *)
  let dirty_pid = ref 0 in
  get
    (use (fun c ->
         let (Sqlkit.Driver.Conn ((module D), raw, _)) = c in
         dirty_pid := (get (Db.backend_pid c)).pid;
         ignore (D.exec raw ~sql:"BEGIN" ~params:[]);
         Ok ()));
  check "connection returned mid-transaction is not reused"
    ((get (use Db.backend_pid)).pid <> !dirty_pid);

  (* The deadline applies to acquisition, not to the duration of the callback. *)
  get
    (use (fun _ ->
         Eio.Time.Mono.sleep clock 0.15;
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
           (fun () -> Eio.Time.Mono.sleep clock 0.02);
         check "pool waiter cancellation propagates" !cancelled_waiter;
         Ok ()));
  ignore (get (use Db.backend_pid));
  check "cancelled waiter does not lose capacity" true;

  let wait_until ~what pred =
    let rec loop fuel =
      if pred () then ()
      else if fuel = 0 then failwith what
      else (
        Eio.Time.Mono.sleep clock 0.01;
        loop (fuel - 1))
    in
    loop 500
  in
  let sleeping pid = (get (Db.backend_sleeping admin ~pid)).sleeping in
  let wait_until_sleeping pid =
    wait_until ~what:"backend never entered pg_sleep" (fun () -> sleeping pid)
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
    check
      (if terminate then "terminated query keeps the server's SQLSTATE"
       else "cancelled query reports 57014")
      (match !result with
      | Some r -> code (if terminate then "57P01" else "57014") r
      | None -> false);
    let next = get (use Db.backend_pid) in
    check "pool recovers after interrupted query"
      (if terminate then next.pid <> pid else next.pid = pid)
  in
  interrupt false;
  interrupt true;

  (* cancelling the fiber stops the work on the server, not just the wait *)
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
  wait_until ~what:"cancelled backend kept sleeping" (fun () -> not (sleeping before));
  check "server stopped the cancelled statement" true;
  check "cancelled connection is replaced" ((get (use Db.backend_pid)).pid <> before);

  let timed = get (D.Pool.create ~sw ~max_size:1 ~query_timeout:0.05 conninfo) in
  let timed_pid = (get (D.Pool.use timed Db.backend_pid)).pid in
  let result = D.Pool.use timed (fun c -> Db.wait_for_server c ~seconds:10.) in
  check "client query deadline is structured"
    (match result with
    | Error (Sqlkit.Error.Timeout { phase = Query; _ }) -> true
    | _ -> false);
  wait_until ~what:"timed-out backend kept sleeping" (fun () -> not (sleeping timed_pid));
  check "server stopped the timed-out statement" true;
  check "client timeout discards and recovers"
    ((get (D.Pool.use timed Db.backend_pid)).pid <> timed_pid);

  (* queries park their fiber instead of holding the domain *)
  let wide = get (D.Pool.create ~sw ~max_size:8 conninfo) in
  let ticks = ref 0 in
  let started = Unix.gettimeofday () in
  Eio.Fiber.first
    (fun () ->
      Eio.Fiber.List.iter
        (fun _ ->
          ignore (get (D.Pool.use wide (fun c -> Db.wait_for_server c ~seconds:0.3))))
        (List.init 8 Fun.id))
    (fun () ->
      while true do
        Eio.Time.Mono.sleep clock 0.01;
        incr ticks
      done);
  let elapsed = Unix.gettimeofday () -. started in
  check "eight 0.3 s queries on eight connections overlap" (elapsed < 1.2);
  check "other fibers run while queries wait" (!ticks >= 10);
  let seen = Hashtbl.create 8 in
  Eio.Fiber.List.iter
    (fun i ->
      get
        (D.Pool.use wide (fun c ->
             let* r = Db.payload_size c ~payload:(String.make (i * 1000) 'y') in
             Hashtbl.replace seen i r.size;
             Ok ())))
    (List.init 64 Fun.id);
  check "interleaved queries get their own answers"
    (List.for_all
       (fun i -> Hashtbl.find_opt seen i = Some (i * 1000))
       (List.init 64 Fun.id));

  let limits =
    { Sqlkit.Limits.default with statement_timeout_ms = 50; lock_timeout_ms = 25 }
  in
  let limited = get (D.Pool.create ~sw ~max_size:1 ~limits conninfo) in
  let limited_use f = D.Pool.use limited f in
  check "server statement timeout retains SQLSTATE"
    (code "57014" (limited_use (fun c -> Db.wait_for_server c ~seconds:1.)));
  ignore (get (Db.production_delete admin ~org));
  ignore (get (Db.production_insert admin ~org ~count:1));
  get
    (Sqlkit.transaction admin (fun a ->
         let* _ = Db.production_update a ~org ~label:"held lock" in
         check "server lock timeout retains SQLSTATE"
           (code "55P03"
              (D.Pool.transaction limited (fun c ->
                   Db.production_update c ~org ~label:"blocked")));
         Ok ()));
  ignore (get (limited_use (fun c -> Db.production_update c ~org ~label:"recovered")));
  check "lock timeout rolls back and pool recovers" true;
  ignore (get (Db.production_delete admin ~org));
  let bad_observer _ = failwith "observer unavailable" in
  let quiet = get (D.Pool.create ~sw ~observe:bad_observer conninfo) in
  ignore (get (D.Pool.use quiet Db.backend_pid));
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
