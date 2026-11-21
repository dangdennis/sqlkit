(* The Eio drivers behind PgBouncer in transaction mode, where one backend
   serves many clients and a driver that prepares statements can be handed a
   backend holding somebody else's.

     docker compose up -d pgbouncer pgbouncer-prepared
     dune exec example/pooler_checks.exe -- pgwire|eio

   56432 has no prepared-statement support; 56433 has. The direct server on
   55432 is used to watch what the backends are doing. *)

let driver = match Sys.argv with [| _; d |] -> d | _ -> "pgwire"
let failures = ref 0
let reasons : string list ref = ref []

let refusal (e : Sqlkit.Driver.error) =
  let r =
    Printf.sprintf "%s %s"
      (match e.sqlstate with Some c -> Sqlkit.Sqlstate.to_string c | None -> "-")
      e.message
  in
  if (not (List.mem r !reasons)) && List.length !reasons < 6 then reasons := r :: !reasons

let check what b =
  if b then Printf.printf "ok   %s\n%!" what
  else (
    Printf.printf "FAIL %s\n%!" what;
    incr failures)

let url port =
  Printf.sprintf "postgresql://sqlkit:sqlkit@127.0.0.1:%d/sqlkit?sslmode=disable" port

let text s = Sqlkit.Value.Text s

let query (Sqlkit.Driver.Conn ((module D), raw, _)) sql params =
  D.query raw ~sql ~params ~columns:0

let exec (Sqlkit.Driver.Conn ((module D), raw, _)) sql = D.exec raw ~sql ~params:[]

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock and net = env#net in
  let get = function Ok c -> c | Error e -> failwith (Sqlkit.Error.to_string e) in
  let connect ?statement_cache port =
    get
      (if driver = "pgwire" then
         Sqlkit_pgwire.connect ~sw ~net ~clock ?statement_cache (url port)
       else Sqlkit_eio.connect ~sw ~clock ?statement_cache (url port))
  in
  let admin = get (Sqlkit_pgwire.connect ~sw ~net ~clock (url 55432)) in
  (* Two clients, each preparing its first statement while it holds a
     different backend, then taking turns so that each keeps landing on the
     backend the other one prepared on. *)
  let collide ?statement_cache ~in_transactions port =
    let a = connect ?statement_cache port and b = connect ?statement_cache port in
    ignore (exec a "BEGIN");
    ignore (exec b "BEGIN");
    ignore (query a "SELECT 'A:' || $1::text" [ text "warm" ]);
    ignore (query b "SELECT 'B:' || $1::text" [ text "warm" ]);
    ignore (exec a "COMMIT");
    ignore (exec b "COMMIT");
    let right = ref 0 and wrong = ref 0 and refused = ref 0 in
    for i = 1 to 40 do
      List.iter
        (fun (conn, who, sql) ->
          let arg = Printf.sprintf "%s%d" who i in
          if in_transactions then ignore (exec conn "BEGIN");
          (match query conn sql [ text arg ] with
          | Ok [ [| Sqlkit.Value.Text got |] ]
            when got = String.uppercase_ascii who ^ ":" ^ arg ->
              incr right
          | Ok _ -> incr wrong
          | Error e ->
              refusal e;
              incr refused);
          if in_transactions then ignore (exec conn "ROLLBACK"))
        [ (a, "a", "SELECT 'A:' || $1::text"); (b, "b", "SELECT 'B:' || $1::text") ]
    done;
    (!right, !wrong, !refused)
  in
  let report what (right, wrong, refused) =
    Printf.printf "     %s: %d right, %d wrong, %d refused\n%!" what right wrong refused
  in
  List.iter
    (fun (port, label) ->
      let r = collide ~in_transactions:false port in
      report label r;
      check
        (label ^ ": no statement ever returns another client's rows")
        (match r with _, 0, _ -> true | _ -> false);
      check
        (label ^ ": outside transactions every statement succeeds")
        (match r with 80, 0, 0 -> true | _ -> false);
      let r = collide ~in_transactions:true port in
      report (label ^ ", in transactions") r;
      check
        (label ^ ": in transactions, still never another client's rows")
        (match r with _, 0, _ -> true | _ -> false);
      let r = collide ~statement_cache:false ~in_transactions:true port in
      report (label ^ ", cache off, in transactions") r;
      check
        (label ^ ": with the cache off everything succeeds, in transactions too")
        (match r with 80, 0, 0 -> true | _ -> false))
    [ (56432, "no prepared-statement support"); (56433, "max_prepared_statements") ];
  (* Many fibers through a pool, more clients than the bouncer has backends. *)
  List.iter
    (fun (port, statement_cache, label) ->
      let use =
        if driver = "pgwire" then
          let pool =
            get
              (Sqlkit_pgwire.Pool.create ~sw ~net ~clock ~max_size:12 ~statement_cache
                 (url port))
          in
          fun f -> Sqlkit_pgwire.Pool.use pool f
        else
          let pool =
            get
              (Sqlkit_eio.Pool.create ~sw ~clock ~max_size:12 ~statement_cache (url port))
          in
          fun f -> Sqlkit_eio.Pool.use pool f
      in
      let right = ref 0 and wrong = ref 0 and refused = ref 0 in
      Eio.Fiber.List.iter
        (fun worker ->
          for i = 1 to 60 do
            let token = Printf.sprintf "%d-%d" worker i in
            (* a different statement per worker, so names differ per client *)
            let sql = Printf.sprintf "SELECT %d::text || '/' || $1::text" worker in
            match use (fun c -> Ok (query c sql [ text token ])) with
            | Ok (Ok [ [| Sqlkit.Value.Text got |] ])
              when got = string_of_int worker ^ "/" ^ token ->
                incr right
            | Ok (Ok _) -> incr wrong
            | Ok (Error e) ->
                refusal e;
                incr refused
            | Error e ->
                refusal (Sqlkit.Driver.error (Sqlkit.Error.to_string e));
                incr refused
          done)
        (List.init 24 Fun.id);
      report label (!right, !wrong, !refused);
      check (label ^ ": pooled, all 1440 answers right") (!right = 1440 && !wrong = 0))
    [
      (56432, true, "no support, cache on");
      (56432, false, "no support, cache off");
      (56433, true, "max_prepared_statements, cache on");
    ];
  (* Cancelling the fiber must stop the statement on the server, not just
     stop waiting for it: the pooler has to see the cancel before the client
     goes away. *)
  let sleepers () =
    match
      query admin
        "SELECT count(*)::text FROM pg_stat_activity WHERE query LIKE 'SELECT \
         pg_sleep(7.7%' AND state = 'active'"
        []
    with
    | Ok [ [| Sqlkit.Value.Text n |] ] -> int_of_string n
    | _ -> -1
  in
  List.iter
    (fun (port, label) ->
      let conn = connect port in
      Eio.Fiber.first
        (fun () -> ignore (query conn "SELECT pg_sleep(7.7)::text" []))
        (fun () ->
          let rec started fuel =
            if sleepers () > 0 || fuel = 0 then ()
            else (
              Eio.Time.Mono.sleep clock 0.02;
              started (fuel - 1))
          in
          started 100);
      let rec stopped fuel =
        sleepers () = 0
        || fuel > 0
           &&
           (Eio.Time.Mono.sleep clock 0.05;
            stopped (fuel - 1))
      in
      check (label ^ ": a cancelled statement stops on the server") (stopped 40))
    [ (56432, "through the pooler"); (55432, "direct") ];
  List.iter (Printf.printf "     refused with: %s\n") (List.rev !reasons);
  if !failures = 0 then print_endline "all good"
  else (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
