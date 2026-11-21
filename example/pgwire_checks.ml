(* Connection checks for the pure OCaml driver against the TLS server in
   docker-compose.yml (port 55433: TLS on, a certificate for "localhost" signed
   by a throwaway CA, MD5 passwords). The main e2e suite covers SCRAM without
   TLS.

     docker compose up -d postgres-tls
     docker exec sqlkit-pg-tls cat /var/lib/sqlkit-tls/ca.pem > /tmp/sqlkit-ca.pem
     dune exec example/pgwire_checks.exe -- /tmp/sqlkit-ca.pem *)

let failures = ref 0

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    incr failures)

let () =
  let root = match Sys.argv with [| _; path |] -> path | _ -> "" in
  if root = "" then (
    prerr_endline "usage: pgwire_checks CA_CERTIFICATE.pem";
    exit 2);
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let connect conninfo =
    Sqlkit_pgwire.connect ~sw ~net:env#net ~clock:env#mono_clock ~connect_timeout:5.
      conninfo
  in
  let base host =
    Printf.sprintf "host=%s port=55433 user=sqlkit password=sqlkit dbname=sqlkit" host
  in
  let rows conn sql =
    let (Sqlkit.Driver.Conn ((module D), raw, _)) = conn in
    D.query raw ~sql ~params:[] ~columns:0
  in
  let encrypted conn =
    rows conn "SELECT ssl::text FROM pg_stat_ssl WHERE pid = pg_backend_pid()"
  in
  let yes = Ok [ [| Sqlkit.Value.Text "true" |] ]
  and no = Ok [ [| Sqlkit.Value.Text "false" |] ] in
  let connects what conninfo expected =
    match connect conninfo with
    | Ok conn -> check what (encrypted conn = expected)
    | Error e ->
        print_endline (Sqlkit.Error.to_string e);
        check what false
  in
  let refuses what conninfo =
    check what
      (match connect conninfo with Error (Sqlkit.Error.Connect _) -> true | _ -> false)
  in
  connects "sslmode=disable connects in the clear (MD5 password)"
    (base "127.0.0.1" ^ " sslmode=disable")
    no;
  connects "default sslmode uses TLS when offered" (base "127.0.0.1") yes;
  connects "sslmode=require encrypts without verifying"
    (base "127.0.0.1" ^ " sslmode=require")
    yes;
  refuses "verify-full rejects a certificate the system does not trust"
    (base "localhost" ^ " sslmode=verify-full");
  connects "verify-full accepts it once its root is given"
    (base "localhost" ^ " sslmode=verify-full sslrootcert=" ^ root)
    yes;
  refuses "verify-full rejects a name the certificate does not cover"
    (base "127.0.0.1" ^ " sslmode=verify-full sslrootcert=" ^ root);
  connects "verify-ca accepts the chain whatever the name"
    (base "127.0.0.1" ^ " sslmode=verify-ca sslrootcert=" ^ root)
    yes;
  refuses "wrong password is a connect error"
    "host=127.0.0.1 port=55433 user=sqlkit password=nope dbname=sqlkit sslmode=require";
  refuses "missing password is a connect error"
    "host=127.0.0.1 port=55433 user=sqlkit dbname=sqlkit sslmode=require";
  refuses "unknown database is a connect error"
    "host=127.0.0.1 port=55433 user=sqlkit password=sqlkit dbname=nope sslmode=require";
  (* the cancel request has to work when the main connection is encrypted *)
  let pool =
    Result.get_ok
      (Sqlkit_pgwire.Pool.create ~sw ~net:env#net ~clock:env#mono_clock ~max_size:1
         ~query_timeout:0.2
         (base "127.0.0.1" ^ " sslmode=require"))
  in
  let pid conn = rows conn "SELECT pg_backend_pid()::text" in
  let first = Sqlkit_pgwire.Pool.use pool (fun c -> Ok (pid c)) in
  let slow =
    Sqlkit_pgwire.Pool.use pool (fun c ->
        let (Sqlkit.Driver.Conn ((module D), raw, _)) = c in
        Ok (D.exec raw ~sql:"SELECT pg_sleep(10)" ~params:[]))
  in
  check "deadline over TLS is reported"
    (match slow with Ok (Error e) -> e.timeout_seconds <> None | _ -> false);
  let admin = Result.get_ok (connect (base "127.0.0.1" ^ " sslmode=require")) in
  let sleepers () =
    rows admin
      "SELECT count(*)::text FROM pg_stat_activity WHERE query = 'SELECT pg_sleep(10)' \
       AND state = 'active'"
  in
  let rec settled fuel =
    sleepers () = Ok [ [| Sqlkit.Value.Text "0" |] ]
    || fuel > 0
       &&
       (Eio.Time.Mono.sleep env#mono_clock 0.02;
        settled (fuel - 1))
  in
  check "server stopped the statement after the deadline" (settled 100);
  check "pool replaced the connection"
    (match (first, Sqlkit_pgwire.Pool.use pool (fun c -> Ok (pid c))) with
    | Ok (Ok a), Ok (Ok b) -> a <> b
    | _ -> false);
  (* a large result and a large parameter through the TLS record layer *)
  let big = String.make (3 * 1024 * 1024) 'z' in
  let (Sqlkit.Driver.Conn ((module D), raw, _)) = admin in
  check "3 MB round trip over TLS"
    (D.query raw ~sql:"SELECT $1::text" ~params:[ Sqlkit.Value.Text big ] ~columns:1
    = Ok [ [| Sqlkit.Value.Text big |] ]);
  if !failures = 0 then print_endline "all good"
  else (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
