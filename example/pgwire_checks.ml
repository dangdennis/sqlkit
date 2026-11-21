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
  (* ---- a hostile server ----
     Each case plays one side of the protocol badly and watches what the
     client does. [script] gets the connection after the startup message has
     been read. *)
  let message tag payload =
    let b = Buffer.create (String.length payload + 5) in
    Buffer.add_char b tag;
    Buffer.add_int32_be b (Int32.of_int (String.length payload + 4));
    Buffer.add_string b payload;
    Buffer.contents b
  in
  let int32 n =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Bytes.to_string b
  in
  let auth code rest = message 'R' (int32 code ^ rest) in
  let ready = message 'Z' "I" in
  let current = message 'S' "server_version\00018.0\000" in
  let read_some flow =
    let buf = Cstruct.create 65536 in
    match Eio.Flow.single_read flow buf with
    | n -> Cstruct.to_string (Cstruct.sub buf 0 n)
    | exception End_of_file -> ""
    | exception Eio.Io _ -> ""
  in
  let hostile ?(parameters = "") script client =
    let listener =
      Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port = match Eio.Net.listening_addr listener with `Tcp (_, p) -> p | _ -> 0 in
    let result = ref None in
    Eio.Fiber.both
      (fun () ->
        Eio.Switch.run (fun server_sw ->
            let flow, _ = Eio.Net.accept ~sw:server_sw listener in
            ignore (read_some flow);
            (try script flow with Eio.Io _ | End_of_file -> ());
            Eio.Net.close listener))
      (fun () ->
        result :=
          Some
            (client
               (Printf.sprintf
                  "host=127.0.0.1 port=%d user=u password=hunter2 dbname=d \
                   sslmode=disable%s"
                  port parameters)));
    Option.get !result
  in
  let connect_refused = function Error (Sqlkit.Error.Connect _) -> true | _ -> false in
  let seen = ref "" in
  check "password is not sent in clear over an unencrypted connection"
    (connect_refused
       (hostile
          (fun flow ->
            Eio.Flow.copy_string (auth 3 "") flow;
            seen := read_some flow)
          connect)
    && !seen = "");
  seen := "";
  check "unless the caller asked for exactly that"
    (match
       hostile ~parameters:" require_auth=password"
         (fun flow ->
           Eio.Flow.copy_string (auth 3 "") flow;
           seen := read_some flow;
           Eio.Flow.copy_string (auth 0 "" ^ current ^ ready) flow;
           ignore (read_some flow);
           Eio.Flow.copy_string ready flow)
         connect
     with
    | Ok _ -> !seen = message 'p' "hunter2\000"
    | Error _ -> false);
  seen := "";
  check "a password request after SCRAM has started is refused"
    (connect_refused
       (hostile ~parameters:" require_auth=scram-sha-256,password"
          (fun flow ->
            Eio.Flow.copy_string (auth 10 "SCRAM-SHA-256\000\000") flow;
            ignore (read_some flow);
            Eio.Flow.copy_string (auth 3 "") flow;
            seen := read_some flow)
          connect)
    && !seen = "");
  check "require_auth=scram-sha-256 refuses a server that asks for nothing"
    (connect_refused
       (hostile ~parameters:" require_auth=scram-sha-256"
          (fun flow -> Eio.Flow.copy_string (auth 0 "" ^ current ^ ready) flow)
          connect));
  let started = Unix.gettimeofday () in
  check "an absurd SCRAM cost is refused, not computed"
    (connect_refused
       (hostile
          (fun flow ->
            Eio.Flow.copy_string (auth 10 "SCRAM-SHA-256\000\000") flow;
            let initial = read_some flow in
            (* echo the client's nonce, extended, with a ruinous iteration count *)
            let nonce =
              match String.index_opt initial '=' with
              | Some _ ->
                  let i = String.length initial - 24 in
                  String.sub initial i 24
              | None -> ""
            in
            Eio.Flow.copy_string
              (auth 11 (Printf.sprintf "r=%sXYZ,s=c2FsdA==,i=10000000" nonce))
              flow;
            ignore (read_some flow))
          connect)
    && Unix.gettimeofday () -. started < 2.);
  let status k v = message 'S' (k ^ "\000" ^ v ^ "\000") in
  check "a server older than PostgreSQL 15 is refused by name"
    (match
       hostile
         (fun flow ->
           Eio.Flow.copy_string (auth 0 "" ^ status "server_version" "14.19" ^ ready) flow;
           ignore (read_some flow))
         connect
     with
    | Error (Sqlkit.Error.Connect why) ->
        why = "PostgreSQL 15 or later is required; the server is 14.19"
    | _ -> false);
  check "an oversized message before authentication is refused"
    (connect_refused
       (hostile
          (fun flow ->
            Eio.Flow.copy_string ("R" ^ int32 0x40000004) flow;
            ignore (read_some flow))
          connect));
  check "a reply stream past max_result_bytes fails the statement"
    (match
       hostile
         (fun flow ->
           Eio.Flow.copy_string (auth 0 "" ^ current ^ ready) flow;
           ignore (read_some flow);
           Eio.Flow.copy_string ready flow;
           ignore (read_some flow);
           let row = message 'D' "\000\001" ^ "" in
           let row = message 'D' ("\000\001" ^ int32 1000 ^ String.make 1000 'x') ^ row in
           let chunk = String.concat "" (List.init 200 (fun _ -> row)) in
           for _ = 1 to 200 do
             Eio.Flow.copy_string chunk flow
           done)
         (fun conninfo ->
           Sqlkit_pgwire.connect ~sw ~net:env#net ~clock:env#mono_clock
             ~connect_timeout:5. ~max_result_bytes:(1024 * 1024) conninfo
           |> Result.map (fun conn -> rows conn "SELECT 1"))
     with
    | Ok (Error e) -> e.sqlstate = None
    | _ -> false);
  check "a second ReadyForQuery is not taken for the next statement's reply"
    (match
       hostile
         (fun flow ->
           Eio.Flow.copy_string (auth 0 "" ^ current ^ ready) flow;
           ignore (read_some flow);
           Eio.Flow.copy_string (ready ^ ready) flow;
           ignore (read_some flow))
         (fun conninfo ->
           connect conninfo
           |> Result.map (fun conn ->
               Eio.Time.Mono.sleep env#mono_clock 0.05;
               rows conn "SELECT 1"))
     with
    | Ok (Error _) -> true
    | _ -> false);
  (* ---- names that cannot be checked against a certificate ---- *)
  refuses "verify-full refuses a host it cannot compare with the certificate"
    (base "a_b.localhost" ^ " sslmode=verify-full sslrootcert=" ^ root);
  check "verify-ca without a root is refused before connecting"
    (match connect (base "127.0.0.1" ^ " sslmode=verify-ca") with
    | Error (Sqlkit.Error.Invalid_argument _) -> true
    | _ -> false);
  connects "require with a root verifies the chain"
    (base "127.0.0.1" ^ " sslmode=require sslrootcert=" ^ root)
    yes;
  (* one handle, two fibers *)
  let shared = Result.get_ok (connect (base "127.0.0.1" ^ " sslmode=require")) in
  let slow = ref None and quick = ref None in
  Eio.Fiber.both
    (fun () -> slow := Some (rows shared "SELECT pg_sleep(0.1)::text"))
    (fun () -> quick := Some (rows shared "SELECT 2::text"));
  check "concurrent use of one handle is refused, not interleaved"
    ((match (!slow, !quick) with
       | Some (Ok _), Some (Error e) -> e.sqlstate = None
       | _ -> false)
    && rows shared "SELECT 3::text" = Ok [ [| Sqlkit.Value.Text "3" |] ]);
  if !failures = 0 then print_endline "all good"
  else (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
