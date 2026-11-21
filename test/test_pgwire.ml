(* The parts of the pure OCaml driver that need no server: message layout,
   SCRAM arithmetic against the RFC's own exchange, connection strings, and a
   decoder that must survive any bytes a server could send. *)

open Pgwire_protocol

let failures = ref 0

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    incr failures)

let built f =
  let b = Buffer.create 64 in
  f b;
  Buffer.contents b

let int32 n =
  let b = Bytes.create 4 in
  Bytes.set_int32_be b 0 (Int32.of_int n);
  Bytes.to_string b

let int16 n =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 n;
  Bytes.to_string b

let frontend () =
  check "ssl request" (Wire.ssl_request = "\000\000\000\008\004\210\022\047");
  check "startup"
    (Wire.startup [ ("user", "u"); ("database", "d") ]
    = int32 27 ^ "\000\003\000\000" ^ "user\000u\000database\000d\000\000");
  check "cancel request"
    (Wire.cancel_request ~pid:7l ~key:"\001\002\003\004"
    = int32 16 ^ "\004\210\022\046" ^ int32 7 ^ "\001\002\003\004");
  check "password" (Wire.password "pw" = "p" ^ int32 7 ^ "pw\000");
  check "sasl initial"
    (Wire.sasl_initial ~mechanism:"M" "abc" = "p" ^ int32 13 ^ "M\000" ^ int32 3 ^ "abc");
  check "parse"
    (built (fun b -> Wire.parse b ~name:"s" ~sql:"SELECT 1")
    = "P" ^ int32 17 ^ "s\000SELECT 1\000" ^ int16 0);
  check "bind with a value and a NULL"
    (built (fun b -> Wire.bind b ~statement:"s" [| Some "ab"; None |])
    = "B"
      ^ int32 (4 + 1 + 2 + 2 + 2 + 6 + 4 + 2)
      ^ "\000s\000" ^ int16 0 ^ int16 2 ^ int32 2 ^ "ab" ^ "\255\255\255\255" ^ int16 0);
  check "execute" (built Wire.execute = "E" ^ int32 9 ^ "\000" ^ int32 0);
  check "sync" (built Wire.sync = "S" ^ int32 4);
  check "query" (built (fun b -> Wire.query b "X") = "Q" ^ int32 6 ^ "X\000");
  check "copy data" (built (fun b -> Wire.copy_data b "r\n") = "d" ^ int32 6 ^ "r\n");
  check "copy done" (built Wire.copy_done = "c" ^ int32 4)

let backend () =
  let ok tag payload expected = Wire.decode tag payload = Ok expected in
  check "auth ok" (ok 'R' (int32 0) (Authentication Auth_ok));
  check "auth md5" (ok 'R' (int32 5 ^ "salt") (Authentication (Md5 "salt")));
  check "auth sasl mechanisms"
    (ok 'R'
       (int32 10 ^ "SCRAM-SHA-256-PLUS\000SCRAM-SHA-256\000\000")
       (Authentication (Sasl [ "SCRAM-SHA-256-PLUS"; "SCRAM-SHA-256" ])));
  check "auth sasl continue"
    (ok 'R' (int32 11 ^ "r=x") (Authentication (Sasl_continue "r=x")));
  check "auth gss is named, not guessed"
    (ok 'R' (int32 7) (Authentication (Unsupported 7)));
  check "key data"
    (ok 'K' (int32 42 ^ "abcd") (Backend_key_data { pid = 42l; key = "abcd" }));
  check "ready: idle" (ok 'Z' "I" (Ready_for_query Idle));
  check "ready: failed transaction" (ok 'Z' "E" (Ready_for_query Failed));
  check "data row with NULL and empty string"
    (ok 'D'
       (int16 3 ^ int32 2 ^ "hi" ^ "\255\255\255\255" ^ int32 0)
       (Data_row [| Text "hi"; Null; Text "" |]));
  check "command complete" (ok 'C' "INSERT 0 3\000" (Command_complete "INSERT 0 3"));
  check "error fields"
    (match
       Wire.decode 'E'
         "SERROR\000C23505\000Mduplicate key\000Dalready \
          there\000nusers_pk\000tusers\000\000"
     with
    | Ok (Error_response f) ->
        f.severity = "ERROR" && f.sqlstate = Some "23505" && f.message = "duplicate key"
        && f.detail = Some "already there"
        && f.constraint_name = Some "users_pk"
        && f.table_name = Some "users" && f.column_name = None && f.hint = None
    | _ -> false);
  let bad tag payload =
    match Wire.decode tag payload with Error _ -> true | Ok _ -> false
  in
  check "truncated auth" (bad 'R' "\000");
  check "truncated key data" (bad 'K' "\000\000");
  check "row that claims more columns than it has" (bad 'D' (int16 2 ^ int32 1 ^ "x"));
  check "column longer than the message" (bad 'D' (int16 1 ^ int32 99 ^ "x"));
  check "unterminated command tag" (bad 'C' "SELECT 1");
  check "unknown transaction status" (bad 'Z' "?");
  check "empty ready" (bad 'Z' "");
  check "unknown message type" (bad '?' "");
  check "unterminated error field" (bad 'E' "Mno end");
  check "affected: insert" (Wire.affected "INSERT 0 3" = Some 3);
  check "affected: update" (Wire.affected "UPDATE 0" = Some 0);
  check "affected: copy" (Wire.affected "COPY 10000" = Some 10000);
  check "affected: set has none" (Wire.affected "SET" = None);
  check "affected: ddl has none" (Wire.affected "CREATE TABLE" = None)

(* No input, however shaped, may raise or loop: a server (or whoever sits
   between us and it) chooses these bytes. *)
let decoder_is_total () =
  let state = Random.State.make [| 0x5ca1ab1e |] in
  let tags = "RKSZ123nIsTtDCENGHWdcAv?" in
  let escaped = ref 0 in
  for _ = 1 to 200_000 do
    let tag = tags.[Random.State.int state (String.length tags)] in
    let payload =
      String.init (Random.State.int state 40) (fun _ ->
          (* weighted toward the bytes that steer the parser *)
          match Random.State.int state 6 with
          | 0 -> '\000'
          | 1 -> '\255'
          | 2 -> Char.chr (Random.State.int state 4)
          | _ -> Char.chr (Random.State.int state 256))
    in
    match Wire.decode tag payload with
    | Ok _ | Error _ -> ()
    | exception _ -> incr escaped
  done;
  check "200k random messages decode or are refused, never raise" (!escaped = 0)

(* RFC 7677 section 3. The RFC's exchange names the user in the first message;
   PostgreSQL leaves it empty, so the proof differs and only the pieces that do
   not depend on it are compared: the derived keys, through the server
   signature check failing and succeeding in the right places. *)
let scram () =
  check "client first" (Scram.client_first ~nonce:"abc" = "n,,n=,r=abc");
  let nonce = "rOprNGfwEbeRWgbNEkqO" in
  let server_first =
    "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096"
  in
  (match Scram.client_final ~password:"pencil" ~nonce ~server_first () with
  | Error why -> check ("client final: " ^ why) false
  | Ok (message, expected) ->
      check "client final echoes the combined nonce and binding"
        (String.starts_with
           ~prefix:"c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p="
           message);
      (* Independent computation of the server signature for this exchange,
         from the RFC's definitions, using only Digestif. *)
      let hmac ~key s = Digestif.SHA256.(to_raw_string (hmac_string ~key s)) in
      let xor a b =
        String.init (String.length a) (fun i ->
            Char.chr (Char.code a.[i] lxor Char.code b.[i]))
      in
      let salt = Result.get_ok (Base64.decode "W22ZaJ0SNY7soEsUEjb6gQ==") in
      let u = ref (hmac ~key:"pencil" (salt ^ "\000\000\000\001")) in
      let salted = ref !u in
      for _ = 2 to 4096 do
        u := hmac ~key:"pencil" !u;
        salted := xor !salted !u
      done;
      let auth =
        "n=,r=" ^ nonce ^ "," ^ server_first
        ^ ",c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"
      in
      let signature =
        Base64.encode_string (hmac ~key:(hmac ~key:!salted "Server Key") auth)
      in
      check "genuine server signature verifies"
        (Scram.verify expected ~server_final:("v=" ^ signature));
      check "wrong server signature is refused"
        (not
           (Scram.verify expected
              ~server_final:"v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4="));
      check "missing server signature is refused"
        (not (Scram.verify expected ~server_final:"e=x")));
  let refused server_first =
    match Scram.client_final ~password:"p" ~nonce:"abc" ~server_first () with
    | Error _ -> true
    | Ok _ -> false
  in
  check "nonce that does not extend ours" (refused "r=xyzdef,s=c2FsdA==,i=4096");
  check "nonce returned unchanged" (refused "r=abc,s=c2FsdA==,i=4096");
  check "zero iterations" (refused "r=abcdef,s=c2FsdA==,i=0");
  check "absurd iterations" (refused "r=abcdef,s=c2FsdA==,i=2000000000");
  check "salt that is not base64" (refused "r=abcdef,s=!!!,i=4096");
  check "missing salt" (refused "r=abcdef,i=4096");
  check "md5 response"
    (* md5(md5("secret" ^ "alice") ^ salt), checked against PostgreSQL's formula *)
    (Scram.md5 ~user:"alice" ~password:"secret" ~salt:"\001\002\003\004"
    = "md5"
      ^ Digestif.MD5.(
          to_hex
            (digest_string (to_hex (digest_string "secretalice") ^ "\001\002\003\004"))))

let conninfo () =
  let parse = Conninfo.parse in
  (match
     parse
       "postgresql://al%40ice:p%3Aw@db.example.com:6543/app%20db?sslmode=verify-full&application_name=web"
   with
  | Ok c ->
      check "uri: host, port, credentials, database"
        (c.host = "db.example.com" && c.port = 6543 && c.user = "al@ice"
       && c.password = Some "p:w" && c.database = "app db");
      check "uri: query parameters"
        (c.sslmode = Verify_full && c.application_name = Some "web")
  | Error why -> check ("uri: " ^ why) false);
  (match
     parse
       "host=/var/run/postgresql user=bob dbname='my db' password='it\\'s' \
        sslmode=disable"
   with
  | Ok c ->
      check "keywords: socket directory, quoting, escapes"
        (c.host = "/var/run/postgresql"
        && c.user = "bob" && c.database = "my db" && c.password = Some "it's"
        && c.sslmode = Disable && c.port = 5432)
  | Error why -> check ("keywords: " ^ why) false);
  (match parse "postgresql://carol@h/" with
  | Ok c ->
      check "database defaults to the user" (c.database = "carol" && c.sslmode = Prefer)
  | Error why -> check ("defaults: " ^ why) false);
  let refused s = match parse s with Error _ -> true | Ok _ -> false in
  check "unknown parameter is an error, not ignored"
    (refused "postgresql://u@h/d?sslcert=x");
  check "channel binding cannot be silently dropped"
    (refused "postgresql://u@h/d?channel_binding=require");
  check "several hosts" (refused "host=a,b user=u");
  check "bad port" (refused "host=a port=99999 user=u");
  check "bad sslmode" (refused "host=a user=u sslmode=maybe");
  check "unterminated quote" (refused "host=a user='u")

(* Regressions from the security review. *)
let hardening () =
  let raises f = match f () with exception Invalid_argument _ -> true | _ -> false in
  check "a length that does not fit its field is never written"
    (raises (fun () ->
         let b = Buffer.create 16 in
         Wire.bind b ~statement:"s" (Array.make 70_000 None)));
  check "statement size counts every parameter"
    (Wire.statement_size ~sql:"SELECT 1" [| Some "abcd"; None |] >= 8 + 8 + 4);
  let parse = Conninfo.parse in
  let refused s = match parse s with Error _ -> true | Ok _ -> false in
  (match parse "postgresql://u:a%2Cb@h/d?options=-c%20x%3D1,y" with
  | Ok c ->
      check "commas in values survive"
        (c.password = Some "a,b" && c.options = Some "-c x=1,y")
  | Error why -> check ("commas: " ^ why) false);
  check "sslmode with trailing junk is not read as the default"
    (refused "postgresql://u@h/d?sslmode=verify-full,x");
  check "second @ is refused, not guessed at" (refused "postgresql://u:SEC@RET@h/db");
  check "fragment is refused" (refused "postgresql://u:SECRET#PW@h/db");
  check "non-numeric port is refused" (refused "postgresql://u@h:54a32/db");
  check "percent-encoded NUL in the database is refused"
    (refused "postgresql://u@h/db%00application_name%00x");
  check "percent-encoded NUL in the password is refused"
    (refused "postgresql://u:p%00q@h/db");
  check "repeated key is refused" (refused "host=a host=b user=u");
  check "verify-ca without a root is refused" (refused "host=a user=u sslmode=verify-ca");
  (match parse "host=a user=u sslmode=require sslrootcert=/tmp/ca.pem" with
  | Ok c -> check "require with a root verifies the chain" (c.sslmode = Verify_ca)
  | Error why -> check ("require+root: " ^ why) false);
  (match parse "host=a user=u sslmode=require sslrootcert=system" with
  | Ok c ->
      check "sslrootcert=system means verify-full"
        (c.sslmode = Verify_full && c.sslrootcert = None)
  | Error why -> check ("system root: " ^ why) false);
  (match parse "host=a user=u require_auth=scram-sha-256" with
  | Ok c -> check "require_auth: one method" (c.require_auth = Some [ Scram_sha_256 ])
  | Error why -> check ("require_auth: " ^ why) false);
  (match parse "postgresql://u@h/d?require_auth=!password,!md5" with
  | Ok c ->
      check "require_auth: negations"
        (c.require_auth = Some [ Scram_sha_256; No_authentication ])
  | Error why -> check ("require_auth negated: " ^ why) false);
  check "require_auth: mixed forms refused"
    (refused "host=a user=u require_auth=md5,!password");
  check "require_auth: unknown method refused" (refused "host=a user=u require_auth=gss");
  (match parse "host=a user=u connect_timeout=3" with
  | Ok c -> check "connect_timeout is kept" (c.connect_timeout = Some 3.)
  | Error why -> check ("connect_timeout: " ^ why) false);
  let challenge i = Printf.sprintf "r=abcdef,s=c2FsdA==,i=%s" i in
  let scram_refused server_first =
    match Scram.client_final ~password:"p" ~nonce:"abc" ~server_first () with
    | Error _ -> true
    | Ok _ -> false
  in
  check "iteration count above the cap is refused"
    (scram_refused (challenge (string_of_int (Scram.max_iterations + 1))));
  check "hexadecimal iteration count is refused" (scram_refused (challenge "0x1000"));
  check "mandatory extension is refused" (scram_refused ("m=x," ^ challenge "4096"));
  check "empty salt is refused" (scram_refused "r=abcdef,s=,i=4096");
  let pauses = ref 0 in
  ignore
    (Scram.client_final
       ~pause:(fun () -> incr pauses)
       ~password:"p" ~nonce:"abc" ~server_first:(challenge "20000") ());
  check "key derivation pauses as it goes" (!pauses >= 5)

let () =
  frontend ();
  hardening ();
  backend ();
  decoder_is_total ();
  scram ();
  conninfo ();
  if !failures = 0 then print_endline "all good"
  else (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
