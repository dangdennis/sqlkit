(* PostgreSQL driver in OCaml, over Eio. No libpq.

   The shape follows sqlkit-eio so the two can be held to the same checks: one
   statement is Parse (first use only) + Bind + Execute + Sync in a single
   write, session setup is a single round trip, and a statement that is
   cancelled or runs past its deadline costs the connection after the server
   has been asked to stop.

   Everything moves in text format. *)

open Pgwire_protocol

type clock = Eio.Time.Mono.ty Eio.Resource.t
type flow = Eio.Flow.two_way_ty Eio.Resource.t

let client_diag message =
  {
    Wire.severity = "ERROR";
    message;
    sqlstate = None;
    detail = None;
    hint = None;
    constraint_name = None;
    table_name = None;
    column_name = None;
  }

module Raw = struct
  type conn = {
    flow : flow;
    input : Eio.Buf_read.t;
    out : Buffer.t;
    shutdown : unit -> unit;
    readable : unit -> [ `Yes | `No | `Unknown ];
    cancel : unit -> unit;
    clock : clock;
    query_timeout : float;
    max_result_bytes : int;
    mutable received : int;
    mutable busy : bool;
    stmts : (string, string) Hashtbl.t;
    statement_cache : bool;
    mutable generation : int;
    mutable last_tag : string;
    mutable transaction : Wire.transaction;
    mutable broken : bool;
    mutable closed : bool;
  }

  (* The connection can no longer be trusted. Never escapes this module. *)
  exception Io of string

  let now c = Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now c.clock)) /. 1e9
  let is_cancellation = function Eio.Cancel.Cancelled _ -> true | _ -> false

  let with_cleanup ~finally f =
    Fun.protect ~finally:(fun () -> Eio.Cancel.protect finally) f

  let close c =
    if not c.closed then (
      c.closed <- true;
      c.shutdown ())

  (* Socket and TLS failures arrive as exceptions of several kinds. Whatever
     they are, they mean the same thing here. *)
  let io f =
    match f () with
    | v -> v
    | exception ((Out_of_memory | Stack_overflow | Sys.Break | Io _) as e) -> raise e
    | exception e when is_cancellation e -> raise e
    | exception End_of_file -> raise (Io "server closed the connection unexpectedly")
    | exception e -> raise (Io (Printexc.to_string e))

  (* The length is checked before a byte of the payload is asked for, so a
     server cannot make the client reserve memory by announcing a size. Until
     the session is established the limit is small: nothing a server
     legitimately says before then is long. *)
  let read ?(limit = Wire.max_message) flow_input =
    io (fun () ->
        let tag = Eio.Buf_read.any_char flow_input in
        let len = Int32.to_int (Eio.Buf_read.BE.uint32 flow_input) land 0xFFFFFFFF in
        if len < 4 || len - 4 > limit then
          raise (Io "server sent a message of unacceptable length");
        match Wire.decode tag (Eio.Buf_read.take (len - 4) flow_input) with
        | Ok m -> (m, len)
        | Error why -> raise (Io why))

  (* Every message counts against the statement's budget, including the ones
     that carry no reply: a stream of notices is as good a way to fill memory
     as a stream of rows. *)
  let rec next c =
    let m, len = read c.input in
    c.received <- c.received + len;
    if c.received > c.max_result_bytes then
      raise (Io "reply exceeds the configured max_result_bytes");
    match m with
    | Wire.Notice_response _ | Wire.Parameter_status _ | Wire.Notification -> next c
    | m -> m

  (* Start of a request. Idle and in step, nothing can be waiting to be read;
     if something is, the replies would be attributed one statement late. *)
  let begin_request c =
    if Eio.Buf_read.buffered_bytes c.input > 0 then
      raise (Io "unexpected data from the server between statements");
    c.received <- 0

  let write c =
    let data = Buffer.contents c.out in
    Buffer.clear c.out;
    io (fun () -> Eio.Flow.copy_string data c.flow)

  type reply = Rows of Sqlkit.Value.t array list * int option | Refused of Wire.fields

  (* Everything up to ReadyForQuery. After an error the server skips to the
     Sync on its own, so there is nothing to resynchronise. A FATAL error is
     followed by the end of the stream instead; the error already read is the
     better diagnosis, so it is kept and the connection marked broken. *)
  let collect c =
    let rec go ~parsed ~rows ~tag ~error =
      match next c with
      | exception Io _ when error <> None ->
          c.broken <- true;
          (parsed, rows, tag, error)
      | Wire.Ready_for_query t ->
          c.transaction <- t;
          (parsed, rows, tag, error)
      | Wire.Parse_complete -> go ~parsed:true ~rows ~tag ~error
      | Wire.Data_row row -> go ~parsed ~rows:(row :: rows) ~tag ~error
      | Wire.Command_complete t -> go ~parsed ~rows ~tag:(Some t) ~error
      | Wire.Error_response f ->
          go ~parsed ~rows ~tag ~error:(if error = None then Some f else error)
      | Wire.Copy_in_response ->
          (* COPY FROM STDIN run as an ordinary statement. Refuse it; the
             server ignored the Sync already sent, so send another. *)
          Wire.copy_fail c.out "COPY FROM STDIN needs a :copy query";
          Wire.sync c.out;
          write c;
          go ~parsed ~rows ~tag ~error
      | Wire.Bind_complete | Wire.Close_complete | Wire.No_data | Wire.Empty_query
      | Wire.Portal_suspended | Wire.Row_description | Wire.Parameter_description
      | Wire.Copy_out_response | Wire.Copy_both_response | Wire.Copy_data | Wire.Copy_done
      | Wire.Negotiate_protocol_version | Wire.Notice_response _ | Wire.Parameter_status _
      | Wire.Notification ->
          go ~parsed ~rows ~tag ~error
      | Wire.Authentication _ | Wire.Backend_key_data _ ->
          raise (Io "unexpected authentication message")
    in
    go ~parsed:false ~rows:[] ~tag:None ~error:None

  let reply c (_, rows, tag, error) =
    c.last_tag <- Option.value tag ~default:"";
    match error with
    | Some f -> Refused f
    | None -> Rows (List.rev rows, Option.bind tag Wire.affected)

  (* Returns the statement's reply and whether a cached plan produced it. *)
  (* The first keyword of a statement, lower case. *)
  let first_word sql =
    let n = String.length sql in
    let rec skip i =
      if i < n && (sql.[i] = ' ' || sql.[i] = '\n' || sql.[i] = '\t' || sql.[i] = '\r')
      then skip (i + 1)
      else i
    in
    let start = skip 0 in
    let rec stop i =
      if
        i < n && ((sql.[i] >= 'a' && sql.[i] <= 'z') || (sql.[i] >= 'A' && sql.[i] <= 'Z'))
      then stop (i + 1)
      else i
    in
    String.lowercase_ascii (String.sub sql start (stop start - start))

  (* Only statements with a plan worth keeping are prepared by name. Above
     all, transaction control never is: a COMMIT whose cached name has gone
     missing would fail, abort the transaction, be retried, and then succeed
     as a rollback. *)
  let plans sql =
    match first_word sql with
    | "select" | "insert" | "update" | "delete" | "with" | "merge" | "values" | "table" ->
        true
    | _ -> false

  (* PostgreSQL answers COMMIT in a failed transaction with success and the
     tag ROLLBACK. To a caller that is an acknowledged commit that never
     happened, so it is turned into the error it is. *)
  let rolled_back ~sql ~tag =
    tag = "ROLLBACK" && match first_word sql with "commit" | "end" -> true | _ -> false

  let commit_refused = "COMMIT rolled back: the transaction had already failed"

  (* A statement's name is a digest of its text, not a counter. Behind a
     pooler in transaction mode one backend serves many clients; with counters
     two clients would both call their first statement "s1", and the second
     would execute the first one's SQL and get its rows back with no error.
     With a digest, a name that already exists is the same statement. The
     generation changes the names after a plan has gone stale, so the
     replacement does not collide with what it replaces. *)
  let statement_name c sql =
    Printf.sprintf "sqlkit_%s%s"
      (Digest.to_hex (Digest.string sql))
      (if c.generation = 0 then "" else "_" ^ string_of_int c.generation)

  (* Returns the statement's reply and whether a cached plan produced it. *)
  let rec attempt c ~sql ~args =
    begin_request c;
    if not (c.statement_cache && plans sql) then begin
      (* The unnamed statement, parsed every time. Still one write and one
         round trip, and nothing is left behind on the backend. *)
      Wire.parse c.out ~name:"" ~sql;
      Wire.bind c.out ~statement:"" args;
      Wire.execute c.out;
      Wire.sync c.out;
      write c;
      (reply c (collect c), false)
    end
    else
      match Hashtbl.find_opt c.stmts sql with
      | Some name ->
          Wire.bind c.out ~statement:name args;
          Wire.execute c.out;
          Wire.sync c.out;
          write c;
          (reply c (collect c), true)
      | None -> (
          let name = statement_name c sql in
          Wire.parse c.out ~name ~sql;
          Wire.bind c.out ~statement:name args;
          Wire.execute c.out;
          Wire.sync c.out;
          write c;
          let ((parsed, _, _, _) as result) = collect c in
          if parsed then Hashtbl.replace c.stmts sql name;
          match (reply c result, parsed) with
          | (Refused { Wire.sqlstate = Some "42P05"; _ } as exists), false -> (
              (* The name is taken on this backend. Names are derived from the
                 SQL, so it is this statement, prepared by an earlier client
                 of a pooler that hands backends around. Use it. Inside a
                 transaction the refusal has already aborted it, and that is
                 what gets reported. *)
              Hashtbl.replace c.stmts sql name;
              match attempt c ~sql ~args with
              | Refused { Wire.sqlstate = Some "25P02"; _ }, _ -> (exists, false)
              | answer -> answer)
          | r, _ -> (r, false))

  (* See sqlkit-eio for why a stale plan is retried even inside a transaction:
     the retry is refused with 25P02 there, except for the ROLLBACK or COMMIT
     that has to get through. *)
  let dispatch c ~sql ~args =
    (* Behind a pooler without prepared-statement support each attempt can
       land on a different backend, one that lacks the statement or already
       has it, so one retry is not always enough. The bound keeps a server
       that always answers "stale" from holding the caller forever. *)
    let rec go ~first ~tries =
      match attempt c ~sql ~args with
      | (Refused { Wire.sqlstate = Some (("26000" | "0A000") as code); _ } as stale), true
        when tries > 0 ->
          Hashtbl.remove c.stmts sql;
          if code = "0A000" then c.generation <- c.generation + 1;
          go ~first:(Some (Option.value first ~default:stale)) ~tries:(tries - 1)
      | Refused { Wire.sqlstate = Some "25P02"; _ }, _ when first <> None ->
          Option.get first
      | r, _ -> r
    in
    go ~first:None ~tries:4

  let fatal c (f : Wire.fields) =
    c.broken
    ||
    match f.sqlstate with
    | Some code ->
        String.starts_with ~prefix:"08" code
        || code = "57P01" || code = "57P02" || code = "57P03"
    | None -> false

  (* The primary line in libpq's wording. libpq's own message goes on to repeat
     the detail and hint; here those stay in their fields only. *)
  let of_fields (f : Wire.fields) =
    Sqlkit.Driver.error
      (if f.sqlstate = None then f.message else f.severity ^ ":  " ^ f.message)
      ?sqlstate:(Option.map Sqlkit.Sqlstate.of_string f.sqlstate)
      ?detail:f.detail ?hint:f.hint ?constraint_name:f.constraint_name
      ?table_name:f.table_name ?column_name:f.column_name

  let guard c f =
    if c.closed then Error (Sqlkit.Driver.error "connection discarded")
    else if c.busy then
      (* Two fibers on one handle would read each other's replies. *)
      Error (Sqlkit.Driver.error "connection is in use by another fiber")
    else
      let f () =
        c.busy <- true;
        Fun.protect ~finally:(fun () -> c.busy <- false) f
      in
      let run () =
        match f () with
        | Ok _ as r -> r
        | Error d ->
            if fatal c d then close c;
            Error (of_fields d)
        | exception Io message ->
            close c;
            Error (Sqlkit.Driver.error message)
        | exception e ->
            Eio.Cancel.protect c.cancel;
            close c;
            raise e
      in
      match
        Eio.Time.Timeout.run (Eio.Time.Timeout.seconds c.clock c.query_timeout) (fun () ->
            Ok (run ()))
      with
      | Ok r -> r
      | Error `Timeout ->
          Error
            (Sqlkit.Driver.error ~timeout_seconds:c.query_timeout
               "query deadline exceeded")

  let has_nul s = String.contains s '\000'

  let statement c ~sql ~params =
    guard c (fun () ->
        let args = Array.of_list (List.map Sqlkit.Value.to_pg_text params) in
        if
          has_nul sql
          || Array.exists (function Some s -> has_nul s | None -> false) args
        then Error (client_diag "SQL or parameter contains a NUL byte")
        else if
          Array.length args > Wire.max_parameters
          || Wire.statement_size ~sql args > Wire.max_frontend_message
        then Error (client_diag "statement is larger than PostgreSQL accepts")
        else
          match dispatch c ~sql ~args with
          | Rows _ when rolled_back ~sql ~tag:c.last_tag ->
              Error { (client_diag commit_refused) with sqlstate = Some "25P02" }
          | Rows (rows, affected) -> Ok (rows, affected)
          | Refused f -> Error f)

  let query c ~sql ~params ~columns:_ = Result.map fst (statement c ~sql ~params)
  let exec c ~sql ~params = Result.map snd (statement c ~sql ~params)
  let copy_chunk = 64 * 1024

  (* COPY runs over the simple protocol. The server answers the statement with
     CopyInResponse, takes the data, and reports the row count or the error
     once it has seen CopyDone. *)
  let copy c ~sql ~rows =
    guard c (fun () ->
        if has_nul sql then Error (client_diag "SQL contains a NUL byte")
        else if
          String.length sql > Wire.max_frontend_message - 64
          || List.exists
               (fun line ->
                 String.length line > Wire.max_frontend_message - copy_chunk - 64)
               rows
        then Error (client_diag "COPY statement or row is larger than PostgreSQL accepts")
        else begin
          begin_request c;
          Wire.query c.out sql;
          write c;
          let rec finish ~count ~error =
            match next c with
            | exception Io _ when error <> None ->
                c.broken <- true;
                (count, error)
            | Wire.Ready_for_query t ->
                c.transaction <- t;
                (count, error)
            | Wire.Command_complete tag -> finish ~count:(Wire.affected tag) ~error
            | Wire.Error_response f ->
                finish ~count ~error:(if error = None then Some f else error)
            | _ -> finish ~count ~error
          in
          let rec await ~error =
            match next c with
            | Wire.Copy_in_response -> `Streaming
            | Wire.Ready_for_query t ->
                c.transaction <- t;
                `Finished error
            | Wire.Error_response f ->
                await ~error:(if error = None then Some f else error)
            | _ -> await ~error
          in
          match await ~error:None with
          | `Finished (Some f) -> Error f
          | `Finished None -> Error (client_diag "statement did not enter COPY mode")
          | `Streaming -> (
              let pending = Buffer.create copy_chunk in
              let flush () =
                if Buffer.length pending > 0 then begin
                  Wire.copy_data c.out (Buffer.contents pending);
                  Buffer.clear pending;
                  write c
                end
              in
              List.iter
                (fun line ->
                  Buffer.add_string pending line;
                  Buffer.add_char pending '\n';
                  if Buffer.length pending >= copy_chunk then flush ())
                rows;
              flush ();
              Wire.copy_done c.out;
              write c;
              match finish ~count:None ~error:None with
              | _, Some f -> Error f
              | Some n, None -> Ok n
              | None, None -> raise (Io "COPY finished without a row count"))
        end)

  (* A Sync on its own is answered with ReadyForQuery and nothing else. *)
  let ping c =
    match
      begin_request c;
      Wire.sync c.out;
      write c;
      collect c
    with
    | _, _, _, None -> c.transaction = Wire.Idle
    | _, _, _, Some _ -> false
    | exception Io _ -> false
    | exception e ->
        close c;
        raise e

  (* Idle and in step, the server has nothing to say, so a readable socket
     means it closed the connection or sent something we would have to read to
     understand; either way a fresh connection is cheaper than guessing. When
     the socket cannot be examined without a round trip, the round trip is
     made. A handle returned with a transaction open is not reused. *)
  let healthy c =
    (not c.closed) && (not c.broken) && (not c.busy) && c.transaction = Wire.Idle
    && Eio.Buf_read.buffered_bytes c.input = 0
    && match c.readable () with `No -> true | `Yes -> false | `Unknown -> ping c

  let setup c statements =
    begin_request c;
    List.iter
      (fun sql ->
        Wire.parse c.out ~name:"" ~sql;
        Wire.bind c.out ~statement:"" [||];
        Wire.execute c.out)
      statements;
    Wire.sync c.out;
    write c;
    match collect c with _, _, _, error -> error
end

(* ---------- connecting ---------- *)

(* TLS and the SCRAM nonce both draw on it. Installed on first use, once. *)
let rng = lazy (Mirage_crypto_rng_unix.use_default ())

let authenticator (info : Conninfo.t) =
  let lenient ?ip:_ ~host:_ _ = Ok None in
  let trusted () =
    match info.sslrootcert with
    | None -> Ca_certs.authenticator ()
    | Some path -> (
        match
          X509.Certificate.decode_pem_multiple
            (In_channel.with_open_bin path In_channel.input_all)
        with
        | Ok roots ->
            Ok
              (X509.Authenticator.chain_of_trust
                 ~time:(fun () -> Some (Ptime_clock.now ()))
                 roots)
        | Error _ as e -> e
        | exception Sys_error why -> Error (`Msg why))
  in
  match info.sslmode with
  | Disable | Prefer | Require -> Ok lenient
  | Verify_full -> trusted ()
  | Verify_ca ->
      (* Conninfo guarantees a root file here: checking a chain against every
         public authority while ignoring the name would accept anybody. *)
      Result.map
        (fun (verify : X509.Authenticator.t) ?ip:_ ~host:_ chain ->
          verify ?ip:None ~host:None chain)
        (trusted ())

let secure (info : Conninfo.t) socket =
  Lazy.force rng;
  let ip = Result.to_option (Ipaddr.of_string info.host) in
  let host =
    match ip with
    | Some _ -> None
    | None ->
        Result.to_option (Result.bind (Domain_name.of_string info.host) Domain_name.host)
  in
  if info.sslmode = Verify_full && ip = None && host = None then
    (* Without a name to compare, only the chain would be checked, which is
       not what verify-full promises. *)
    Error
      (Printf.sprintf
         "TLS: %S cannot be verified against a certificate (not a host name or address)"
         info.host)
  else
    match authenticator info with
    | Error (`Msg why) -> Error ("TLS: " ^ why)
    | Ok authenticator -> (
        match Tls.Config.client ~authenticator ?ip () with
        | Error (`Msg why) -> Error ("TLS: " ^ why)
        | Ok config -> Ok ((Tls_eio.client_of_flow config ?host socket :> flow), true))

(* SSLRequest is answered with a single byte, read before any buffering so that
   the TLS handshake sees the stream from its first byte. *)
let negotiate (info : Conninfo.t) ~unix socket =
  let plain = ((socket :> flow), false) in
  if unix || info.sslmode = Disable then Ok plain
  else begin
    Eio.Flow.copy_string Wire.ssl_request socket;
    let answer = Cstruct.create 1 in
    Eio.Flow.read_exact socket answer;
    match (Cstruct.get_char answer 0, info.sslmode) with
    | 'S', _ -> secure info socket
    | 'N', Prefer -> Ok plain
    | 'N', _ -> Error "the server does not support TLS"
    | _ -> Error "unexpected reply to the TLS request"
  end

(* The server proposes the method; the client decides whether to go along.
   Each connection gets one exchange, in one method, and a method is used only
   if the policy allows it. Otherwise whoever answers for the server could ask
   for the password in clear after, or instead of, the SCRAM exchange that was
   meant to keep it from them.

   Without [require_auth], everything is allowed except sending the password
   itself over a connection that is neither encrypted nor a local socket.
   AuthenticationOk with no exchange at all is allowed too, as in libpq: that
   is trust authentication, and it means the server has proved nothing. *)
let authenticate (info : Conninfo.t) ~confidential flow input =
  let send message = Eio.Flow.copy_string message flow in
  let allowed (m : Conninfo.auth) =
    match info.require_auth with
    | Some methods -> List.mem m methods
    | None -> ( match m with Password -> confidential | _ -> true)
  in
  let refused what =
    Error
      (Printf.sprintf "the server asked for %s, which this connection does not allow" what)
  in
  let need_password k =
    match info.password with
    | Some p -> k p
    | None -> Error "the server asked for a password and none was given"
  in
  let rec go stage =
    match fst (Raw.read ~limit:Wire.max_startup_message input) with
    | Wire.Authentication Auth_ok -> (
        match stage with
        | `Start ->
            if allowed No_authentication then Ok () else refused "no authentication"
        | `Verified | `Answered -> Ok ()
        | `Sent _ | `Proving _ -> Error "the server ended SCRAM without proving itself")
    | Wire.Authentication Cleartext -> (
        match stage with
        | `Start when allowed Password ->
            need_password (fun p ->
                send (Wire.password p);
                go `Answered)
        | `Start -> refused "the password in clear"
        | _ -> Error "the server asked to authenticate twice")
    | Wire.Authentication (Md5 salt) -> (
        match stage with
        | `Start when allowed Md5 ->
            need_password (fun p ->
                send (Wire.password (Scram.md5 ~user:info.user ~password:p ~salt));
                go `Answered)
        | `Start -> refused "an MD5 password"
        | _ -> Error "the server asked to authenticate twice")
    | Wire.Authentication (Sasl mechanisms) -> (
        match stage with
        | `Start when not (allowed Scram_sha_256) -> refused "SCRAM-SHA-256"
        | `Start when not (List.mem Scram.mechanism mechanisms) ->
            Error "the server offers no supported SASL mechanism"
        | `Start ->
            need_password (fun _ ->
                Lazy.force rng;
                let nonce = Base64.encode_string (Mirage_crypto_rng.generate 18) in
                send
                  (Wire.sasl_initial ~mechanism:Scram.mechanism
                     (Scram.client_first ~nonce));
                go (`Sent nonce))
        | _ -> Error "the server asked to authenticate twice")
    | Wire.Authentication (Sasl_continue server_first) -> (
        match (stage, info.password) with
        | `Sent nonce, Some password -> (
            (* The derivation yields as it goes, so the connect deadline can
               end it and other fibers keep running. *)
            match
              Scram.client_final ~pause:Eio.Fiber.yield ~password ~nonce ~server_first ()
            with
            | Error _ as e -> e
            | Ok (message, expected) ->
                send (Wire.sasl_response message);
                go (`Proving expected))
        | _ -> Error "unexpected SASL challenge")
    | Wire.Authentication (Sasl_final server_final) -> (
        match stage with
        | `Proving expected when Scram.verify expected ~server_final -> go `Verified
        | _ -> Error "the server failed to prove it knows the password")
    | Wire.Authentication (Unsupported code) ->
        Error (Printf.sprintf "unsupported authentication method (code %d)" code)
    | Wire.Error_response f -> Error (f.severity ^ ":  " ^ f.message)
    | Wire.Notice_response _ | Wire.Negotiate_protocol_version -> go stage
    | _ -> Error "unexpected message during authentication"
  in
  go `Start

let startup_parameters (info : Conninfo.t) =
  [ ("user", info.user); ("database", info.database); ("client_encoding", "UTF8") ]
  @ (match info.application_name with
    | Some n -> [ ("application_name", n) ]
    | None -> [])
  @ match info.options with Some o -> [ ("options", o) ] | None -> []

let addresses ~net (info : Conninfo.t) =
  if String.starts_with ~prefix:"/" info.host then
    [ (`Unix (Printf.sprintf "%s/.s.PGSQL.%d" info.host info.port), true) ]
  else
    List.map
      (fun addr -> (addr, false))
      (Eio.Net.getaddrinfo_stream ~service:(string_of_int info.port) net info.host)

(* The cancel request travels on its own short-lived connection, as the
   protocol requires, protected the same way as the main one: the key it
   carries is what lets someone stop this session's statements. It is bounded:
   a server that has gone away must not hold up the caller. *)
let cancel_request ~net ~clock (info : Conninfo.t) ~unix addr ~pid ~key () =
  match
    Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock 2.) (fun () ->
        Eio.Switch.run (fun sw ->
            let socket = Eio.Net.connect ~sw net addr in
            match negotiate info ~unix socket with
            | Error _ -> Ok ()
            | Ok (flow, _) ->
                Eio.Flow.copy_string (Wire.cancel_request ~pid ~key) flow;
                (* the server closes once it has acted on the request *)
                let sink = Cstruct.create 1 in
                (try ignore (Eio.Flow.single_read flow sink : int)
                 with End_of_file -> ());
                Ok ()))
  with
  | Ok () | Error `Timeout -> ()
  | exception ((Out_of_memory | Stack_overflow | Sys.Break) as e) -> raise e
  | exception _ -> ()

(* select cannot look at descriptors past its set size; then the answer is
   unknown, not "nothing there". *)
let readable socket () =
  match Eio_unix.Resource.fd_opt socket with
  | None -> `Unknown
  | Some fd ->
      Eio_unix.Fd.use fd
        ~if_closed:(fun () -> `Yes)
        (fun fd ->
          match Unix.select [ fd ] [] [] 0. with
          | [], _, _ -> `No
          | _ -> `Yes
          | exception Unix.Unix_error _ -> `Unknown)

(* A switch that belongs to one physical connection. Eio attaches a socket to
   the switch it is opened under before it connects, so a connect that fails
   leaves its descriptor there until that switch ends. Under the pool's switch
   that is the life of the process: one leaked descriptor per refused
   connection, and an outage of a few seconds exhausts the limit. Each attempt
   therefore gets a switch of its own, released when the attempt fails or the
   connection closes. The daemon holds it open in between and goes away with
   the caller's switch. *)
let scoped ~sw =
  let ready, set_ready = Eio.Promise.create () in
  let stop, set_stop = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run (fun inner ->
          Eio.Promise.resolve set_ready inner;
          Eio.Promise.await stop);
      `Stop_daemon);
  ( Eio.Promise.await ready,
    fun () -> if not (Eio.Promise.is_resolved stop) then Eio.Promise.resolve set_stop ()
  )

let establish ~sw ~net ~clock ~query_timeout ~max_result_bytes ~statement_cache
    ~statements (info : Conninfo.t) =
  let attempt (addr, unix) =
    let owner, release = scoped ~sw in
    let socket =
      match Eio.Net.connect ~sw:owner net addr with
      | socket -> socket
      | exception e ->
          release ();
          raise e
    in
    let ready = ref false in
    let shutdown () =
      (try Eio.Resource.close socket with Invalid_argument _ | Eio.Io _ -> ());
      release ()
    in
    Raw.with_cleanup
      ~finally:(fun () -> if not !ready then shutdown ())
      (fun () ->
        let ( let* ) = Result.bind in
        match
          Raw.io (fun () ->
              let* flow, encrypted = negotiate info ~unix socket in
              let input = Eio.Buf_read.of_flow ~max_size:(Wire.max_message + 5) flow in
              Eio.Flow.copy_string (Wire.startup (startup_parameters info)) flow;
              let* () = authenticate info ~confidential:(encrypted || unix) flow input in
              let rec greet key =
                match fst (Raw.read ~limit:Wire.max_startup_message input) with
                | Wire.Backend_key_data { pid; key } -> greet (Some (pid, key))
                | Wire.Ready_for_query _ -> Ok key
                | Wire.Error_response f -> Error (f.severity ^ ":  " ^ f.message)
                | _ -> greet key
              in
              let* key = greet None in
              let cancel =
                match key with
                | Some (pid, key) -> cancel_request ~net ~clock info ~unix addr ~pid ~key
                | None -> ignore
              in
              let c =
                {
                  Raw.flow;
                  input;
                  out = Buffer.create 1024;
                  shutdown;
                  readable = readable socket;
                  cancel;
                  clock;
                  query_timeout;
                  max_result_bytes;
                  received = 0;
                  busy = false;
                  stmts = Hashtbl.create 16;
                  statement_cache;
                  generation = 0;
                  last_tag = "";
                  transaction = Wire.Idle;
                  broken = false;
                  closed = false;
                }
              in
              match Raw.setup c statements with
              | None -> Ok c
              | Some f -> Error (f.severity ^ ":  " ^ f.message))
        with
        | Ok c ->
            ready := true;
            Ok c
        | Error message | (exception Raw.Io message) -> Error message)
  in
  (* Each address a name resolves to is tried in turn; the last failure is the
     one reported. *)
  let rec first last = function
    | [] -> Error (Sqlkit.Error.Connect last)
    | target :: rest -> (
        match attempt target with
        | Ok _ as ok -> ok
        | Error message -> first message rest
        | exception (Eio.Io _ as e) -> first (Printexc.to_string e) rest)
  in
  match addresses ~net info with
  | [] ->
      Error (Sqlkit.Error.Connect (Printf.sprintf "could not resolve host %S" info.host))
  | targets -> first "" targets
  | exception (Eio.Io _ as e) -> Error (Sqlkit.Error.Connect (Printexc.to_string e))

let conninfo_of_env = Conninfo.of_env
let boundary = Sqlkit_pool_eio.boundary
let default_max_result_bytes = 256 * 1024 * 1024

let validate ~limits ~connect_timeout ~query_timeout ~max_result_bytes conninfo =
  let invalid message =
    Error (Sqlkit.Error.Invalid_argument { operation = "connect"; message })
  in
  match Sqlkit.Limits.validate limits with
  | Error message -> invalid message
  | Ok () -> (
      if
        not
          (List.for_all
             (fun n -> Float.is_finite n && n > 0.)
             [ connect_timeout; query_timeout ])
      then invalid "client timeouts must be finite and positive"
      else if max_result_bytes < 1 then invalid "max_result_bytes must be positive"
      else if String.contains conninfo '\000' then invalid "NUL in connection string"
      else
        match Conninfo.parse conninfo with Ok info -> Ok info | Error why -> invalid why)

(* A connect_timeout in the connection string can shorten the deadline, never
   extend it past what the caller asked for. *)
let open_raw ~sw ~net ~clock ~limits ~connect_timeout ~query_timeout ~max_result_bytes
    ~statement_cache (info : Conninfo.t) =
  let connect_timeout =
    match info.connect_timeout with
    | Some seconds -> Float.min seconds connect_timeout
    | None -> connect_timeout
  in
  boundary ~operation:"connect" (fun () ->
      let statements = Sqlkit.Driver.session_setup @ Sqlkit.Limits.statements limits in
      match
        Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock connect_timeout) (fun () ->
            Ok
              (establish ~sw ~net ~clock ~query_timeout ~max_result_bytes ~statement_cache
                 ~statements info))
      with
      | Ok r -> r
      | Error `Timeout ->
          Error
            (Sqlkit.Error.Timeout
               { operation = "connect"; phase = Connect; seconds = connect_timeout }))

let connect ~sw ~net ~clock ?(limits = Sqlkit.Limits.default) ?(connect_timeout = 10.)
    ?(query_timeout = 35.) ?(max_result_bytes = default_max_result_bytes)
    ?(statement_cache = true) ?observe conninfo =
  let clock = (clock :> clock) in
  Result.bind
    (validate ~limits ~connect_timeout ~query_timeout ~max_result_bytes conninfo)
    (fun info ->
      Result.map
        (fun c ->
          let conn = Sqlkit.Driver.make ?observe (module Raw) c in
          Eio.Switch.on_release sw (fun () ->
              Raw.close c;
              Sqlkit.Driver.release conn);
          conn)
        (open_raw ~sw ~net ~clock ~limits ~connect_timeout ~query_timeout
           ~max_result_bytes ~statement_cache info))

module Pool = struct
  include Sqlkit_pool_eio.Make (Raw) (Raw)

  let create ~sw ~net ~clock ?(max_size = 10) ?(acquire_timeout = 5.)
      ?(connect_timeout = 10.) ?(query_timeout = 35.)
      ?(max_result_bytes = default_max_result_bytes) ?(statement_cache = true)
      ?(limits = Sqlkit.Limits.default) ?(observe = Sqlkit.Telemetry.ignore) conninfo =
    let mono = (clock :> clock) in
    Result.bind
      (validate ~limits ~connect_timeout ~query_timeout ~max_result_bytes conninfo)
      (fun info ->
        create ~sw ~clock ~max_size ~acquire_timeout ~observe (fun () ->
            open_raw ~sw ~net ~clock:mono ~limits ~connect_timeout ~query_timeout
              ~max_result_bytes ~statement_cache info))
end
