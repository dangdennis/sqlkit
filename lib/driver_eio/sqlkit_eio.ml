(* Native PostgreSQL driver for Eio.

   libpq does the protocol, authentication and TLS; this module does the
   waiting. Every libpq call used here is nonblocking, and whenever libpq needs
   the socket the fiber parks on it through Eio, so one domain serves many
   connections and a query can be cancelled or timed out like any other fiber.

   The connection stays in pipeline mode. A statement seen for the first time
   goes out as Parse + Bind/Execute + Sync in one write, so preparing costs no
   extra round trip, and session setup is one round trip however many
   statements it has. COPY is the exception: libpq refuses it inside a
   pipeline, so [copy] steps out and back in.

   Everything moves in text format, as in the blocking driver. *)

module A = Pq.Async

type clock = Eio.Time.Mono.ty Eio.Resource.t

let client_diag message =
  {
    Pq.message;
    sqlstate = None;
    detail = None;
    hint = None;
    constraint_name = None;
    table_name = None;
    column_name = None;
  }

module Raw = struct
  type conn = {
    raw : Pq.conn;
    clock : clock;
    query_timeout : float;
    stmts : (string, string) Hashtbl.t;
    statement_cache : bool;
    mutable generation : int;
    mutable closed : bool;
  }

  (* The connection can no longer be trusted: the socket failed, or libpq and
     the server disagree about where the conversation is. Never escapes this
     module; [guard] turns it into a driver error and discards the connection. *)
  exception Io of string

  let now _ = Pq.monotonic_time ()
  let is_cancellation = function Eio.Cancel.Cancelled _ -> true | _ -> false

  let with_cleanup ~finally f =
    Fun.protect ~finally:(fun () -> Eio.Cancel.protect finally) f

  let close c =
    if not c.closed then (
      c.closed <- true;
      Pq.finish c.raw)

  let io_error c fallback =
    match String.trim (Pq.error_message c.raw) with "" -> fallback | m -> m

  let fail c fallback = raise (Io (io_error c fallback))
  let sent c ok = if not ok then fail c "could not queue the command"

  let fd c =
    match A.socket c.raw with Some fd -> fd | None -> fail c "connection has no socket"

  let consume c = if not (A.consume_input c.raw) then fail c "connection lost"

  (* The server may be blocked writing to us while we are blocked writing to
     it, so a stalled flush has to keep reading as well. *)
  let flush_step c =
    let fd = fd c in
    let readable =
      Eio.Fiber.first
        (fun () ->
          Eio_unix.await_readable fd;
          true)
        (fun () ->
          Eio_unix.await_writable fd;
          false)
    in
    if readable then consume c

  let rec flush c =
    match A.flush c.raw with
    | A.Done -> ()
    | A.Failed -> fail c "could not send to the server"
    | A.Again ->
        flush_step c;
        flush c

  let rec next_result c =
    if A.is_busy c.raw then begin
      Eio_unix.await_readable (fd c);
      consume c;
      next_result c
    end
    else A.get_result c.raw

  type reply =
    | Rows of Sqlkit.Value.t array list * int option
    | Refused of Pq.diag
    | Skipped

  (* Copies what is needed off the result and clears it, so no libpq memory is
     held across a point where the fiber can be cancelled. *)
  let reply r =
    match A.status r with
    | A.Pipeline_aborted ->
        Pq.clear r;
        Skipped
    | A.Finished | A.Copy_in | A.Pipeline_sync -> (
        match Pq.check r with
        | Error d -> Refused d
        | Ok r ->
            Pq.with_result r (fun r ->
                let cols = Pq.nfields r in
                let rows =
                  List.init (Pq.ntuples r) (fun i ->
                      Array.init cols (fun j ->
                          if Pq.getisnull r i j then Sqlkit.Value.Null
                          else Sqlkit.Value.Text (Pq.getvalue r i j)))
                in
                Rows (rows, int_of_string_opt (Pq.cmd_tuples_text r))))

  let refused = function Refused _ -> true | Rows _ | Skipped -> false

  (* One reply per queued command, up to the Sync marker. libpq separates
     commands with a null result; two in a row mean nothing more is coming.
     When the server reports an error and then drops the connection (57P01),
     the Sync never arrives: the error already read is the better diagnosis, so
     it is returned and the dead connection is noticed by the caller. *)
  let read_until_sync c =
    let rec go acc gap =
      match next_result c with
      | exception Io _ when List.exists refused acc -> List.rev acc
      | None ->
          if List.exists refused acc && not (Pq.connect_ok c.raw) then List.rev acc
          else if gap || not (Pq.connect_ok c.raw) then fail c "connection lost"
          else go acc true
      | Some r -> (
          match A.status r with
          | A.Pipeline_sync ->
              Pq.clear r;
              List.rev acc
          | A.Finished | A.Copy_in | A.Pipeline_aborted -> go (reply r :: acc) false)
    in
    go [] false

  let sync c =
    sent c (A.pipeline_sync c.raw);
    flush c;
    read_until_sync c

  let confused c = fail c "unexpected reply to a pipelined statement"

  (* Named after a digest of the SQL, for the reason given in sqlkit-pgwire:
     behind a pooler in transaction mode a counter would let one client run
     another's statement. *)
  let statement_name c sql =
    Printf.sprintf "sqlkit_%s%s"
      (Digest.to_hex (Digest.string sql))
      (if c.generation = 0 then "" else "_" ^ string_of_int c.generation)

  (* Returns the statement's reply and whether a cached plan produced it. *)
  let rec attempt c ~sql ~args =
    if not c.statement_cache then begin
      sent c (A.send_query_params c.raw sql args);
      match sync c with [ r ] -> (r, false) | _ -> confused c
    end
    else
      match Hashtbl.find_opt c.stmts sql with
      | Some name -> (
          sent c (A.send_query_prepared c.raw name args);
          match sync c with [ r ] -> (r, true) | _ -> confused c)
      | None -> (
          let name = statement_name c sql in
          sent c (A.send_prepare c.raw name sql);
          sent c (A.send_query_prepared c.raw name args);
          match sync c with
          | [ Rows _; r ] ->
              Hashtbl.replace c.stmts sql name;
              (r, false)
          | (Refused { Pq.sqlstate = Some "42P05"; _ } as exists) :: _ -> (
              (* Already prepared on this backend by an earlier client of a
                 pooler; the name says it is the same statement. *)
              Hashtbl.replace c.stmts sql name;
              match attempt c ~sql ~args with
              | Refused { Pq.sqlstate = Some "25P02"; _ }, _ -> (exists, false)
              | answer -> answer)
          | (Refused _ as r) :: _ -> (r, false)
          | _ -> confused c)

  (* A cached statement can go stale: dropped by DEALLOCATE or DISCARD ALL
     (26000), or its result type changed under it (0A000). Forget it and run
     the statement once more from scratch. Inside a transaction the stale error
     has already aborted it, so the second attempt is refused with 25P02 and
     the original error is the one worth reporting. The exception is what makes
     the retry necessary there: ROLLBACK and COMMIT are still accepted, and a
     transaction must not be left open because its ROLLBACK was cached. *)
  let dispatch c ~sql ~args =
    (* Behind a pooler without prepared-statement support each attempt can
       land on a different backend, one that lacks the statement or already
       has it, so one retry is not always enough. The bound keeps a server
       that always answers "stale" from holding the caller forever. *)
    let rec go ~first ~tries =
      match attempt c ~sql ~args with
      | (Refused { Pq.sqlstate = Some (("26000" | "0A000") as code); _ } as stale), true
        when tries > 0 ->
          Hashtbl.remove c.stmts sql;
          if code = "0A000" then c.generation <- c.generation + 1;
          go ~first:(Some (Option.value first ~default:stale)) ~tries:(tries - 1)
      | Refused { Pq.sqlstate = Some "25P02"; _ }, _ when first <> None ->
          Option.get first
      | r, _ -> r
    in
    go ~first:None ~tries:4

  (* Ask the server to stop, and give the request a moment to arrive before
     the connection is closed. A pooler in front of the server drops a cancel
     whose client has already gone, which would leave the statement running,
     and holding its locks, with nobody waiting for it. libpq's request blocks,
     so it runs on its own thread and reports back through a pipe. *)
  let cancel c =
    let finished, report = Unix.pipe ~cloexec:true () in
    if A.cancel c.raw report then
      ignore
        (Eio.Time.Timeout.run (Eio.Time.Timeout.seconds c.clock 2.) (fun () ->
             Eio_unix.await_readable finished;
             Ok ())
          : (unit, [ `Timeout ]) result)
    else Unix.close report;
    Unix.close finished

  let fatal c (d : Pq.diag) =
    (not (Pq.connect_ok c.raw))
    ||
    match d.Pq.sqlstate with
    | Some code ->
        String.starts_with ~prefix:"08" code
        || code = "57P01" || code = "57P02" || code = "57P03"
    | None -> false

  let of_diag (d : Pq.diag) =
    Sqlkit.Driver.error d.Pq.message
      ?sqlstate:(Option.map Sqlkit.Sqlstate.of_string d.Pq.sqlstate)
      ?detail:d.Pq.detail ?hint:d.Pq.hint ?constraint_name:d.Pq.constraint_name
      ?table_name:d.Pq.table_name ?column_name:d.Pq.column_name

  (* Every operation runs under the client deadline. If the fiber is cancelled
     or the deadline passes while the server is still working, the server is
     told to stop and the connection is dropped: its position in the protocol
     is unknown, and a backend left running would hold its locks until it
     next tried to write to a socket nobody reads. *)
  let guard c f =
    if c.closed then Error (Sqlkit.Driver.error "connection discarded")
    else
      let run () =
        match f () with
        | Ok _ as r -> r
        | Error d ->
            if fatal c d then close c;
            Error (of_diag d)
        | exception Io message ->
            close c;
            Error (Sqlkit.Driver.error message)
        | exception e ->
            Eio.Cancel.protect (fun () -> cancel c);
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
        else
          match dispatch c ~sql ~args with
          | Rows (rows, affected) -> Ok (rows, affected)
          | Refused d -> Error d
          | Skipped -> confused c)

  let query c ~sql ~params ~columns:_ = Result.map fst (statement c ~sql ~params)
  let exec c ~sql ~params = Result.map snd (statement c ~sql ~params)

  (* Results until libpq's null, keeping the first error. *)
  let rec drain c verdict =
    match next_result c with
    | None -> verdict
    | Some r ->
        let this =
          match Pq.check r with
          | Ok r -> Pq.with_result r (fun r -> Ok (Pq.cmd_tuples r))
          | Error d -> Error d
        in
        drain c (match verdict with Error _ -> verdict | Ok _ -> this)

  let copy_chunk = 64 * 1024

  let copy_conversation c ~sql ~rows =
    sent c (A.send_query c.raw sql);
    flush c;
    match next_result c with
    | None -> fail c "statement did not enter COPY mode"
    | Some r when A.status r = A.Copy_in ->
        Pq.clear r;
        let rec put data =
          match A.put_copy_data c.raw data with
          | A.Done -> true
          | A.Failed -> false
          | A.Again ->
              flush_step c;
              put data
        in
        let buf = Buffer.create copy_chunk in
        let take () =
          let data = Buffer.contents buf in
          Buffer.clear buf;
          data
        in
        let rec send = function
          | [] -> Buffer.length buf = 0 || put (take ())
          | line :: rest ->
              Buffer.add_string buf line;
              Buffer.add_char buf '\n';
              if Buffer.length buf < copy_chunk then send rest
              else put (take ()) && send rest
        in
        let delivered = send rows in
        let rec finish () =
          match A.put_copy_end c.raw with
          | A.Done -> true
          | A.Failed -> false
          | A.Again ->
              flush_step c;
              finish ()
        in
        let ended = finish () in
        flush c;
        (* Always drain. The server's verdict names the offending line and
           beats anything seen on the client while streaming. *)
        begin match drain c (Ok 0) with
        | Error _ as e -> e
        | Ok _ as n when delivered && ended -> n
        | Ok _ -> fail c "COPY data could not be sent"
        end
    | Some r ->
        let first =
          match Pq.check r with
          | Error d -> Error d
          | Ok r ->
              Pq.clear r;
              Error (client_diag "statement did not enter COPY mode")
        in
        drain c first

  let copy c ~sql ~rows =
    guard c (fun () ->
        if has_nul sql then Error (client_diag "SQL contains a NUL byte")
        else begin
          sent c (A.exit_pipeline c.raw);
          let verdict = copy_conversation c ~sql ~rows in
          if Pq.connect_ok c.raw then sent c (A.enter_pipeline c.raw);
          verdict
        end)

  (* No round trip: reads whatever the socket already holds, which is how a
     connection the server closed while idle is noticed. Twice, because the
     server's parting message and the end of the stream arrive as separate
     reads. A handle returned with a transaction still open is not reused. *)
  let healthy c =
    (not c.closed) && A.consume_input c.raw && A.consume_input c.raw
    && Pq.connect_ok c.raw && A.transaction_idle c.raw

  let setup c statements =
    List.iter (fun sql -> sent c (A.send_query_params c.raw sql [||])) statements;
    List.find_map (function Refused d -> Some d | Rows _ | Skipped -> None) (sync c)

  let establish ~clock ~query_timeout ~statement_cache ~statements conninfo =
    match A.connect_start conninfo with
    | None -> Error (Sqlkit.Error.Connect "libpq could not allocate a connection")
    | Some raw ->
        let c =
          {
            raw;
            clock;
            query_timeout;
            stmts = Hashtbl.create 16;
            statement_cache;
            generation = 0;
            closed = false;
          }
        in
        let ready = ref false in
        with_cleanup
          ~finally:(fun () -> if not !ready then close c)
          (fun () ->
            (* libpq's rule: wait for writability first, then for whatever
               each poll asks. The descriptor can change between polls. *)
            let rec poll wait =
              wait (fd c);
              match A.connect_poll raw with
              | A.Polling_ok -> ()
              | A.Polling_reading -> poll Eio_unix.await_readable
              | A.Polling_writing -> poll Eio_unix.await_writable
              | A.Polling_failed -> fail c "connection failed"
            in
            match
              poll Eio_unix.await_writable;
              Option.iter (fun message -> raise (Io message)) (Pq.unsupported_server raw);
              if not (A.set_nonblocking raw) then fail c "could not set nonblocking mode";
              if not (A.enter_pipeline raw) then fail c "could not enter pipeline mode";
              setup c statements
            with
            | None ->
                ready := true;
                Ok c
            | Some d -> Error (Sqlkit.Error.Connect d.Pq.message)
            | exception Io message -> Error (Sqlkit.Error.Connect message))
end

let conninfo_of_env = Pq.conninfo_of_env
let boundary = Sqlkit_pool_eio.boundary

let validate ~limits ~connect_timeout ~query_timeout conninfo =
  match Sqlkit.Limits.validate limits with
  | Error message ->
      Error (Sqlkit.Error.Invalid_argument { operation = "connect"; message })
  | Ok () ->
      if String.contains conninfo '\000' then
        Error
          (Sqlkit.Error.Invalid_argument
             { operation = "connect"; message = "NUL in connection string" })
      else if
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

let open_raw ~clock ~limits ~connect_timeout ~query_timeout ~statement_cache conninfo =
  boundary ~operation:"connect" (fun () ->
      let statements = Sqlkit.Driver.session_setup @ Sqlkit.Limits.statements limits in
      match
        Eio.Time.Timeout.run (Eio.Time.Timeout.seconds clock connect_timeout) (fun () ->
            Ok (Raw.establish ~clock ~query_timeout ~statement_cache ~statements conninfo))
      with
      | Ok r -> r
      | Error `Timeout ->
          Error
            (Sqlkit.Error.Timeout
               { operation = "connect"; phase = Connect; seconds = connect_timeout }))

let connect ~sw ~clock ?(limits = Sqlkit.Limits.default) ?(connect_timeout = 10.)
    ?(query_timeout = 35.) ?(statement_cache = true) ?observe conninfo =
  let clock = (clock :> clock) in
  Result.bind (validate ~limits ~connect_timeout ~query_timeout conninfo) (fun () ->
      Result.map
        (fun c ->
          let conn = Sqlkit.Driver.make ?observe (module Raw) c in
          Eio.Switch.on_release sw (fun () ->
              Raw.close c;
              Sqlkit.Driver.release conn);
          conn)
        (open_raw ~clock ~limits ~connect_timeout ~query_timeout ~statement_cache conninfo))

module Pool = struct
  include Sqlkit_pool_eio.Make (Raw) (Raw)

  let create ~sw ~clock ?(max_size = 10) ?(acquire_timeout = 5.) ?(connect_timeout = 10.)
      ?(query_timeout = 35.) ?(statement_cache = true) ?(limits = Sqlkit.Limits.default)
      ?(observe = Sqlkit.Telemetry.ignore) conninfo =
    let mono = (clock :> clock) in
    Result.bind (validate ~limits ~connect_timeout ~query_timeout conninfo) (fun () ->
        create ~sw ~clock ~max_size ~acquire_timeout ~observe (fun () ->
            open_raw ~clock:mono ~limits ~connect_timeout ~query_timeout ~statement_cache
              conninfo))
end
