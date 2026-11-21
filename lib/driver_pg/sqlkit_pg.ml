(* Postgres driver for the sqlkit runtime.

   Implements Sqlkit.Driver.S over libpq. Nothing above Driver.S knows this
   exists -- generated code takes a [Sqlkit.conn] and never names a backend --
   which is why sqlkit_caqti can implement the same signature (pooling, Eio)
   without touching a line of generated or application code.

   Everything moves in Postgres text format. Values come back as [Value.Text]
   or [Value.Null] and are parsed by [Sqlkit.Row], which is why those decoders
   accept text. *)

module Raw = struct
  type prep = Prepared of string | Unpreparable

  type conn = {
    raw : Pq.conn;
    stmts : (string, prep) Hashtbl.t;
    mutable counter : int;
    mutable closed : bool;
  }

  let now _ = Pq.monotonic_time ()
  let is_cancellation _ = false
  let with_cleanup = Fun.protect

  let close conn =
    if not conn.closed then (
      conn.closed <- true;
      Pq.finish conn.raw)

  (* peek at a result's failure state without consuming it *)
  let stale_statement r =
    (not (Pq.ok (Pq.result_status r)))
    &&
    match Pq.opt_field r Pq.diag_sqlstate with
    | Some ("26000" | "0A000") -> true
    | None | Some _ -> false

  let run conn sql params =
    let ( let* ) = Result.bind in
    let args = Array.of_list (List.map Sqlkit.Value.to_pg_text params) in
    let exec_direct () = Pq.exec_params conn.raw sql args in
    let prepare_as stmt =
      let r = Pq.prepare conn.raw stmt sql in
      match Pq.check r with
      | Ok r ->
          Pq.clear r;
          true
      | Error _ -> false
    in
    match Hashtbl.find_opt conn.stmts sql with
    | Some Unpreparable -> exec_direct ()
    | Some (Prepared stmt) ->
        let* r = Pq.exec_prepared conn.raw stmt args in
        (* Only the two cache-staleness codes are intercepted; any other
           result, success or failure, passes through untouched so the caller
           diagnoses the original error -- re-running inside an aborted
           transaction would mask it with 25P02. *)
        if stale_statement r then begin
          Pq.clear r;
          if prepare_as stmt then Pq.exec_prepared conn.raw stmt args
          else begin
            Hashtbl.replace conn.stmts sql Unpreparable;
            exec_direct ()
          end
        end
        else Ok r
    | None ->
        conn.counter <- conn.counter + 1;
        let stmt = Printf.sprintf "sqlkit_s%d" conn.counter in
        if prepare_as stmt then begin
          Hashtbl.replace conn.stmts sql (Prepared stmt);
          Pq.exec_prepared conn.raw stmt args
        end
        else begin
          Hashtbl.replace conn.stmts sql Unpreparable;
          exec_direct ()
        end

  (* libpq reports the column count back with the result, so [columns] is
     redundant here. It exists for drivers that must declare the shape up
     front. *)
  let of_diag (d : Pq.diag) =
    Sqlkit.Driver.error d.Pq.message
      ?sqlstate:(Option.map Sqlkit.Sqlstate.of_string d.Pq.sqlstate)
      ?detail:d.Pq.detail ?hint:d.Pq.hint ?constraint_name:d.Pq.constraint_name
      ?table_name:d.Pq.table_name ?column_name:d.Pq.column_name

  let query conn ~sql ~params ~columns:_ =
    if conn.closed then Error (Sqlkit.Driver.error "connection discarded")
    else
      match Result.bind (run conn sql params) Pq.check with
      | Error e -> Error (of_diag e)
      | Ok r ->
          Pq.with_result r (fun r ->
              let cols = Pq.nfields r in
              Ok
                (List.init (Pq.ntuples r) (fun i ->
                     Array.init cols (fun j ->
                         if Pq.getisnull r i j then Sqlkit.Value.Null
                         else Sqlkit.Value.Text (Pq.getvalue r i j)))))

  let exec conn ~sql ~params =
    if conn.closed then Error (Sqlkit.Driver.error "connection discarded")
    else
      match Result.bind (run conn sql params) Pq.check with
      | Error e -> Error (of_diag e)
      | Ok r -> Pq.with_result r (fun r -> Ok (int_of_string_opt (Pq.cmd_tuples_text r)))

  (* COPY skips the prepared-statement cache: COPY cannot be prepared, and the
     conversation is one shot by nature. *)
  let copy conn ~sql ~rows =
    if conn.closed then Error (Sqlkit.Driver.error "connection discarded")
    else
      match Pq.copy_from conn.raw ~sql ~rows with
      | Ok n -> Ok n
      | Error e -> Error (of_diag e)
end

(* ---------- connecting ---------- *)

let conninfo_of_env = Pq.conninfo_of_env

let open_raw ~limits ~connect_timeout conninfo =
  match Sqlkit.Limits.validate limits with
  | Error message ->
      Error (Sqlkit.Error.Invalid_argument { operation = "connect"; message })
  | Ok ()
    when connect_timeout < 1 || connect_timeout > 2147483647
         || String.contains conninfo '\000' ->
      Error
        (Sqlkit.Error.Invalid_argument
           {
             operation = "connect";
             message = "invalid connect timeout or NUL in connection string";
           })
  | Ok () ->
      let c = Pq.connect_timeout conninfo connect_timeout in
      if not (Pq.connect_ok c) then (
        let message = String.trim (Pq.error_message c) in
        Pq.finish c;
        Error (Sqlkit.Error.Connect message))
      else if Pq.unsupported_server c <> None then (
        let message = Option.get (Pq.unsupported_server c) in
        Pq.finish c;
        Error (Sqlkit.Error.Connect message))
      else
        let rec setup = function
          | [] ->
              Ok { Raw.raw = c; stmts = Hashtbl.create 16; counter = 0; closed = false }
          | sql :: rest -> (
              match Pq.check (Pq.exec c sql) with
              | Ok r ->
                  Pq.clear r;
                  setup rest
              | Error d ->
                  Pq.finish c;
                  Error (Sqlkit.Error.Connect d.Pq.message))
        in
        setup (Sqlkit.Driver.session_setup @ Sqlkit.Limits.statements limits)

let connect ?(limits = Sqlkit.Limits.default) ?(connect_timeout = 10) ?observe conninfo =
  Result.map
    (fun c -> Sqlkit.Driver.make ?observe (module Raw) c)
    (open_raw ~limits ~connect_timeout conninfo)

let with_connection ?limits ?connect_timeout ?observe conninfo f =
  match connect ?limits ?connect_timeout ?observe conninfo with
  | Error e -> Error e
  | Ok conn ->
      Fun.protect
        ~finally:(fun () -> ignore (Sqlkit.close conn))
        (fun () ->
          Sqlkit.callback conn ~operation:"with_connection" (fun () -> Ok (f conn)))
