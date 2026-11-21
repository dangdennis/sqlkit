(** Thin OCaml surface over pq_stubs.c. Handles are opaque nativeints; the invariant is
    that every [prepare]/[describe]/[exec] result is [clear]ed. *)

type conn = nativeint
type res = nativeint

external connect : string -> conn = "sqlkit_pq_connect"
external connect_ok : conn -> bool = "sqlkit_pq_connect_ok"
external error_message : conn -> string = "sqlkit_pq_error_message"
external finish : conn -> unit = "sqlkit_pq_finish"
external prepare : conn -> string -> string -> res = "sqlkit_pq_prepare"
external describe_prepared : conn -> string -> res = "sqlkit_pq_describe_prepared"
external exec : conn -> string -> res = "sqlkit_pq_exec"

external exec_params_raw : conn -> string -> string option array -> res
  = "sqlkit_pq_exec_params"

external cmd_tuples : res -> int = "sqlkit_pq_cmd_tuples"
external result_error_field : res -> int -> string = "sqlkit_pq_result_error_field"

external exec_prepared_raw : conn -> string -> string option array -> res
  = "sqlkit_pq_exec_prepared"

(* libpq's PG_DIAG_* selectors, which are just the ASCII codes. *)
let diag_sqlstate = Char.code 'C'
let diag_message_detail = Char.code 'D'
let diag_message_hint = Char.code 'H'
let diag_constraint_name = Char.code 'n'
let diag_table_name = Char.code 't'
let diag_column_name = Char.code 'c'

let opt_field r f =
  match String.trim (result_error_field r f) with "" -> None | s -> Some s

external result_status : res -> int = "sqlkit_pq_result_status"
external result_error : res -> string = "sqlkit_pq_result_error"
external clear : res -> unit = "sqlkit_pq_clear"
external nparams : res -> int = "sqlkit_pq_nparams"
external paramtype : res -> int -> int = "sqlkit_pq_paramtype"
external nfields : res -> int = "sqlkit_pq_nfields"
external fname : res -> int -> string = "sqlkit_pq_fname"
external ftype : res -> int -> int = "sqlkit_pq_ftype"
external ftable : res -> int -> int = "sqlkit_pq_ftable"
external ftablecol : res -> int -> int = "sqlkit_pq_ftablecol"
external ntuples : res -> int = "sqlkit_pq_ntuples"
external getvalue : res -> int -> int -> string = "sqlkit_pq_getvalue"
external getisnull : res -> int -> int -> bool = "sqlkit_pq_getisnull"
external put_copy_data : conn -> string -> int = "sqlkit_pq_put_copy_data"
external put_copy_end : conn -> int = "sqlkit_pq_put_copy_end"
external get_result : conn -> res = "sqlkit_pq_get_result"
external result_is_null : res -> bool = "sqlkit_pq_result_is_null"

(* ExecStatusType *)
let command_ok = 1
let tuples_ok = 2
let copy_in = 4
let ok status = status = command_ok || status = tuples_ok

(* Run [f] on a result and always clear it, even if [f] raises. *)
let with_result r f = Fun.protect ~finally:(fun () -> clear r) (fun () -> f r)

(* Diagnostics collected off a failed result, before it is cleared. *)
type diag = {
  message : string;
  sqlstate : string option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}

let checked_params run conn text params =
  if
    String.contains text '\000'
    || Array.exists (function Some s -> String.contains s '\000' | None -> false) params
  then
    Error
      {
        message = "SQL or parameter contains a NUL byte";
        sqlstate = None;
        detail = None;
        hint = None;
        constraint_name = None;
        table_name = None;
        column_name = None;
      }
  else Ok (run conn text params)

let exec_params = checked_params exec_params_raw
let exec_prepared = checked_params exec_prepared_raw

let check r =
  if ok (result_status r) then Ok r
  else
    let d =
      {
        message = String.trim (result_error r);
        sqlstate = opt_field r diag_sqlstate;
        detail = opt_field r diag_message_detail;
        hint = opt_field r diag_message_hint;
        constraint_name = opt_field r diag_constraint_name;
        table_name = opt_field r diag_table_name;
        column_name = opt_field r diag_column_name;
      }
    in
    clear r;
    Error d

(* A catalog query returning rows as string arrays; NULL becomes "". *)
let query conn sql =
  match check (exec conn sql) with
  | Error e -> Error e
  | Ok r ->
      with_result r (fun r ->
          let cols = nfields r in
          let rows =
            List.init (ntuples r) (fun i ->
                Array.init cols (fun j -> if getisnull r i j then "" else getvalue r i j))
          in
          Ok rows)

(* A whole COPY ... FROM STDIN conversation: enter copy-in mode, stream the
   pre-escaped lines, end, and drain every pending result -- the connection is
   left usable even when the server aborts mid-stream. *)
let client_diag conn message_if_empty =
  let m = match String.trim (error_message conn) with "" -> message_if_empty | m -> m in
  {
    message = m;
    sqlstate = None;
    detail = None;
    hint = None;
    constraint_name = None;
    table_name = None;
    column_name = None;
  }

let copy_from conn ~sql ~rows =
  let r = exec conn sql in
  if result_status r <> copy_in then (
    match check r with
    | Error d -> Error d
    | Ok r ->
        clear r;
        Error (client_diag conn "statement did not enter COPY mode"))
  else begin
    clear r;
    let rec send = function
      | [] -> true
      | line :: tl -> put_copy_data conn (line ^ "\n") = 1 && send tl
    in
    let sent = send rows in
    let ended = put_copy_end conn = 1 in
    (* Always drain: PQgetResult until NULL, or the connection is wedged. The
       server's verdict (an error names the offending line) beats any
       client-side failure we saw while streaming. *)
    let rec drain acc =
      let r = get_result conn in
      if result_is_null r then acc
      else
        let this =
          match check r with
          | Ok r -> with_result r (fun r -> Ok (cmd_tuples r))
          | Error d -> Error d
        in
        let acc =
          match (this, acc) with
          | Error _, _ -> this
          | Ok _, Error _ -> acc
          | Ok _, _ -> this
        in
        drain acc
    in
    match drain (Ok 0) with
    | Error d -> Error d
    | Ok n when sent && ended -> Ok n
    | Ok _ -> Error (client_diag conn "COPY data could not be sent")
  end

(* Connection string from the conventional environment variables. Shared by the
   generator and the libpq driver so the env policy cannot drift; the Caqti
   driver mirrors it in URI form. *)
let conninfo_of_env () =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some u when String.trim u <> "" -> u
  | _ ->
      let get k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d in
      Printf.sprintf "host=%s port=%s user=%s dbname=%s%s" (get "PGHOST" "127.0.0.1")
        (get "PGPORT" "5432") (get "PGUSER" "postgres") (get "PGDATABASE" "postgres")
        (match Sys.getenv_opt "PGPASSWORD" with
        | Some p when p <> "" -> " password=" ^ p
        | _ -> "")

external cmd_status : res -> string = "sqlkit_pq_cmd_status"
external fmod : res -> int -> int = "sqlkit_pq_fmod"
external monotonic_time : unit -> float = "sqlkit_monotonic_time"
external connect_timeout : string -> int -> conn = "sqlkit_pq_connect_timeout"
external cmd_tuples_text : res -> string = "sqlkit_pq_cmd_tuples_text"

module Async = struct
  external connect_start_raw : string -> conn = "sqlkit_pq_connect_start"
  external conn_is_null : conn -> bool = "sqlkit_pq_conn_is_null"
  external connect_poll_raw : conn -> int = "sqlkit_pq_connect_poll"
  external socket_fd : conn -> int = "sqlkit_pq_socket"
  external socket_raw : conn -> Unix.file_descr = "sqlkit_pq_socket"
  external set_nonblocking : conn -> bool = "sqlkit_pq_set_nonblocking"
  external enter_pipeline : conn -> bool = "sqlkit_pq_enter_pipeline"
  external exit_pipeline : conn -> bool = "sqlkit_pq_exit_pipeline"
  external pipeline_sync : conn -> bool = "sqlkit_pq_pipeline_sync"
  external transaction_idle : conn -> bool = "sqlkit_pq_transaction_idle"
  external send_query : conn -> string -> bool = "sqlkit_pq_send_query"
  external send_prepare : conn -> string -> string -> bool = "sqlkit_pq_send_prepare"

  external send_query_params : conn -> string -> string option array -> bool
    = "sqlkit_pq_send_query_params"

  external send_query_prepared : conn -> string -> string option array -> bool
    = "sqlkit_pq_send_query_prepared"

  external flush_raw : conn -> int = "sqlkit_pq_flush"
  external consume_input : conn -> bool = "sqlkit_pq_consume_input"
  external is_busy : conn -> bool = "sqlkit_pq_is_busy"
  external get_result_raw : conn -> res = "sqlkit_pq_get_result_nb"
  external put_copy_data_raw : conn -> string -> int = "sqlkit_pq_put_copy_data_nb"
  external put_copy_end_raw : conn -> int = "sqlkit_pq_put_copy_end_nb"
  external cancel : conn -> Unix.file_descr -> bool = "sqlkit_pq_cancel_detached"

  let connect_start conninfo =
    let c = connect_start_raw conninfo in
    if conn_is_null c then None else Some c

  type polling = Polling_failed | Polling_reading | Polling_writing | Polling_ok

  let connect_poll c =
    match connect_poll_raw c with
    | 1 -> Polling_reading
    | 2 -> Polling_writing
    | 3 -> Polling_ok
    | _ -> Polling_failed

  let socket c = if socket_fd c < 0 then None else Some (socket_raw c)

  type progress = Done | Again | Failed

  let progress_of_flush = function 0 -> Done | 1 -> Again | _ -> Failed
  let progress_of_put = function 1 -> Done | 0 -> Again | _ -> Failed
  let flush c = progress_of_flush (flush_raw c)
  let put_copy_data c data = progress_of_put (put_copy_data_raw c data)
  let put_copy_end c = progress_of_put (put_copy_end_raw c)

  let get_result c =
    let r = get_result_raw c in
    if result_is_null r then None else Some r

  type status = Finished | Copy_in | Pipeline_sync | Pipeline_aborted

  let status r =
    match result_status r with
    | 4 -> Copy_in
    | 10 -> Pipeline_sync
    | 11 -> Pipeline_aborted
    | _ -> Finished
end
