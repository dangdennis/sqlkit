(** Thin binding over libpq, shared by the generator (PQdescribePrepared,
    PQftable/PQftablecol — the basis of nullability and shared-model detection) and the
    libpq driver (PQexecParams / prepared execution).

    [conn] and [res] are abstract on purpose: both are raw C pointers, and when they were
    aliases of the same type, [finish] on a result or [clear] on a connection typechecked
    and corrupted memory. Lifetimes are the caller's: every
    [prepare]/[describe_prepared]/[exec]/[exec_params]/[exec_prepared] result must be
    [clear]ed (or go through {!with_result}/{!check}), and every [connect]ed handle must
    be [finish]ed. *)

type conn
type res

type diag = {
  message : string;
  sqlstate : string option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}
(** Diagnostics collected off a failed result before it is cleared. *)

(** {1 Connections} *)

val connect : string -> conn
val connect_ok : conn -> bool
val error_message : conn -> string
val finish : conn -> unit

val conninfo_of_env : unit -> string
(** [DATABASE_URL], or a keyword string from [PGHOST]/[PGPORT]/[PGUSER]/
    [PGDATABASE]/[PGPASSWORD]. *)

(** {1 Executing} *)

val prepare : conn -> string -> string -> res
val describe_prepared : conn -> string -> res
val exec : conn -> string -> res
val exec_params : conn -> string -> string option array -> (res, diag) result
val exec_prepared : conn -> string -> string option array -> (res, diag) result

(** {1 Results} *)

val ok : int -> bool
val result_status : res -> int
val clear : res -> unit

val check : res -> (res, diag) result
(** [Ok] passes the live result through; on failure the diagnostics are read and the
    result is cleared. *)

val with_result : res -> (res -> 'a) -> 'a
(** Runs the function and always clears, even on raise. *)

val opt_field : res -> int -> string option
val diag_sqlstate : int
val nparams : res -> int
val paramtype : res -> int -> int
val nfields : res -> int
val fname : res -> int -> string
val ftype : res -> int -> int
val ftable : res -> int -> int
val ftablecol : res -> int -> int
val ntuples : res -> int
val getvalue : res -> int -> int -> string
val getisnull : res -> int -> int -> bool
val cmd_tuples : res -> int

val query : conn -> string -> (string array list, diag) result
(** A catalog query returning rows as string arrays; NULL becomes [""]. *)

val copy_from : conn -> sql:string -> rows:string list -> (int, diag) result
(** The whole [COPY ... FROM STDIN] conversation: enter copy-in mode, stream the
    pre-escaped text lines (no trailing newline), end, and drain every pending result so
    the connection stays usable even when the server aborts mid-stream. Returns rows
    written. *)

val server_version : conn -> int
(** libpq's encoding: 150004 for 15.4. *)

val unsupported_server : conn -> string option
(** A message when the connected server is older than the project supports (15). *)

val fmod : res -> int -> int
val monotonic_time : unit -> float
val connect_timeout : string -> int -> conn
val cmd_tuples_text : res -> string

(** {1 Asynchronous API}

    For a driver that waits on the socket itself. No function here blocks on the server:
    each one queues bytes, moves bytes the socket already has, or reads libpq's state. The
    caller waits on {!Async.socket} between calls. The exception is
    {!Async.connect_start}, where libpq resolves host names with a blocking lookup.

    One connection belongs to one fiber at a time. *)
module Async : sig
  val connect_start : string -> conn option
  (** [None] only when libpq could not allocate. A returned connection may already have
      failed; check {!connect_ok} or poll it. It must be [finish]ed either way. *)

  type polling = Polling_failed | Polling_reading | Polling_writing | Polling_ok

  val connect_poll : conn -> polling

  val socket : conn -> Unix.file_descr option
  (** The descriptor can change while connecting; ask again after each poll. *)

  val set_nonblocking : conn -> bool
  val enter_pipeline : conn -> bool
  val exit_pipeline : conn -> bool
  val pipeline_sync : conn -> bool

  val transaction_idle : conn -> bool
  (** No transaction open and no command in progress. *)

  val send_query : conn -> string -> bool
  (** Simple protocol; not available in pipeline mode. Needed for COPY. *)

  val send_prepare : conn -> string -> string -> bool

  val send_query_params : conn -> string -> string option array -> bool
  (** Text parameters; [None] is NULL. The caller must reject NUL bytes first, in the SQL
      and in every parameter: libpq would truncate there. *)

  val send_query_prepared : conn -> string -> string option array -> bool

  type progress = Done | Again | Failed

  val flush : conn -> progress
  (** [Again]: wait until the socket is readable or writable, consume input if readable,
      and call again. *)

  val consume_input : conn -> bool
  val is_busy : conn -> bool

  val get_result : conn -> res option
  (** Call only when {!is_busy} is false. The result must be [clear]ed. *)

  val put_copy_data : conn -> string -> progress
  val put_copy_end : conn -> progress

  type status = Finished | Copy_in | Pipeline_sync | Pipeline_aborted

  val status : res -> status
  (** [Finished] covers success and failure alike; pass the result to {!check}. *)

  val cancel : conn -> Unix.file_descr -> bool
  (** Ask the server to stop the running command, on a thread of its own. When the request
      has been delivered (or has failed) the thread writes one byte to the descriptor and
      closes it. [false] means no thread was started and the descriptor is still the
      caller's. Safe to [finish] the connection at any point. Best effort. *)
end
