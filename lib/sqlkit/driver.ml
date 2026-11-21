(* See driver.mli for the boundary story. One note lives here: the asymmetry
   with query dispatch. The *driver* is existential (a plain GADT + first-class
   module, so a connection is an ordinary value you can store or pass around),
   while the *query* is a modular explicit, so its params/row types can appear
   in the execution function's type. *)

(* What a driver reports when a statement fails. [sqlstate] is absent for
   client-side failures (a dropped socket, a malformed URI) where the server
   never got far enough to classify anything. *)
type error = {
  timeout_seconds : float option;
  message : string;
  sqlstate : Sqlstate.t option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}

let error ?timeout_seconds ?sqlstate ?detail ?hint ?constraint_name ?table_name
    ?column_name message =
  {
    timeout_seconds;
    message;
    sqlstate;
    detail;
    hint;
    constraint_name;
    table_name;
    column_name;
  }

module type S = sig
  type conn

  val now : conn -> float
  (** Monotonic seconds for telemetry. *)

  val is_cancellation : exn -> bool
  (** Identify runtime cancellation without depending on that runtime in sqlkit. *)

  val with_cleanup : finally:(unit -> unit) -> (unit -> 'a) -> 'a
  (** Always run cleanup, shielding it from cancellation where supported. *)

  (* [columns] is how many columns the caller expects. Drivers that discover the
     shape from the result (libpq) may ignore it; drivers that must declare it
     up front (Caqti) need it. *)
  val query :
    conn ->
    sql:string ->
    params:Value.t list ->
    columns:int ->
    (Value.t array list, error) result

  val exec : conn -> sql:string -> params:Value.t list -> (int option, error) result

  (* [rows] are pre-escaped COPY text lines (Value.Copy.line), no trailing
     newline. The runtime owns escaping so drivers cannot drift on it. *)
  val copy : conn -> sql:string -> rows:string list -> (int, error) result
  val close : conn -> unit
  (* Discard the physical connection, never return it to a pool. Idempotent. *)
end

(* Statements every driver must run once per physical connection before it is
   used. The temporal decoders parse ISO output, so this is a runtime
   invariant, not a per-driver preference -- a driver that skips it corrupts
   every date and timestamp read on servers with a different DateStyle. *)
let session_setup = [ "SET datestyle TO ISO" ]

(* [state.depth] is transaction depth, owned by [Sqlkit.transaction]: 0 outside
   any transaction, incremented per nesting level. It lives here so that the
   runtime can decide between BEGIN and SAVEPOINT without asking the driver,
   which could not answer uniformly anyway. *)
type state = { depth : int ref; mutable usable : bool; observe : Telemetry.observer }
type t = Conn : (module S with type conn = 'c) * 'c * state -> t

let make (type c) ?(observe = Telemetry.ignore) (module D : S with type conn = c) (c : c)
    : t =
  Conn ((module D), c, { depth = ref 0; usable = true; observe })

let release (Conn (_, _, state)) = state.usable <- false
