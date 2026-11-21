(** The backend boundary: everything above is backend-neutral, everything below is a
    driver (libpq, Caqti). Generated code never names a driver — it receives a {!t}, which
    packs the driver away existentially. This module is driver SPI, not application API.
*)

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
(** What a driver reports when a statement fails. [sqlstate] is absent for client-side
    failures, where the server never classified anything. [timeout_seconds] identifies a
    client query deadline and becomes [Error.Timeout]. *)

val error :
  ?timeout_seconds:float ->
  ?sqlstate:Sqlstate.t ->
  ?detail:string ->
  ?hint:string ->
  ?constraint_name:string ->
  ?table_name:string ->
  ?column_name:string ->
  string ->
  error

module type S = sig
  type conn

  val now : conn -> float
  (** Monotonic seconds for telemetry. *)

  val is_cancellation : exn -> bool
  (** Identify runtime cancellation without depending on that runtime in sqlkit. *)

  val with_cleanup : finally:(unit -> unit) -> (unit -> 'a) -> 'a
  (** Always run cleanup, shielding it from cancellation where supported. *)

  val query :
    conn ->
    sql:string ->
    params:Value.t list ->
    columns:int ->
    (Value.t array list, error) result
  (** [columns] is how many columns the caller expects; drivers that discover the shape
      from the result (libpq) may ignore it, drivers that must declare it up front (Caqti)
      need it. *)

  val exec : conn -> sql:string -> params:Value.t list -> (int option, error) result

  val copy : conn -> sql:string -> rows:string list -> (int, error) result
  (** Bulk load: [sql] is a [COPY ... FROM STDIN] statement, [rows] are pre-escaped COPY
      text lines ({!Sqlkit.Value.Copy.line}, no trailing newline). Returns rows written. A
      driver with no COPY support returns a clear [error] rather than failing obscurely.
  *)

  val close : conn -> unit
end

type state = { depth : int ref; mutable usable : bool; observe : Telemetry.observer }
(** A connection: the driver module, its handle, and the transaction-depth counter owned
    by [Sqlkit.transaction] (0 outside any transaction). The constructor is public because
    the runtime pattern-matches it; applications should treat values of this type as
    opaque. *)

type t = Conn : (module S with type conn = 'c) * 'c * state -> t

val make : ?observe:Telemetry.observer -> (module S with type conn = 'c) -> 'c -> t

val release : t -> unit
(** Invalidate a borrowed handle without closing the pool's physical connection. *)

val session_setup : string list
(** Statements every driver must run once per physical connection. The temporal decoders
    parse ISO output, so pinning DateStyle is a runtime invariant, not a per-driver
    preference. *)
