(** Native PostgreSQL driver for Eio, with connection pooling.

    Built on libpq's asynchronous API: libpq keeps the protocol, authentication and TLS,
    and the fiber waits on the socket through Eio. Compared with [sqlkit-caqti] this keeps
    every diagnostic field (constraint, table, column, detail, hint), reports real
    affected-row counts, supports [:copy], and asks the server to stop a statement when
    its fiber is cancelled or its deadline passes.

    A statement is prepared on first use and cached per connection. The prepare travels in
    the same write as the first execution, so it costs no extra round trip.

    Eio is direct style, so generated signatures are unchanged: a query returns a plain
    [result].

    {[
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pool =
      Result.get_ok
        (Sqlkit_eio.Pool.create ~sw ~clock:env#mono_clock ~max_size:10
           (Sqlkit_eio.conninfo_of_env ()))
    in
    Sqlkit_eio.Pool.use pool (fun conn -> Db.get_user conn ~id)
    ]}

    Connection strings are libpq's: a [postgresql://] URI or [key=value] pairs. Host names
    are resolved by libpq with a blocking lookup, which holds the domain for its duration;
    use an address where that matters. *)

val conninfo_of_env : unit -> string
(** Connection string from [DATABASE_URL], or from
    [PGHOST]/[PGPORT]/[PGUSER]/[PGDATABASE]/[PGPASSWORD]. *)

val connect :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.Mono.t ->
  ?limits:Sqlkit.Limits.t ->
  ?connect_timeout:float ->
  ?query_timeout:float ->
  ?observe:Sqlkit.Telemetry.observer ->
  string ->
  (Sqlkit.conn, Sqlkit.Error.t) result
(** A single connection, closed when the switch finishes. Prefer {!Pool}. Client timeouts
    default to 10 seconds for connection establishment and 35 seconds per query operation.
    Both must be positive and finite. Server limits default to {!Sqlkit.Limits.default}.
    Handles must be used sequentially, from the domain that opened them.

    A statement that is cancelled or exceeds [query_timeout] costs the connection: the
    server is asked to stop and the handle becomes unusable. *)

module Pool : sig
  type t
  (** A pool carries no query operations on purpose. The only way to reach a
      {!Sqlkit.conn} is {!use} or {!transaction}, both of which scope it, so a connection
      cannot outlive its borrow and a transaction cannot leak statements onto a different
      connection. *)

  val create :
    sw:Eio.Switch.t ->
    clock:_ Eio.Time.Mono.t ->
    ?max_size:int ->
    ?acquire_timeout:float ->
    ?connect_timeout:float ->
    ?query_timeout:float ->
    ?limits:Sqlkit.Limits.t ->
    ?observe:Sqlkit.Telemetry.observer ->
    string ->
    (t, Sqlkit.Error.t) result
  (** Lazy pool, confined to one Eio domain and switch. Defaults: maximum 10 connections,
      acquisition deadline 5 seconds (including connection setup), connection deadline 10
      seconds, query deadline 35 seconds per operation. Acquisition stops being timed
      before the callback begins. Waiting cancellation preserves pool capacity. A
      connection found dead, or returned with a transaction still open, is replaced. An
      observer receives events without SQL, parameters or error messages. *)

  val use :
    t -> (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) -> ('a, Sqlkit.Error.t) result
  (** Borrow a connection for the duration of [f].

      {b Do not call this inside {!transaction}.} It borrows a {e second} connection, and
      statements on it run outside the transaction — invisibly, since both handles have
      the same type. Inside a transaction body, use the handle the body was given. *)

  val transaction :
    ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
    ?retry:int ->
    t ->
    (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) ->
    ('a, Sqlkit.Error.t) result
  (** Borrow a connection and run [f] on it inside a transaction. See
      {!Sqlkit.transaction} for commit and rollback behaviour. *)
end
