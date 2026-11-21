(** PostgreSQL driver written in OCaml, over Eio, with connection pooling.

    Speaks the wire protocol directly, so there is no libpq underneath: name resolution,
    the cancel request and the TLS handshake all run as fibers, and a result is decoded
    straight from the socket buffer.

    Supported: TCP and Unix-domain sockets; trust, password, MD5 and SCRAM-SHA-256
    authentication (without channel binding); TLS through [sslmode] [prefer], [require],
    [verify-ca] and [verify-full], with the system trust store or [sslrootcert]; prepared
    statements cached per connection, prepared in the same write as their first execution;
    [:copy]; every diagnostic field; server-side cancellation.

    Not supported, and reported as an error rather than ignored: several hosts in one
    connection string, GSSAPI and SSPI, client certificates, [.pgpass] and service files,
    SCRAM channel binding. Passwords are sent as given, without SASLprep, which only
    matters for non-ASCII passwords. Use [sqlkit-eio] where one of these is required.

    {[
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let pool =
      Result.get_ok
        (Sqlkit_pgwire.Pool.create ~sw ~net:env#net ~clock:env#mono_clock
           (Sqlkit_pgwire.conninfo_of_env ()))
    in
    Sqlkit_pgwire.Pool.use pool (fun conn -> Db.get_user conn ~id)
    ]} *)

val conninfo_of_env : unit -> string
(** Connection string from [DATABASE_URL], or from
    [PGHOST]/[PGPORT]/[PGUSER]/[PGDATABASE]/[PGPASSWORD]. *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.Mono.t ->
  ?limits:Sqlkit.Limits.t ->
  ?connect_timeout:float ->
  ?query_timeout:float ->
  ?observe:Sqlkit.Telemetry.observer ->
  string ->
  (Sqlkit.conn, Sqlkit.Error.t) result
(** A single connection, closed when the switch finishes. Prefer {!Pool}. Client timeouts
    default to 10 seconds for connection establishment and 35 seconds per query operation.
    Handles must be used sequentially, from the domain that opened them.

    [sslmode] defaults to [prefer], as in libpq: TLS when the server offers it, without
    verifying the certificate. Ask for [verify-full] on any network you do not control.

    A statement that is cancelled or exceeds [query_timeout] costs the connection: the
    server is asked to stop and the handle becomes unusable. *)

module Pool : sig
  type t

  val create :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    clock:_ Eio.Time.Mono.t ->
    ?max_size:int ->
    ?acquire_timeout:float ->
    ?connect_timeout:float ->
    ?query_timeout:float ->
    ?limits:Sqlkit.Limits.t ->
    ?observe:Sqlkit.Telemetry.observer ->
    string ->
    (t, Sqlkit.Error.t) result
  (** Lazy pool, confined to one Eio domain and switch. Same defaults and behaviour as
      [Sqlkit_eio.Pool.create]. *)

  val use :
    t -> (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) -> ('a, Sqlkit.Error.t) result
  (** Borrow a connection for the duration of [f]. Do not call this inside {!transaction}:
      it borrows a second connection. *)

  val transaction :
    ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
    ?retry:int ->
    t ->
    (Sqlkit.conn -> ('a, Sqlkit.Error.t) result) ->
    ('a, Sqlkit.Error.t) result
end
