(** PostgreSQL driver written in OCaml, over Eio, with connection pooling.

    Speaks the wire protocol directly, so there is no libpq underneath: name resolution,
    the cancel request and the TLS handshake all run as fibers, and a result is decoded
    straight from the socket buffer.

    Requires PostgreSQL 15 or later; an older server is refused when connecting.

    Supported: TCP and Unix-domain sockets; trust, password, MD5 and SCRAM-SHA-256
    authentication (without channel binding); TLS through [sslmode] [prefer], [require],
    [verify-ca] and [verify-full], with the system trust store or [sslrootcert];
    [require_auth]; prepared statements cached per connection, prepared in the same write
    as their first execution; [:copy]; every diagnostic field; server-side cancellation.

    Not supported, and reported as an error rather than ignored: several hosts in one
    connection string, GSSAPI and SSPI, client certificates, [.pgpass] and service files,
    SCRAM channel binding, and the [PG*] environment variables that ask for any of these.
    Passwords are sent as given, without SASLprep, which only matters for non-ASCII
    passwords. Use [sqlkit-eio] where one of these is required.

    {2 Connection poolers}

    Prepared statements are named after a digest of their SQL, so a backend shared with
    other clients of this driver holds the same statement under the same name. That keeps
    results correct behind PgBouncer, but each mode has its terms:

    - session pooling: works as a direct connection does.
    - transaction pooling with PgBouncer 1.21 or later and [max_prepared_statements] set:
      works. After a table changes shape, a cached [SELECT *] keeps failing with [0A000]
      until the client reconnects, because PgBouncer reuses the server's plan for
      identical text; and [DEALLOCATE ALL] or [DISCARD ALL] costs the connection.
    - transaction pooling without that support: pass [~statement_cache:false]. Every
      statement is then parsed afresh as the unnamed statement, still in one round trip.
      With the cache left on, statements fail inside transactions whenever a backend does
      not yet hold, or already holds, the name.

    Behind any pooler, cancelling a statement stops it on the server but the next
    connection may be given the same backend, and session state is shared more widely than
    the pool's own borrowers.

    {2 What the defaults do and do not protect}

    - [sslmode] defaults to [prefer], as in libpq: TLS when the server offers it, with no
      check of who the server is. Someone on the network path can remove the encryption or
      stand in for the server. Only [verify-full] (or [verify-ca] with your own root)
      identifies the server.
    - The server chooses the authentication method. Unless [require_auth] says otherwise
      the driver goes along with MD5, SCRAM or none at all; it sends the password itself
      only over TLS or a Unix socket. On an unverified connection a stand-in server can
      therefore still collect an MD5 response or, over unverified TLS, the password. Set
      [require_auth=scram-sha-256] to refuse everything but SCRAM, which also makes the
      server prove it knows the password.
    - There is no channel binding, so SCRAM over unverified TLS does not detect an
      intermediary that relays the exchange.
    - Session state survives from one pool borrower to the next: settings changed with
      [SET], temporary tables, advisory locks. Transactions do not; a connection returned
      with one open is discarded.

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
  ?max_result_bytes:int ->
  ?statement_cache:bool ->
  ?observe:Sqlkit.Telemetry.observer ->
  string ->
  (Sqlkit.conn, Sqlkit.Error.t) result
(** A single connection, closed when the switch finishes. Prefer {!Pool}. Client timeouts
    default to 10 seconds for connection establishment and 35 seconds per query operation.
    Handles must be used sequentially, from the domain that opened them.

    [connect_timeout] in the connection string can shorten the deadline but not extend it.
    [max_result_bytes] (default 256 MiB) bounds everything the server sends in reply to
    one statement; beyond it the statement fails and the connection is dropped, so a
    server cannot grow the process without limit. One handle serves one fiber at a time; a
    second concurrent use is refused.

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
    ?max_result_bytes:int ->
    ?statement_cache:bool ->
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
