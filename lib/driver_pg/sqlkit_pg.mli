(** PostgreSQL driver built directly on libpq.

    A single connection, no pooling. Suited to CLI tools, scripts and tests; a web
    application wants [sqlkit-caqti] instead.

    Both drivers report affected-row counts when PostgreSQL supplies them.
    [connect_timeout] defaults to 10 seconds per host; server limits use
    {!Sqlkit.Limits.default}. This blocking driver has no client query deadline. *)

val conninfo_of_env : unit -> string
(** Connection string from [DATABASE_URL], or from
    [PGHOST]/[PGPORT]/[PGUSER]/[PGDATABASE]/[PGPASSWORD]. *)

val connect :
  ?limits:Sqlkit.Limits.t ->
  ?connect_timeout:int ->
  ?observe:Sqlkit.Telemetry.observer ->
  string ->
  (Sqlkit.conn, Sqlkit.Error.t) result
(** Open a connection. Not closed automatically; use {!with_connection} when the lifetime
    is scoped. *)

val with_connection :
  ?limits:Sqlkit.Limits.t ->
  ?connect_timeout:int ->
  ?observe:Sqlkit.Telemetry.observer ->
  string ->
  (Sqlkit.conn -> 'a) ->
  ('a, Sqlkit.Error.t) result
(** [with_connection conninfo f] opens a connection, runs [f], and closes it. *)
