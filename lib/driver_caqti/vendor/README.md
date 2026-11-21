# Caqti PostgreSQL compatibility driver

Copied from the installed Caqti v3.0.0 release (`caqti-driver-postgresql`):
`caqti_driver_postgresql.ml` and `postgresql_conv.ml`.
Upstream: https://github.com/paurkedal/ocaml-caqti/tree/v3.0.0/caqti-driver-postgresql
The upstream copyright and LGPL-3.0-or-later with LGPL linking exception notices
are retained in each source file. See the accompanying license files.

Local changes are intentionally limited:

- Use `Postgresql.connection ~startonly:true` so connection establishment yields
  to Eio instead of blocking its domain before the connect timer can run.
- Finish incomplete connections on errors/cancellation during initial setup.
- Enable nonblocking I/O and poll buffered output while consuming input, so
  large uploads yield to query deadlines as well as result reads.
- Register only `sqlkit-postgresql`, then normalize that URI scheme for libpq.
  Other Caqti users in the application retain their original driver.

The parent adapter still owns result conversion, query deadlines, disposal,
server limits, and pooling. Remove this compatibility copy once an upstream
release supplies asynchronous startup and cancellation-safe initial cleanup.
The production suite includes a TCP listener that never completes PostgreSQL's
handshake to test this exact dependency boundary.
