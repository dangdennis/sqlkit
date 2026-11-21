# sqlkit

Typed OCaml from your SQL, checked against PostgreSQL.

You write `.sql` files. sqlkit asks PostgreSQL for the parameter and result types
of each query, then emits an OCaml module. No query DSL, no ORM, no hand-written
codecs.

## Example

`sql/users.sql`:

```sql
-- name: GetUser :one
-- Fetch a single user by id.
SELECT id, email, display_name, status, created_at
FROM users
WHERE id = :id;

-- name: SearchUsers :many
SELECT id, email FROM users
WHERE organization_id = :organization_id AND email ILIKE :pattern
LIMIT :limit;

-- name: DeleteUser :exec
DELETE FROM users WHERE id = :id;
```

```
$ sqlkit generate -q sql -o lib/db
wrote lib/db/db.mli and lib/db/db.ml (3 queries from 1 file(s))
```

The generated interface:

```ocaml
type public_user_status = Active | Banned

type get_user_row =
  { id : Uuidm.t option
  ; email : string option
  ; display_name : string option
  ; status : public_user_status option
  ; created_at : Ptime.t option
  }

val get_user : Sqlkit.conn -> id:Uuidm.t -> (get_user_row option, Sqlkit.Error.t) result
val get_user_exn : Sqlkit.conn -> id:Uuidm.t -> get_user_row option
```

And using it:

```ocaml
open Db

let () =
  match get_user conn ~id with
  | Ok (Some u) -> print_endline (Option.value u.email ~default:"(no email)")
  | Ok None -> print_endline "not found"
  | Error e -> prerr_endline (Sqlkit.Error.to_string e)
```

Result fields are optional by default: catalog constraints alone cannot prove
query nullability. Enum values use schema-qualified generated variants.

## Install

```
opam install sqlkit sqlkit-postgresql
```

`sqlkit` is the runtime that generated code depends on. `sqlkit-postgresql`
provides the `sqlkit` command and a libpq driver. Add `sqlkit-caqti` for
connection pooling.

Requires OCaml 5.5.0 or later, for modular explicits.

## Commands

```
sqlkit generate -q sql -o lib/db      write db.ml and db.mli
sqlkit check    -q sql -o lib/db      verify they match the database; exit 1 if not
sqlkit describe -q sql                print what PostgreSQL says about each query
sqlkit snapshot -q sql                cache the database's answers for --offline
```

Connection comes from `DATABASE_URL`, or `PGHOST`/`PGPORT`/`PGUSER`/
`PGDATABASE`/`PGPASSWORD`, or `--database`.

`-q` defaults to `sql` and is searched recursively. Every query becomes a nested
module inside one generated module, whichever file it came from, so row types
can be shared across files. Query names must therefore be unique project-wide;
a collision is reported with both locations.

Run `check` in CI. Generated code keeps compiling after the schema moves under
it and only fails at runtime; `check` is what catches that.

```
(rule
 (alias runtest)
 (deps (glob_files_rec sql/*.sql) db.ml db.mli)
 (action (run sqlkit check -q sql -o . -m db)))
```

### Offline mode

`sqlkit snapshot` describes every query against a live database and writes
`sqlkit.snapshot.json` beside the queries. Commit it; `generate --offline` and
`check --offline` then need no database at all.

Staleness is loud, never silent: each entry is keyed on a hash of every SQL
variant the query can execute, and the schema files listed in `sqlkit.toml`
(`schema = ["../schema.sql"]`) are hashed as a whole, so editing a query or a
listed schema file makes `--offline` fail with the query's name and "re-run
`sqlkit snapshot`". The one blind spot is schema drift *outside* the listed
files — a migration applied to the database but never written down. Offline
mode cannot see that by construction; keep one live `check` in CI as the
backstop.

## Annotations

| Syntax | Meaning |
| --- | --- |
| `-- name: GetUser :one` | at most one row; returns `option` |
| `-- name: GetUser :one!` | exactly one row; returned unwrapped, absence is an error |
| `-- name: ListUsers :many` | zero or more rows; returns `list` |
| `-- name: DeleteUser :exec` | no rows; returns `Some affected_count`, or `None` when unavailable |
| `-- name: BulkAddUsers :copy` | bulk insert via `COPY FROM STDIN`; takes a row list |
| `:id` | named parameter, becomes a labelled argument |
| `:name?` | nullable parameter, becomes an optional argument |
| `AS "total!"` | force a column non-null |
| `AS "title?"` | force a column nullable |

Cardinality is enforced by the type system: passing a `:one` query to
`fetch_all` does not compile.

A `:copy` query must be exactly `INSERT INTO t (col, ...) VALUES (:param, ...)`
— the INSERT is what Describe verifies the types of, and the generator builds
the `COPY t (col, ...) FROM STDIN` statement from that verified column list.
The generated function takes the whole row list and loads it in one round-trip;
any bad row aborts the entire load server-side. COPY runs on the libpq driver;
the Caqti driver returns a clear error pointing there.

Every result column defaults to `option`, including columns declared `NOT NULL`: an
outer join can still produce NULL. `AS "total!"` is an explicit user assertion; a
NULL then becomes a decode error. `?` explicitly retains nullable results.
Parameter nullability remains controlled by the placeholder syntax.

## Types

| PostgreSQL | OCaml |
| --- | --- |
| `bool` | `bool` |
| `int2`, `int4` | `int` |
| `int8` | `int64` |
| `float4`, `float8` | `float` |
| `numeric` | `Decimal.t` |
| `text`, `varchar`, `char` | `string` |
| `bytea` | `string` |
| `uuid` | `Uuidm.t` |
| `timestamp`, `timestamptz` | `Ptime.t` |
| `json`, `jsonb` | `Yojson.Safe.t` |
| enum types | a schema-prefixed variant |
| `T[]` | `T Sqlkit.Pg_array.t` |
| domains | underlying type, or custom mapping |
| named composites | schema-prefixed records with optional fields |
| ranges | `T Sqlkit.Range.t` |
| multiranges | `T Sqlkit.Range.t list` |
| `date` | `Ptime.date` |
| `time` | `Ptime.Span.t` (since midnight) |
| `interval` | `Sqlkit.Interval.t` |
| `timetz`, `inet` | `string` |

An unmapped type is an error naming the column, not a silent fall back to
`string`.

Arrays give you dynamic IN lists:

```sql
-- name: GetUsersByIds :many
SELECT id, email FROM users WHERE id = ANY(:ids);
```

```ocaml
val get_users_by_ids : Sqlkit.conn -> ids:Uuidm.t Sqlkit.Pg_array.t -> (get_users_by_ids_row list, _) result
```

Use `Sqlkit.Pg_array.of_list ids` for a conventional list. Arrays preserve NULL
elements, dimensions, and lower bounds; `to_list` rejects values that cannot be
represented faithfully as a plain list. Range bounds distinguish empty,
unbounded, inclusive, and exclusive values; PostgreSQL performs canonicalization.
Named composites can nest these containers. Anonymous `record` results require a
cast to a named composite type.

`timestamptz` decodes to the instant it names: output carries the session's
offset, which the parser honours, so any server `TimeZone` round-trips
correctly. `timestamp` (without time zone) has no zone to honour; it is read
and written as UTC wall-clock time by convention — prefer `timestamptz`. Both
drivers pin `datestyle = ISO` per connection, so a server configured with
`German` or `SQL` output styles cannot poison temporal decoding.

`Sqlkit.Interval.t` is a private record with readable `months`, `days`, and
`micros` fields. Use `Interval.make` (result), `make_opt`, or `make_exn` to
construct one. Months and days fit signed 32-bit integers; time fits signed
64-bit microseconds. Constructors check intermediate arithmetic and reject
PostgreSQL's reserved infinity representations.

The three components keep their independent signs: a month has no fixed length
and a day is not always 24 hours. Parsing accepts all four PostgreSQL
`IntervalStyle` output formats and preserves microseconds exactly. Fractional
seconds beyond microsecond precision are accepted only when the extra digits
are zero. Infinity is not represented.

```ocaml
let duration = Sqlkit.Interval.make ~months:1 ~days:(-2) ~hours:3 ()
(* (Sqlkit.Interval.t, Sqlkit.Codec.error) result *)
```

Note that `jsonb` is a normalised representation: PostgreSQL reorders object
keys and drops duplicates, so a round-trip preserves the value, not the text.
Use `json` if you need the text preserved exactly.

## Configuration

An optional `sqlkit.toml` beside your queries directory controls the two things
that cannot be inferred: what generated names are called, and which OCaml type
a column maps to.

```toml
[rename]
users = "user"                  # users_row becomes user_row
"users.display_name" = "name"   # the field becomes `name`

[types."users.id"]              # one column
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"

[types.citext]                  # or a whole Postgres type
ocaml = "Email.t"
of_string = "Email.of_string"
to_string = "Email.to_string"
```

Table names are not singularized automatically. English pluralization is a
swamp — `data`, `series`, `status`, `people` — so an explicit rename is longer
and always right.

Parameters are keyed on the query rather than the column, because PostgreSQL
reports a parameter's type but not which column it is compared against, so
`users.id` cannot reach the `$1` in `WHERE id = $1`:

```toml
[types."GetUser.id"]
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"
```

## Errors

Failures carry a SQLSTATE, so a handler can act on them:

```ocaml
match create_user conn ~id ~email () with
| Ok _ -> respond `Created
| Error e when Sqlkit.Error.is_unique_violation e ->
  respond (`Conflict (Option.value (Sqlkit.Error.constraint_name e) ~default:"duplicate"))
| Error e when Sqlkit.Error.is_retryable e -> retry ()
| Error e -> log (Sqlkit.Error.to_string e); respond `Internal_error
```

`Sqlkit.Sqlstate` covers all 262 PostgreSQL codes across 43 classes, with
`condition` for matching, `name`, `class_`, and predicates including
`is_retryable` (serialization failure or deadlock after successful cleanup).

The libpq driver also reports the constraint name, detail, hint, table and
column. The Caqti driver reports only the message and code, which is all Caqti
exposes.

## Streaming

For results too large to hold as a list, rows are read in batches from a
server-side cursor, so memory is bounded by the batch size:

```ocaml
Sqlkit.fetch_fold (module Db.Search_users) ~batch:1000 conn
  { organization_id; email_pattern = "%"; limit = 1_000_000 }
  ~init:0 ~f:(fun n _row -> n + 1)
```

`fetch_iter` is the same with no accumulator. Streaming uses the exported query
module directly, so parameters are passed as the record rather than labelled
arguments. Works through both drivers and inside an enclosing transaction.

## Optional filters

A `/*? ... */` block is included only when its parameter is supplied:

```sql
-- name: FindUsers :many
SELECT id, email, status FROM users
WHERE organization_id = :org
  /*? AND email ILIKE :email */
  /*? AND status = :status */
ORDER BY email
LIMIT :limit;
```

```ocaml
val find_users :
  Sqlkit.conn -> org:Uuidm.t -> limit:int64 ->
  ?email:string -> ?status:public_user_status -> unit ->
  (find_users_row list, Sqlkit.Error.t) result
```

Every inclusion combination is generated as its own statement and verified
against the database at codegen, so all of them are type-checked, each plans
with only its live predicates, and each gets its own prepared-statement cache
entry. Blocks may appear anywhere in the query — subqueries and CTEs included —
because each combination is validated whole. Blocks must not change the result
shape, and at most 4 blocks are allowed per query (2^n combinations are
verified).

A parameter inside a block must belong to that block alone. A block with
several parameters is included when all of them are supplied; supplying only
some returns `Error.Encode` before executing SQL.

## Result and exception APIs

Plain fallible functions return `result`. `_opt` codec adapters discard error details;
`_exn` adapters raise. Total functions such as `Pg_array.of_list` return values directly.
OCaml identifiers cannot end in `!`; `_exn` is the exception marker. SQL `!` annotations
remain assertions about nullability or row cardinality, and do not select an exception API.

```ocaml
Sqlkit.Pg_array.of_string Sqlkit.Row.Elem.int "{1,NULL,3}"
(* Ok array, preserving the NULL element *)
Sqlkit.Pg_array.of_string_opt Sqlkit.Row.Elem.int "malformed"
(* None *)
Sqlkit.Pg_array.of_string_exn Sqlkit.Row.Elem.int "malformed"
(* raises Sqlkit.Codec.Codec_error *)
```

Custom `of_string` functions return `(t, Sqlkit.Codec.error) result`; custom
`to_string` functions return `(string, Sqlkit.Codec.error) result`. Container
callbacks follow those contracts. Invalid input is an `Error`, not an exception.
To adapt an existing string diagnostic, use `Result.map_error Sqlkit.Codec.custom`.

Codec errors contain a typed `reason` and an ordered `path`. Reasons distinguish
invalid syntax/values, NULL, type/shape mismatches, enum labels, inconsistent
parameters, NUL text, and custom failures. Generated paths include column and
parameter names, composite fields, array subscripts, range bounds, and COPY rows.
Array subscripts follow PostgreSQL, including custom lower bounds; other indexes
are zero-based. Syntax offsets refer to bytes in the parsed containing value,
after any outer whitespace trimming.

`Error.Encode { query; error }` and `Error.Decode { query; error }` retain that
structure. Inspect it when useful, or handle every error with `Sqlkit.Error.pp`:

```ocaml
let report error =
  match Sqlkit.Error.codec_error error with
  | Some { reason = Sqlkit.Codec.Unknown_enum { type_name; value; _ }; _ } ->
      Printf.eprintf "Unknown %s label: %S\n" type_name value
  | _ -> Format.eprintf "%a@." Sqlkit.Error.pp error
```

Use `Sqlkit.Codec.pp` for standalone codec failures. Human-readable wording is not
a machine-readable contract. Generated query `sql` and `encode` return codec
results; `decode` returns `Sqlkit.Row.error`, an alias of `Sqlkit.Codec.error`.

Migration: regenerate query modules, adapt custom string errors with
`Codec.custom`, and replace matches on `Row.error`'s former `column/expected/got`
fields or `Error.Encode.message` with reason/path inspection or generic formatting.

Query absence stays distinct from failure: `:one` returns `(row option, Error.t) result`.
`Ok None` means no row; a database failure never becomes absence. Generated `_exn`
functions derive from their result counterpart using `Sqlkit.get_exn`.

Unexpected ordinary exceptions from query codecs and transaction/streaming callbacks
are translated at the execution boundary into `Error.Callback`. Runtime cancellation,
`Out_of_memory`, `Stack_overflow`, and `Sys.Break` propagate after scoped cleanup.
The core does not use exceptions for parsing, validation, or error propagation.

## Transactions

```ocaml
Sqlkit.transaction ~isolation:`Serializable ~retry:3 conn (fun tx ->
  let* balance = get_balance tx ~id in
  set_balance tx ~id ~balance:(debit balance amount))
```

Commits when the body returns `Ok`, and rolls back on `Error` or an exception.
A raised `Sql_error` becomes its original error; other ordinary callback exceptions
become `Error.Callback`. Cancellation propagates after cleanup. Generated functions
take the transaction handle unchanged.

Nesting uses savepoints, so an inner failure rolls back only the inner work.
`~retry` re-runs the body on a serialization failure or deadlock — the standard
companion to `` `Serializable`` — and on nothing else, since other errors would
fail identically again. The body must be safe to re-run.

## Drivers

`sqlkit-postgresql` gives you a single connection over libpq. Each distinct
query is prepared once per connection and executed by name afterwards, so the
server plans it once, not per call:

```ocaml
let conn = Result.get_ok (Sqlkit_pg.connect (Sqlkit_pg.conninfo_of_env ()))
```

`sqlkit-caqti` gives you a pool over Caqti and Eio. Eio is direct style, so
generated signatures do not change:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let pool =
  Result.get_ok
    (Sqlkit_caqti.Pool.create ~sw ~stdenv:(env :> Caqti_eio.stdenv) ~max_size:10
       (Sqlkit_caqti.uri_of_env ()))
in
Sqlkit_caqti.Pool.use pool (fun conn -> get_user conn ~id)
```

A pool exposes no query operations. Reaching a connection requires `Pool.use` or
`Pool.transaction`, both of which scope it.

## Formatting

Generated code is run through `ocamlformat` using whatever `.ocamlformat`
applies to the output directory, so it arrives in your project's own style.

This is not cosmetic. `check` compares byte-for-byte, so without it an editor
that formats on save would make `check` fail forever on code you did not write.
Formatting in the generator means `generate` and `check` agree by construction.
A missing or mismatched `ocamlformat` is an error.

## Development

```
docker compose up -d     # PostgreSQL 18 on 127.0.0.1:55432
dune build               # dune package management: fetches deps from dune.lock
dune test                # unit tests, no database
DATABASE_URL=postgresql://sqlkit:sqlkit@127.0.0.1:55432/sqlkit dune exec example/e2e.exe
```

`example/` holds a worked schema, queries, generated output, and three
programs: `app` on a single connection, `web` on a pool, `e2e` covering the
type mappings.

### Compiler checks and migration

See [the inference suite](integration/README.md) for the 1,000-case PR gate,
10,000-case extended gate, compiled decoders, and pinned pGenie comparison.

The result-contract migration changes custom codecs, `Query.sql`/`encode`/`decode`,
container parsers/printers, `Row` decoders, and `Interval.of_string`. Use `_exn` only
at an explicitly exception-based call site; otherwise bind the result. `or_raise`
is now `get_exn`, and `close` returns a result (`close_exn` is the raising adapter).
Third-party drivers must implement cancellation classification and protected cleanup.

`Interval.make` now returns a result. Replace direct interval record construction
or updates with a validated constructor; field reads still work. Use `make_exn`
for trusted constants at explicitly exception-based call sites, or `make_opt`
when the reason for failure is unnecessary.

Regenerate snapshots and generated code after upgrading: version 1 snapshots are
rejected. Result fields become optional, `int8` parameters/results become `int64`,
and array APIs use `Sqlkit.Pg_array.t`. Named database types and shared row models
use schema-prefixed names unless renamed. Qualified configuration keys win;
unqualified shorthands must be unambiguous. Exact column/parameter or type
mappings replace the entire value; otherwise component mappings apply recursively.

## Limitations

- PostgreSQL only. The driver boundary anticipates SQLite, but SQLite has no
  equivalent of Describe, so inference there needs a different approach.
- Initial inference needs a live database. A current snapshot supports offline
  codegen; generated code does not need the generator.
- One statement per named query; no dynamic query building beyond `= ANY(...)`.
- Anonymous record-valued results are rejected; cast to a named composite.
- Scalar codec limits also apply inside containers (for example, `Ptime.t` cannot
  represent PostgreSQL timestamp infinity). Custom mappings can supply alternatives.
- The Caqti driver reports SQLSTATE but not the constraint name, detail or
  hint; libpq reports all of them.

## Compatibility

Until **1.0.0**, APIs may change incompatibly, including within 0.1.x. Breaking
changes will be documented with migration guidance. The 0.1 API review establishes
the current design; long-term compatibility commitments begin at 1.0.0.

## Roadmap

See [ROADMAP.md](ROADMAP.md). Next: review the 0.1 public APIs, then exercise them
in a nontrivial web application. Breaking changes remain allowed until 1.0.0.

## Prior art

[sqlc](https://sqlc.dev) for the authoring model, [sqlgg](https://ygrek.org/p/sqlgg/)
as the closest OCaml ancestor, [PG'OCaml](https://github.com/darioteixeira/pgocaml)
for the idea of typing queries against a live database, and
[Caqti](https://github.com/paurkedal/ocaml-caqti) underneath the pooled driver.

## License

MIT

### Operational limits and telemetry

Both drivers apply `Sqlkit.Limits.default` to new sessions: statement timeout
30 seconds, lock timeout 5 seconds, idle transaction timeout 60 seconds. Set a
server timeout to zero to disable it, or pass `Sqlkit.Limits.unlimited`.

```ocaml
let observe (event : Sqlkit.Telemetry.event) =
  (* Enqueue in your metrics/logging system; keep this callback quick. *)
  Printf.eprintf "%s %.3fs\n" event.name event.duration_s
in
let pool =
  Sqlkit_caqti.Pool.create ~sw ~stdenv ~max_size:10
    ~acquire_timeout:5. ~connect_timeout:10. ~query_timeout:35.
    ~limits:Sqlkit.Limits.default ~observe (Sqlkit_caqti.uri_of_env ())
in
(* Pool.create and query functions return results; use Sqlkit.get_exn when desired. *)
pool
```

Those are the pool defaults; client deadlines are positive, finite seconds.
Acquisition includes opening/configuring a new connection. Its deadline ends
before your callback begins. The query deadline covers each driver operation,
including each streaming batch, rather than the whole request or transaction.
The pool is lazy, has a fixed maximum size, and belongs to one Eio domain/switch.
Use separate borrows for concurrent requests; use a connection sequentially.
Borrowed handles become unusable when the callback returns.

Client timeouts return `Error.Timeout` with an acquisition, connection, or query
phase. Server timeouts retain PostgreSQL's SQLSTATE (`57014`/`55P03`). Active
client cancellation discards the connection; the next borrow opens a fresh one.
Failed rollback returns `Error.Cleanup { primary; cleanup }` and also discards it.
Transport errors are never automatically replayed. In particular, connection
loss during COMMIT leaves the outcome uncertain; resolve that through your
application's idempotency key or durable state before retrying.

Events contain query names, monotonic elapsed seconds, outcomes, and SQLSTATE
when available. They include pool waits and rollback cleanup, with no SQL,
parameters, row values, or error messages. Ordinary observer exceptions are
ignored. Cancellation remains an exception after cleanup.

`Sqlkit_pg.connect` accepts `~limits`, `~observe`, and integer
`~connect_timeout` (default 10 seconds per host). Its blocking libpq execution
has server-side limits but **no client query deadline**; use the Eio driver for
web request deadlines. Caqti transport errors may have no SQLSTATE when its
underlying driver does not expose one.

Migration: generated `:exec` results now contain `int option`. `Some 0` is a
real zero-row result; `None` means no count was supplied. Both drivers return
actual DML counts. Regenerate code and update comparisons/pattern matches.

The Caqti adapter currently includes a compatibility copy of Caqti 3.0's
PostgreSQL backend for asynchronous connection startup and cleanup. Caqti is
pinned to 3.0 while that patch uses its private interfaces. See
[compatibility notes](lib/driver_caqti/vendor/README.md).
