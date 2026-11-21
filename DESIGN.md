# sqlkit — design

SQL-first query compiler for OCaml. You write `.sql` files; `sqlkit` asks
PostgreSQL what the types are and emits typed OCaml query modules; a small
runtime executes them.

Lineage: **sqlc** for the authoring model, **sqlgg** as closest OCaml prior art,
**Caqti** as the intended execution substrate (hidden), **modular explicits**
for the execution API.

## Status

Working end to end on stock OCaml 5.5.0: runtime, generator, CLI
(`generate`/`check`/`describe`), libpq driver, Caqti/Eio driver with pooling,
transactions with isolation/savepoints/retry, streaming, SQLSTATE
classification, `sqlkit.toml` renames and custom types, ocamlformat-clean
output. The compiler now uses a transitive PostgreSQL type graph; see ROADMAP.md.

Tests: `dune test` (unit: runtime, generated output, sqlstate table) plus
`example/e2e.exe` (libpq against PostgreSQL 18), `example/app.exe` and
`example/web.exe` (pooled Caqti), and `sqlkit check` as its own regression.

## Toolchain

OCaml 5.5.0 or later. Modular explicits landed upstream in 5.5.0 as
module-dependent functions, so no compiler fork is needed:

```bash
opam switch create sqlkit ocaml-base-compiler.5.5.0
```

Earlier revisions of this project were built on `samsa1/ocaml`'s
`5.3.0+modular-explicit` branch, which used a brace binder (`{M : S}`).
Upstream landed with `(module M : S)` instead, which has the pleasant property
that the *term* syntax is ordinary OCaml -- `let f (module M : S) x` and
`f (module Foo) x` were already valid -- and only the *type*
`(module M : S) -> t[M]` is new, being dependent on the module argument.

## Development database

```bash
docker compose up -d     # Postgres 18 on 127.0.0.1:55432, schema auto-applied
```

`example/schema.sql` is mounted into `/docker-entrypoint-initdb.d`, so it runs
on first boot only. After editing it: `docker compose down -v && docker compose up -d`.

Two gotchas, both hit on the way in:

- **Postgres 18 changed the volume convention.** Data lives in a
  major-version subdirectory, so the volume mounts at `/var/lib/postgresql`,
  not `/var/lib/postgresql/data`. Mounting the old path makes the image refuse
  to start with a `pg_ctlcluster` compatibility error.
- Port 55432, not 5432, to stay clear of any local install.

## Why modular explicits here

The execution API:

```ocaml
val fetch_one : (module Q : Query.ONE)  -> conn -> Q.params -> (Q.row option, Error.t) result
val fetch_all : (module Q : Query.MANY) -> conn -> Q.params -> (Q.row list,   Error.t) result
val exec      : (module Q : Query.EXEC) -> conn -> Q.params -> (int,          Error.t) result
```

The module argument determines both the parameter type you must supply and the
result type you get. With ordinary first-class modules this isn't writable in
curried form — `Q.params` and `Q.row` would each have to escape as a type
variable threaded through a `with type params = 'p and type row = 'r` witness
that every caller reconstructs.

**Explicits, not implicits.** The usual objection to explicits is call-site
noise: you write `(module Get_user)` every time. But in an sqlc-style tool a *program*
writes those call sites. The verbosity is paid by the generator and the
ergonomic cost to the user is zero — so the inference machinery of implicits
buys nothing, at the cost of nothing.

**Where explicits are *not* used:** the driver. A connection is an ordinary
value you store in records and pass around, so it's an existential GADT over a
first-class module (`Driver.t`), not a module-dependent argument. Explicits are for the
thing whose types must project into a signature; existentials are for the thing
that must stay a plain value. Using explicits for both would force every call
site to name its driver for no benefit.

## Layering

```
.sql files
   ↓  generator: discover → parse (-- name: X :one) → PG Parse/Describe → resolve → render
generated query modules  (match Query.ONE / MANY / EXEC)
   ↓  Sqlkit.fetch_one (module Q) / fetch_all (module Q) / exec (module Q)
Driver.S  ←— the backend boundary; Caqti lives strictly below it
   ↓
postgres / sqlite
```

`lib/sqlkit` has **no dependency on Caqti or Eio**. That's deliberate: Caqti's
tuple codecs and first-class connection modules must never reach a generated
signature, so Caqti can be swapped for a native wire driver without touching a
line of generated application code.

Values crossing the driver boundary are `Value.t` (`Null | Bool | Int | Float |
Text | Octets`). Richer Postgres types — uuid, timestamptz, json, arrays, enums
— ride as `Text`/`Octets` and are converted in generated code via `Row`, so
adding a type mapping never requires a driver change. This also sidesteps
Caqti's tuple-arity ceiling: rows decode positionally, so a 30-column SELECT is
no harder than a 3-column one.

## Two constraints the first build surfaced

Both are now encoded in the passing test, and both are the generator's problem:

1. **Record field collisions.** `params` and `row` in one module routinely share
   field names (`id`, `email`). OCaml resolves an unannotated `{ id }` pattern
   to the *last* type defined, so the generator must emit `let encode ({ id } :
   params) = ...` and `let decode r : row = ...` — annotations, always.

2. **Warning 69 fires in generated code.** A row record's fields are written by
   the decoder but often only some are read by the caller, which trips
   `unused-field` in the *defining* module — i.e. in generated code, for a
   reason the user cannot fix. Generated files must carry `[@@@warning "-69"]`.

## The generated API

Decided by working through three compiling variants in `example/` (see git
history for the alternatives).

**Shape C.** Row types at the top level, query modules also exported. One
`open Db` per file puts every row's fields in scope, so `u.email` works and
resolves by type-directed disambiguation even when several row types share a
field name. Exporting the query module keeps `Sqlkit.fetch_one (module Db.Get_user)`
available for tooling that wants to be generic over queries.

**Labelled arguments** on the wrappers: `Db.get_user conn ~id`. Names come
straight from the `:id` in the SQL.

**`result` by default, `_exn` variant alongside**: `get_user` returns
`(row option, Error.t) result`; `get_user_exn` raises `Sqlkit.Sql_error`. Follows
the usual OCaml convention, and keeps the unsuffixed name total.

**Real types**: `uuid → Uuidm.t`, `timestamptz → Ptime.t`, `numeric →
Decimal.t`. Postgres prints timestamps as `2026-07-28 09:00:00+00` — a space
instead of RFC3339's `T`, and a 2-digit offset — so `Row.ptime` normalises
before handing to `Ptime.of_rfc3339`.

**Shared model types.** When a result's columns are exactly one table's columns
— every column carries the same non-zero `tableoid` from `RowDescription`, and
together they cover the table — emit one shared `<table>_row` and reuse it
across every such query. Otherwise the query keeps its own type. Without this,
two `SELECT *` queries produce field-for-field identical but *nominally
distinct* records, and no helper can be shared between them. (The wide-table
exploration that settled this lived in `example/wide.mli`; retired once the
generator produced the real thing — see git history.)

**Nullable parameters are optional arguments**, sorted after the mandatory ones
so the mandatory arguments stay in SQL order. A query with any nullable
parameter ends in `()`. Trades a trailing unit for dropping a column of
explicit `None`s on wide inserts.

Two mli details the generator must get right, both found by compiling:

1. `include Sqlkit.Query.ONE with type row := t` is a *destructive* substitution
   and deletes `type row` from the signature, so the module stops matching
   `ONE`. It must be `type row = t`. (`params :=` is correct, because `params`
   is declared just above the include.)
2. `params` and `row` routinely share field names, and OCaml resolves an
   unannotated `{ id }` to the *last*-defined type — so every generated
   `encode` needs `({ id } : params)` and every `decode` needs `: row`.

## Keeping generated code honest

Generated code keeps compiling after the schema changes under it — a dropped
column, a widened type, a new enum label, a column that becomes NOT NULL — and
only fails at runtime. Every type guarantee here is really "as of the last
`sqlkit generate`". `sqlkit check` is what converts that into "as of now":

```bash
sqlkit generate -q src/sql -o src/db    # write
sqlkit check    -q src/sql -o src/db    # verify, exit 1 on any difference
```

`check` regenerates in memory through the *same* pipeline as `generate` — if it
took a different route it could disagree for reasons unrelated to drift — and
reports the first differing line of each file:

```
sqlkit: src/db/db.mli:14: out of date
  on disk:   ; display_name : string option
  should be: ; display_name : string
```

Run it in CI against a database migrated to the current schema. Wiring it into
a build:

```
(rule
 (alias runtest)
 (deps (glob_files_rec sql/*.sql) db.ml db.mli)
 (action (run sqlkit check -q sql -o . -m db)))
```

## Transactions, and the phantom tag that died twice

```ocaml
val transaction :
  ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
  ?retry:int ->
  conn -> (conn -> ('a, Error.t) result) -> ('a, Error.t) result
```

Commits on `Ok`, rolls back on `Error` or an exception. Ordinary callback exceptions
become errors; cancellation propagates after cleanup. Nesting uses
savepoints, driven by a depth counter on the handle. `~retry` re-runs the body
on `40001`/`40P01` only, at the outermost level only.

The body handle was twice intended to be a distinct phantom-tagged `tx conn`,
and abandoned twice for different reasons — both worth recording.

**First attempt, on the 5.3 fork: not expressible.** The tag would not
generalise through the modular-explicit query functions, and the fork rejected
explicit polymorphic annotations (`'k.` and `type k.`) on any binding whose type
contains a modular-explicit arrow, with "the universal variable would escape its
scope". No way to force it.

**On upstream 5.5.0 that limitation is gone.** Both annotation forms are
accepted, inference generalises the tag, and the real runtime builds with
`'k conn` throughout — verified, not assumed. Polymorphic accumulators
(`fetch_fold`'s `'acc`) likewise infer cleanly. The fork-era claim that
"modular explicits do not compose with explicit polymorphic annotations" is
false upstream.

**Second attempt, on 5.5: expressible, and pointless.** The design had moved
under it. Savepoint nesting made `transaction` legal on a handle already inside
a transaction — so no operation demands a `toplevel conn` any more, which means
the tag distinguishes nothing: it decorates every generated signature with a
type parameter no function constrains. And the hazard it was meant to prevent
became unreachable anyway: the body receives the *same* handle (same depth
ref), so statements on the outer alias still join the transaction, and the one
real remaining hazard — borrowing a second pooled connection mid-transaction —
is a `Pool.use` inside `Pool.transaction`, which no per-handle tag can see.

The general lesson survives in weakened form: a type-level guarantee should be
re-derived from the current design before being re-introduced. The property the
tag encoded stopped being true of the system before the tag became expressible.

## Two drivers

**libpq** (`lib/driver_pg`) — a single connection, no pooling. Fine for CLI
tools, scripts and tests. Returns real affected-row counts via `PQcmdTuples`.

**Caqti over Eio** (`lib/driver_caqti`) — connection pooling and concurrency.
This is what a web app uses.

An earlier revision of this document claimed Caqti could not work because it
sends parameters with explicit type OIDs, so `WHERE uuid_col = $1` would fail
with `operator does not exist: uuid = text`. **That was wrong.** Caqti's
Postgres driver sends parameters with unspecified OIDs and lets the server
infer, exactly as `PQexecParams` with `paramTypes = NULL` does. A `uuid`,
`numeric` or enum parameter round-trips as text with no cast anywhere.

The real obstacle was smaller: Caqti's codecs are static while sqlkit's model is
dynamic. Nesting `t2` existentially bridges them in about forty lines, and
nesting rather than `tup3`/`tup4` means there is no arity ceiling. The one thing
the driver needed from above was the column count, since Caqti must declare the
row shape before executing where libpq discovers it from the result — hence
`Query.ONE`/`MANY` exposing `columns`, which the generator knows anyway.

**Eio is direct style, so no generated signature changed.** A query returns a
plain `result`, not a promise. Choosing Lwt instead would have monadified every
generated function.

`Pool.t` deliberately carries no query operations: the only way to reach a
`Sqlkit.conn` is `Pool.use` or `Pool.transaction`, both scoped. That is the
enforcement the phantom-typed transaction handle could not provide, achieved by
not exposing the operation rather than by type-level machinery.

The generator still uses libpq directly, and always will — `PQftable` /
`PQftablecol` are the only source of nullability and shared-model detection, and
Caqti cannot expose them. Codegen and runtime are different programs with
different needs.

## Open decisions

- **SQLite.** The runtime is engine-neutral, but the generator's type inference
  is PostgreSQL's Parse/Describe; SQLite has no equivalent, so inference there
  needs a different strategy (its own describe over `sqlite3_column_decltype`
  plus the schema, most likely).

Decided since this section was first written: named parameters are rewritten to
`$n` by the parser and become the `params` record fields; result nullability defaults to optional with explicit `!`/`?` alias overrides; and Eio entered
through the Caqti driver only — the runtime stays synchronous and IO-free.

## PostgreSQL type graph and container codecs

`Pg_type` separates database identity from the OCaml representation selected by
`Typemap`. A structured schema/name pair identifies each catalog node; domain,
array, composite, range, and multirange dependencies are references into a
registry. Discovery memoizes OIDs only within the current connection. Visiting
nodes are registered before descending, so named recursion terminates. The
version 2 snapshot stores a deterministic registry and root references, never
OID identities. Column type modifiers remain separate use-site metadata.

Domains retain their base, constraints, and catalog NOT NULL metadata. Their
OCaml representation defaults to the base codec; PostgreSQL validates constraints.
A query may erase a domain to its base type before Describe reports it. Origin
metadata must not be used to invent a domain identity the server did not report.

All result columns are nullable unless the author uses `!`. A table's NOT NULL
constraint does not survive null extension through joins, and Describe does not
supply a proof for general SQL expressions. Automatic proofs are deferred. An
explicit assertion can fail at decode time; it is not an inferred guarantee.
Shared model names include schema identity and require a complete, nonduplicated
projection. Dynamic variants must agree on result shape and parameter identities
by original parameter name after inactive parameters are omitted.

Generated enums, named composites, and shared models use schema-prefixed names,
including `public`. This default is accepted for 0.1: deterministic identity takes
priority over shorter names. Configuration supplies explicit renames; automatic
singularization and numeric collision suffixes are excluded. Adding an unrelated
query or type must not rename existing declarations. This is a compatibility
requirement to verify before the freeze. Ambiguous shorthand keys and generated
name collisions fail before rendering. Composite fields are optional independently
of constraints on the originating table. Named recursive declarations and codecs
are emitted as recursive groups; a NULL composite is distinct from a record whose
fields are all NULL. Anonymous `record` requires an explicit named cast.

Arrays use `Sqlkit.Pg_array.t`: dimensions and lower bounds plus flattened optional
elements. PostgreSQL dimensions are a property of each value, not reliably of the
column type. Constructor validation rejects mismatched sizes, excessive dimensions,
and overflow. A plain-list adapter accepts only representable arrays. The old
`Row.list` / `Value.of_list` helpers remain available for existing handwritten code;
new generated code uses the lossless representation.

Ranges preserve empty, unbounded, inclusive, and exclusive bounds. Multiranges
preserve the server's ordered ranges; canonicalization belongs to PostgreSQL. Public range constructors deliberately permit arbitrary endpoint ordering.
Local construction and encoding preserve the supplied bounds; they do not prove
that PostgreSQL will accept them. Endpoint comparison and canonicalization depend
on the database subtype, so sqlkit does not invent an OCaml comparator or silently
rewrite bounds. The same rule applies to caller-supplied multirange lists: callers
may submit unsorted or overlapping ranges for PostgreSQL to canonicalize. Server
rejections use the existing structured driver error contract.
Scalar codec limits still apply to endpoints. Container grammars share escaping
primitives but keep their different NULL conventions. Bytea is decoded from hex
or escape output and printed as hex, including inside containers. `int8` uses
`int64` throughout so the full PostgreSQL range fits.

Exact custom mappings replace a complete value. Without an exact mapping,
resolution descends through domains and container elements. Codecs and generated
signatures depend only on `sqlkit`; container values still cross the driver boundary as text.

## Inference verification

The corpus crosses SQL shapes with PostgreSQL types and compares sqlkit metadata
against separate libpq Prepare/Describe calls and independently queried catalogs.
It also executes each query and compiles generated decoders against the exact
returned text/NULL rows. Execution can disprove a non-null claim, never prove it.

pGenie is a second implementation, not an oracle of truth. Its pinned 0.15.0
release has explicit unsupported categories for domains, custom ranges, and
composites with dropped attributes. Same-named custom types require separate
pGenie projects. Differences in nullability precision and array dimensionality
policy do not change identity comparisons. Unknown failures remain fatal.
See integration/README.md for replay, reduction, and CI gates.

## Failure contract for 0.1

The implementation propagates results. Only explicitly named `_exn` adapters unwrap
errors by raising; `_opt` adapters discard diagnostics via `Result.to_option`.
SQL `!` annotations assert a result property independently of exception selection.
Expected absence remains `Ok None` in query APIs.

Public codec errors carry a typed reason and an ordered structural path. Callers
may inspect specific cases or handle every error generically through formatting;
exhaustive specialized handling is not required. Human-readable wording is not a
machine-readable contract.

`Codec.error` contains `reason` and `path`; `Row.error` is an alias. Generated code
adds names to column and composite-field positions and names parameter failures.
Array paths use PostgreSQL subscripts in every dimension, including custom lower
bounds. Other indexes are zero-based. Element decoding happens after array shape
validation so the subscript path is known. Range endpoints, multirange positions,
and COPY row positions compose without flattening nested errors into strings.
Syntax offsets are byte offsets within the parsed containing value after trimming,
not offsets into the whole SQL statement.

Execution preserves the codec error in `Error.Decode {query; error}` or
`Error.Encode {query; error}`. `Error.codec_error` optionally retrieves that cause,
including a primary error retained after failed cleanup. Generic `pp`/`to_string`
formatters cover all reasons. `Codec.custom` adapts custom string diagnostics
without pretending to classify them. Invalid runtime arguments continue to return
`Error.Invalid_argument`.
Custom parsers and printers both return codec results, including container callbacks.
Exception-only dependency parsers are caught at their immediate adapter boundary.

A user callback may violate its declared result contract by raising. At the execution
boundary `Sqlkit.callback` translates ordinary exceptions to `Error.Callback`, retaining
an existing `Sql_error` as its original error so transaction retry classification works.
Cancellation propagation by exception is an accepted API decision: cleanup runs before
unwinding continues, including from result-based APIs. The implementation also leaves
out-of-memory, stack overflow, and interruption uncaught.
`Driver.S.is_cancellation` identifies backend cancellation without a core Eio dependency.
`Driver.S.with_cleanup` ensures rollback runs even during cancellation; Caqti shields
cleanup with `Eio.Cancel.protect`. Standalone codec callbacks are required to honor
their result contract, just as driver implementations must honor theirs.

`transaction_exn` and generated query `_exn` functions are outer adapters. Neither
parsing nor normal execution uses them internally. Failed transaction commits are
followed by a rollback attempt, and successful commits are never retried because of
a subsequent cleanup failure. This migration does not yet declare 0.1 API stability.

## Compatibility before 1.0.0

1.0.0 is the compatibility boundary. Before that release, breaking changes are
allowed, including within 0.1.x. Reviewing the 0.1 APIs establishes deliberate
contracts for that release; it does not freeze them through the pre-1.0 series.
Breaking changes must be identified in release notes with migration guidance.

Preserving application source compatibility, generated signatures for unchanged
inputs, structured error meanings, and clear snapshot migrations remains the
design goal. These are intended compatibility guarantees for 1.0.0 onward, not
promises made by pre-1.0 releases. Exact versioning and support rules will be
finalized before 1.0.0. Correctness fixes must never preserve an unsound inferred
contract merely to maintain compatibility.

## Production failure recovery and limits

Rollback failure produces `Error.Cleanup` with the original failure and the
cleanup failure, then closes the physical connection. Nested rollback also
releases its savepoint. Cleanup failure prevents transaction retry. Cancellation
propagates after shielded cleanup; telemetry retains cleanup failures that cannot
be returned alongside that exception. Only serialization/deadlock errors with
successful cleanup are automatically retried; a lost COMMIT response is ambiguous.

The Eio pool owns a semaphore and an idle connection list. Cancellation of a
waiter must release capacity without holding a pool mutex. An integration test
exposed a hang when cancelling Caqti's pool acquisition, so sqlkit owns this small
layer while retaining Caqti for connections and execution. A borrowed runtime
handle expires on release. Poisoned physical connections are disconnected, never
recycled or silently reset in a transaction. Session limits apply to each new
physical connection. The pool is confined to one Eio domain.

Execution returns `int option`: PostgreSQL command tags without row counts are
unknown, while DML counts include zero. Caqti's response count API parses empty
PostgreSQL command tags with `int_of_string`; the adapter handles that dependency
exception locally before Caqti's recovery machinery can reset a transaction.

Limits and telemetry live in the runtime without an Eio dependency. Driver clocks
provide monotonic timing. Telemetry exposes no SQL or values, and an observer's
ordinary failure does not change a query result. Deadline errors are structured;
server timeout errors preserve SQLSTATE. Eio query deadlines cover each operation;
the libpq driver offers server limits and a connect timeout, not a client I/O deadline.

Caqti 3.0's PostgreSQL connection constructor also blocks before Eio can poll it.
The handshake test proved that a timer alone did not bound this call. An isolated
copy of that backend uses asynchronous libpq startup and finishes failed/cancelled
initial connections. It registers a sqlkit-only scheme; other Caqti users are
unaffected. The compatible Caqti version is pinned while this uses private APIs.
Upstream provenance, patch scope and licenses live in `lib/driver_caqti/vendor/`.

## Validated finite intervals

`Interval.t` is a private record: application code can inspect each component,
but construction and parsing share a checked boundary. Months and days have
PostgreSQL's signed 32-bit bounds; time uses signed 64-bit microseconds. Checked
integer arithmetic prevents wrapping, and exact decimal parsing avoids losing
microseconds through floating-point conversion. Intermediate overflow is rejected
even if a later component could mathematically cancel it.

Months, days, and time remain independent, including mixed signs. Normalizing
across them would impose calendar or daylight-saving assumptions. PostgreSQL's
two reserved infinity tuples are rejected because this record models finite
intervals. Result constructors are canonical; option and exception adapters
derive from them. Any future infinity support needs an explicit API decision.
