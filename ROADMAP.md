# Roadmap

Ordered by what a real application needs first. Nothing here is committed to;
it is a plan to argue with.

## 1. SQLSTATE in errors — done

Today `Error.Execute` carries a message string, so a caller cannot tell these
apart:

| SQLSTATE | Meaning | Right response |
| --- | --- | --- |
| `23505` | unique_violation | 409, do not retry |
| `23503` | foreign_key_violation | 400 |
| `23514` | check_violation | 400 |
| `40001` | serialization_failure | retry the transaction |
| `40P01` | deadlock_detected | retry the transaction |
| `57014` | query_canceled | timeout |
| `08006` | connection_failure | reconnect; resolve an uncertain commit before retrying |

A web application has to make these distinctions, and matching on the message
text is fragile and locale-dependent.

Proposed:

```ocaml
type sqlstate = private string

type t =
  | Execute of
      { query : string
      ; sql : string
      ; sqlstate : sqlstate option   (* None for client-side failures *)
      ; message : string
      ; detail : string option
      ; constraint_name : string option
      }
  | ...

val is_unique_violation : t -> bool
val is_retryable : t -> bool          (* 40001 and 40P01; no failed cleanup *)
val constraint_violated : t -> string option
```

`constraint_name` is what turns "something was already taken" into "email was
already taken" without parsing prose.

Shipped. `Sqlkit.Sqlstate` covers all 262 codes across 43 classes, generated from
`errcodes.txt` in the PostgreSQL source. libpq supplies every diagnostic field;
Caqti supplies the code and message only, which is all it exposes.

## 2. Naming, and overrides — done

Generated names come from three places, and they share one namespace:

- shared model row types, from a table name — `users` becomes `users_row`
- per-query row types, from the query name — `GetUser` becomes `get_user_row`
- enums, from the Postgres type name — `user_status`

A query named `Users` therefore collides with the model for table `users`. That
is now detected and reported with both origins, but detection is not a fix.

How the neighbours handle it:

- **sqlc** singularizes table names (`authors` becomes `Author`), names query
  results `<Query>Row`, and provides a `rename:` config. It has the same
  collision class and documents the same limitation: one namespace, so a table
  and a column cannot rename independently.
- **Squirrel** generates one row type per query and no shared models at all.
  No sharing, so no collisions — and no way to write a function that works
  across two queries returning the same table.

We chose sharing deliberately, so we inherit sqlc's problem and take its
escape hatch: a `sqlkit.toml` beside the queries directory.

```toml
[rename]
users = "user"              # users_row -> user_row
"users.display_name" = "name"
```

(Custom OCaml types for columns are separate — see 3.)

Deliberately not singularizing automatically. English pluralization is a swamp
(`data`, `series`, `status`, `people`), and sqlc users hit it constantly. An
explicit rename is longer and always right.

## 3. Custom type mapping — done

The largest single lever on how the generated API feels. Today a `uuid` column
is `Uuidm.t` and an email is `string`; there is no way to say a column is a
`User_id.t` or an `Email.t`.

```toml
[types."users.id"]
ocaml = "User_id.t"
of_string = "User_id.of_string"
to_string = "User_id.to_string"

[types.uuid]
ocaml = "Id.t"
of_string = "Id.of_string"
to_string = "Id.to_string"
```

Per-column overrides win over per-type. The generator splices the named
functions into the decoder and encoder it already emits, so this needs no
runtime support — `Typemap.t` grows a `Custom` case and the machinery is
unchanged. sqlc's most requested feature.

## 4. Transactions: isolation and savepoints — done

`transaction` currently issues a bare `BEGIN`. Two gaps:

- No isolation level. `transaction ~isolation:`Serializable` is required for
  anything doing read-modify-write, and serializable is unusable without a
  retry loop, which is unusable without (1).
- Nested transactions are undefined. A `transaction` inside a `transaction`
  issues a second `BEGIN`, which PostgreSQL warns about and ignores, so the
  inner rollback silently does nothing. Savepoints fix this properly.

```ocaml
val transaction :
  ?isolation:[ `Read_committed | `Repeatable_read | `Serializable ] ->
  ?retry:int ->
  conn -> (conn -> ('a, Error.t) result) -> ('a, Error.t) result
```

With `?retry`, a `40001` or `40P01` re-runs the body — the standard serializable
pattern, and the reason (1) comes first.

## 5. Streaming, and a stricter :one — done

Two small, independent wins.

`fetch_all` builds a list, which is the wrong shape for an export of a million
rows. Caqti already streams; libpq needs a cursor or single-row mode.

```ocaml
val fetch_stream : (module Q : Query.MANY) -> conn -> Q.params -> (Q.row -> unit) -> (unit, Error.t) result
```

And a `:one!` cardinality returning the row directly, erroring when absent,
since a good half of `:one` call sites immediately unwrap the option.

## 6. Dynamic filters — done (optional blocks)

The common request the SQL-first model has no clean answer to: "filter by name
if provided, and by status if provided."

What shipped is the generated-variants option, spelled as optional blocks:

```sql
-- name: FindUsers :many
SELECT id, email, status, balance
FROM users
WHERE organization_id = :org
  /*? AND email ILIKE :email */
  /*? AND status = :status */
ORDER BY email
LIMIT :limit;
```

Each block's parameters become optional labelled arguments; passing one
includes its block. Every inclusion combination is rewritten at codegen and
verified against Postgres with Describe, so the planner sees plain predicates
it can index, and the runtime never assembles a SQL string the generator has
not checked. The combinatorics are capped (4 blocks, 16 variants), all
variants must agree on the result shape, and block parameters cannot also be
nullable.

Rejected along the way: `(:email IS NULL OR email = :email)` — correct, but
PostgreSQL cannot use an index on a predicate it cannot see through — and
typed fragments, which are a query builder in disguise. The variant cap is
what keeps the combinatorial approach honest.

## 7. Bulk COPY inserts — done

`-- name: BulkAddUsers :copy` over a plain `INSERT INTO t (cols) VALUES
(:params)`. The INSERT is what Describe verifies (keeping the
everything-is-verified invariant); the generator builds `COPY t (cols) FROM
STDIN` from the verified column list, and the generated function takes a row
list and streams it in one round-trip through `PQputCopyData`. The runtime owns
COPY text escaping (`Value.Copy`, a third quoting regime after scalars and
array literals). libpq-only: the Caqti driver returns a clear error naming the
fix, because Caqti has no raw COPY surface and a row-at-a-time emulation would
silently lose COPY's all-or-nothing semantics.

## 8. Deeper compiler/type system — done

- `Pg_type` graph, schema-qualified identity, version 2 snapshots, and conservative
  result nullability.
- Domains mapped through their bases, named composite records, lossless arrays,
  built-in/custom ranges and multiranges, and full-width `int8`.
- Real round trips through libpq, Caqti, and COPY; deterministic inference corpus
  with independent PostgreSQL checks, compiled decoders, and pinned pGenie comparison.
- PR tier: 1,000 cases. Weekly/manual extended tier: 10,000 cases. Known pGenie
  limitations remain covered by PostgreSQL; unexplained disagreements fail.

## 9. Stabilize 0.1 APIs, then exercise a real application — next

The failure-contract pass is implemented: result-based codecs and generated contracts,
explicit `_exn` adapters, optional parsing adapters, typed argument/encoding/callback
errors, and cancellation-aware transaction cleanup. Cancellation propagation by
exception is accepted. Public structured codec errors with optional detailed
inspection are implemented: typed reasons and nested paths survive generated
encoding, decoding, runtime wrappers, and cleanup. Schema-qualified default names (including
`public`), explicit renames, and rejection of collisions are accepted; unrelated
additions must not rename existing declarations under the same naming rules.
Breaking changes remain allowed before 1.0.0, including within 0.1.x, with release
notes and migration guidance. The 0.1 review does not establish a compatibility
freeze; remaining API choices still need review.

Interval constructor review is complete: private readable record, validated
result/option/exception constructors, exact parsing, finite PostgreSQL bounds,
and real-backend coverage of all four interval output styles.

Range constructor review is complete: public bounds preserve caller input;
PostgreSQL validates ordering and canonicalizes ranges and multiranges using
subtype rules. Local construction does not guarantee server acceptance.

Review remaining runtime container constructors, generated naming/configuration, snapshot
compatibility, and error contracts after the compiler gates pass. Then build a
nontrivial application covering transactions, concurrency, search, and nested
PostgreSQL types. The example data-layer programs are not that application, and
this compiler work does not declare the APIs stable or publish a release.

## Not planned

- SQLite. No Describe equivalent, so inference needs a wholly different
  strategy, and its flexible typing makes the guarantee weaker anyway.
- Migrations. A separate concern with good existing tools.
- A query DSL. The premise of this project is that SQL is the source of truth.

## Production use in owned applications

Implemented: rollback error preservation and connection disposal; real affected
counts with explicit unknown counts; scoped pooled handles; bounded Eio pool
acquisition/connect/query operations; configurable server limits; query, transaction,
pool-wait, and cleanup telemetry without parameters. Real-backend tests exercise
termination, cancellation, pool exhaustion, timeout recovery, lock contention, and
connection replacement. Blocking libpq has no client query deadline.

## Eio drivers without Caqti — first pass done

Two drivers implement `Driver.S` on Eio and share one pool (`lib/pool_eio`):

- `sqlkit-eio` (`lib/driver_eio`): libpq's asynchronous API, fibers waiting on
  the socket. libpq keeps the protocol, authentication and TLS.
- `sqlkit-pgwire` (`lib/driver_pgwire`): the wire protocol in OCaml, no libpq.
  TCP and Unix sockets; trust, password, MD5 and SCRAM-SHA-256; TLS with
  `sslmode` up to `verify-full`; name resolution and the cancel request run as
  fibers.

Both keep every diagnostic field, real affected counts and COPY, send a
first-use prepare in the same write as its execution, and ask the server to
stop a statement whose fiber is cancelled or whose deadline passes. Both pass
the whole e2e suite, `example/eio_checks.ml` and `example/soak.exe`;
`sqlkit-pgwire` also passes `example/pgwire_checks.ml` (TLS, MD5, certificate
verification) and `test/test_pgwire.ml` (message layout, SCRAM, a decoder that
never raises).

Production testing (2026-11-21), each runnable from `example/`:

- `chaos.exe`: a proxy that fragments, delays, cuts, stalls, corrupts and
  injects into the stream. No escaped exception, no operation past its
  deadline, no wrong answer reported as success (over TLS, even under
  tampering), no leaked descriptors or backends.
- `differential.exe`: the same statements through `sqlkit-pgwire` and libpq,
  comparing rows, counts and every error field.
- `pooler_checks.exe`: PgBouncer in transaction mode with and without
  prepared-statement support. Statement names are a digest of the SQL, so a
  shared backend can never hand one client another's rows;
  `~statement_cache:false` is the mode for poolers without support.
- Compatibility: PostgreSQL 15 to 18, Unix sockets, Linux with the posix and
  io_uring backends. PostgreSQL 15 is the oldest supported version. Older
  servers are not refused, but nothing is tested against them; 13 and 14 are
  out of upstream support and reject interval literals whose time part is
  longer than about 35 minutes.

- A COMMIT that the server answers with ROLLBACK (the transaction had already
  failed) is an error in the libpq, Eio and pgwire drivers, not a success.
  Only statements with a plan worth keeping are prepared by name; transaction
  control never is.

Still open:

- Binary result format, in both drivers and the runtime's decoders.
- A batch API that exposes pipelining to callers.
- `sqlkit-pgwire`: SCRAM channel binding, SASLprep for non-ASCII passwords,
  client certificates, `.pgpass`, several hosts, GSSAPI. Each is refused
  loudly today.
- `sqlkit-pgwire` had one adversarial review (2026-11-21); its findings are
  fixed and covered by `test/test_pgwire.ml` and the hostile-server cases in
  `example/pgwire_checks.ml`. Not yet done: a review by someone other than
  its author's tooling, the same hardening pass on `sqlkit-eio` (no guard
  against two fibers on one handle, no reply budget), a stale reply that
  arrives after the between-statements check, and bytes already decrypted by
  the TLS layer being invisible to the pool's health check.
- `sqlkit-pgwire`: move the generator onto it (RowDescription carries the table
  and column identifiers it needs), which would remove libpq from the project.
- `sqlkit-eio`: a nonblocking cancel request once libpq 17 is the floor (today
  it runs on a detached thread); its host lookup blocks.
- Longer soaks, and the same checks against a managed server and a pooler.
- Retire `sqlkit-caqti` once an application has run on one of these.
