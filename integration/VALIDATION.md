# Local validation — 2026-09-15

Toolchain: OCaml 5.5.0, dune 3.24.1, PostgreSQL 18.0, ocamlformat 0.29.0,
pGenie 0.15.0 (verified release executable).

| Gate | Result |
| --- | --- |
| Locked-dependency `dune build` and `dune test` | Passed |
| Opam dependency workflow build and unit tests | Passed |
| libpq e2e, including nested containers and COPY | Passed |
| Application example and pooled Caqti example | Passed |
| Regenerated code, live check, offline check, snapshot | Passed; 24 example queries |
| Formatting and documentation | Passed |
| Three-package isolated install build | Passed |
| PostgreSQL inference corpus | 1,000 and 10,000 cases passed, seed 0 |
| Compiled generated corpus decoders | Both tiers passed against captured live rows |
| pGenie PR tier | 783 matched; 217 explicitly unsupported |
| pGenie extended tier | 7,814 matched; 2,186 explicitly unsupported |
| Recreated-schema snapshot fingerprint | Identical despite new catalog OIDs |
| Injected differential mismatch and SQL reduction | Failure detected; reduced query verified to retain disagreement |
| Final vector/array-detection refinement | Rechecked with 1,000 cases and compiled decoders |

The extended pGenie exclusions were 1,562 domain cases, 312 custom-range cases,
and 312 dropped-composite-attribute cases. Those cases passed the PostgreSQL and
compiled-decoder gates. No unexplained differences remain in these runs.

The corpus is a documented finite matrix, not a proof for all PostgreSQL programs.
The new CI workflow is configured for PR and weekly/manual tiers; it has not been
run remotely as part of this local work. No release has been published.

Full transient evidence is in `/tmp/sqlkit-inference-final`,
`/tmp/sqlkit-inference-extended`, and `/tmp/sqlkit-inference-final-reviewed` on the
implementation machine. Reproduction commands and limitations are in README.md
in this directory. Migration guidance is in the repository root README.md.

## Result-contract migration — 2026-09-15

The subsequent 0.1 failure-contract pass changes parsers, generated codecs, query
contracts and callback handling. Its local gates passed:

- Locked build and database-free unit tests, including 10,000 deterministic
  malformed inputs, explicit exception/option adapters, and pre-execution failures.
- libpq e2e, application example, and pooled Caqti example. NUL text returns an
  encoding error; real Eio cancellation propagates after rolling back an insert.
- Generated output, live and offline checks: 25 queries from three SQL files.
- Formatting, documentation, and isolated installation of all three packages.
- 1,000 PostgreSQL cases and compiled result-based decoders, including separate
  schema-qualified composites with identical field names and different field types.
- pGenie: 783 matched, 217 documented unsupported; no unexplained differences.

The 10,000-case differential tier above records the earlier compiler implementation;
this migration reran the 1,000-case tier. Evidence for the migration is in
`/tmp/sqlkit-result-final` and `/tmp/sqlkit-result-*.log`. Cancellation propagation by
exception has since been acknowledged by the user. Other API decisions remain under
review; no release is declared.

## Production recovery and operations — 2026-09-15

Local validation passed after the production hardening changes:

- Locked default build, database-free unit tests, formatting and documentation.
- libpq e2e, application example, and pooled Caqti web example.
- Generated output and live/offline checks: 35 queries from four SQL files.
- Isolated installation of all three packages, including the compatibility
  backend's license files.
- 1,000 PostgreSQL inference cases and compiled decoders; pGenie matched 783,
  with 217 documented exclusions and no unexplained differences.

`example/e2e.exe --section production` now exercises real affected counts
(zero/one/many), commands without counts, failed rollback with both errors
preserved, backend termination during transactions and active queries, server
query cancellation, active Eio cancellation, expired handles, pool exhaustion,
waiter cancellation, client query and handshake deadlines, server statement and
lock timeouts, connection replacement/session setup, an 8 MiB parameter upload,
and telemetry including observer failure. Test termination targets only backend
PIDs obtained from its own connections. Unit tests separately prove a retryable
primary failure is not retried when rollback fails.

A silent TCP peer exposed blocking connection startup in Caqti 3.0. The isolated
compatibility backend enables asynchronous startup, cleans up incomplete
connections, and uses nonblocking reads/writes. The Eio pool owns cancellation-safe
capacity accounting. Upstream code, license notices, and patch rationale are in
`lib/driver_caqti/vendor/`. Caqti stays pinned to the compatible version; unrelated
locked dependency versions were retained when updating the dependency digest.

Evidence: `/tmp/sqlkit-production-*.log` and `/tmp/sqlkit-production-corpus`.
These are local checks, not a remote CI run or a release. The blocking libpq
driver still has no client query deadline; its connect and server limits are
explicitly documented. Neither driver automatically retries transport failures.

## Structured codec diagnostics — 2026-09-15

Codec failures now carry typed reasons and outer-to-inner structural paths.
Generated decoders preserve column names, composite field names (including lexical
failures), PostgreSQL array subscripts, and range/multirange positions. Generated
encoders preserve parameter names and nested paths; COPY adds the input row index.
Runtime `Error.Encode`/`Error.Decode` retain the codec error without flattening it.

Local gates passed:

- Default locked build and database-free unit tests, including the existing
  10,000 malformed-input cases and new structured-path assertions using real
  generated decoders and encoders.
- libpq e2e and application example; pooled Caqti web/recovery tests. Nested NUL
  failures now assert both their reason and named parameter/field path on both drivers.
- Formatting, documentation, and isolated installation of all three packages.
- Generated output and live/offline checks for 35 queries from four SQL files.
- 1,000 PostgreSQL inference cases and compiled decoders; pGenie matched 783,
  with 217 documented exclusions and no unexplained differences.

New assertions cover multidimensional negative array bounds, syntax offsets,
named lexical composite fields, unknown enum labels, field counts, multirange
bounds, NULL/missing columns, custom errors, pre-execution parameter/COPY failures,
optional inspection, generic formatting, and equivalent result/option/exception
adapters. Evidence is in `/tmp/sqlkit-structured-*.log` and
`/tmp/sqlkit-structured-corpus`. These are local checks; no release or remote CI
run is implied. Regenerate query code and migrate custom string errors as described
in the root README.

## Validated interval constructors — 2026-09-15

`Interval.t` is now private, with checked result/option/exception constructors.
Parsing uses checked integer arithmetic and exact fractional seconds. It retains
mixed signs, handles SQL-standard leading signs and verbose fractional seconds,
and rejects overflow and reserved infinity values.

Local gates passed:

- Database-free unit tests, including constructor/parser bounds, equivalent
  adapters, malformed fractions, and 2,000 deterministic finite round trips.
- libpq e2e and pooled Caqti web tests: 40 interval boundary round trips per
  driver across all four PostgreSQL interval styles, including int64 extrema.
- Application example, formatting, documentation, and isolated installation of
  all three packages with the locked build.
- Refreshed snapshot and matching live/offline generated output for 36 queries
  from four SQL files.
- External compilation accepts field reads and rejects direct record construction
  and record updates through the private interface.

Evidence: `/tmp/sqlkit-interval-*.log` and `/tmp/sqlkit-interval-api`.
The compiler inference corpus was not rerun for this interval runtime change;
its preceding result is recorded above. No release or remote CI run is implied.
