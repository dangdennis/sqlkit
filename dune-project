(lang dune 3.20)
(name sqlkit)
(version 0.1.0)
(generate_opam_files true)

(source (github dangdennis/sqlkit))
(authors "Dennis Dang")
(maintainers "Dennis Dang")
(license MIT)
(documentation "https://github.com/dangdennis/sqlkit")
(maintenance_intent "(latest)")

; Several packages, so that depending on the runtime does not drag in libpq,
; Caqti and Eio. Generated code needs only `sqlkit`.

(package
 (name sqlkit)
 (synopsis "Typed OCaml from your SQL, checked against PostgreSQL")
 (description
  "Write .sql files with sqlc-style annotations; sqlkit asks PostgreSQL for the\
 \n parameter and result types of each query and emits typed OCaml. This package\
 \n is the runtime that generated code depends on: value encoding and decoding,\
 \n the query interface, transactions, and the driver boundary. It pulls in no\
 \n database client of its own -- add sqlkit-postgresql or sqlkit-caqti for that.")
 (depends
  (ocaml (>= 5.5.0))
  (ptime (>= 1.0.0))
  (uuidm (>= 0.9.8))
  (decimal (>= 1.0.0))
  (yojson (>= 2.0.0))))

(package
 (name sqlkit-postgresql)
 (synopsis "The sqlkit code generator, CLI, and libpq driver")
 (description
  "Provides the `sqlkit` command (generate, check, describe) and a PostgreSQL\
 \n driver built directly on libpq. The generator needs libpq rather than a\
 \n higher-level client because it uses PQdescribePrepared together with\
 \n PQftable and PQftablecol, which are what tell it a result column's\
 \n originating table -- the basis for nullability and shared row types.")
 (depends
  (ocaml (>= 5.5.0))
  (sqlkit (= :version))
  dune-build-info
  (cmdliner (>= 1.3.0))
  (otoml (>= 1.0.0))
  (yojson (>= 2.0.0))
  conf-postgresql))

(package
 (name sqlkit-caqti)
 (synopsis "Caqti driver for sqlkit, with connection pooling")
 (description
  "Runs generated queries through Caqti over Eio, which adds connection\
 \n pooling and concurrency. Eio is direct style, so generated signatures are\
 \n unchanged: a query returns a plain result rather than a promise.")
 (depends
  (ocaml (>= 5.5.0))
  (sqlkit (= :version))
  (caqti (= 3.0.0))
  (caqti-eio (= 3.0.0))
  (uri (>= 4.0.0))
  (mtime (>= 2.0.0))
  (postgresql (>= 5.2.0))
  ; The compatibility driver uses Caqti 3.0 private interfaces.
  ; Stock driver is exercised by example dependencies only.
  (caqti-driver-postgresql (and (= 3.0.0) :with-test))
  ; example programs and e2e only
  (eio_main :with-test)))

(package
 (name sqlkit-eio)
 (synopsis "Native Eio driver for sqlkit on libpq's asynchronous API")
 (description
  "Runs generated queries over libpq without blocking the domain: fibers wait\
 \n on the connection's socket through Eio. Pooled, pipelined, with full\
 \n diagnostics, COPY and server-side cancellation. libpq still owns the\
 \n protocol, authentication and TLS.")
 (depends
  (ocaml (>= 5.5.0))
  (sqlkit (= :version))
  (sqlkit-postgresql (= :version))
  (sqlkit-pool-eio (= :version))
  (eio (>= 1.0))
  ; real-backend checks only
  (eio_main :with-test)))

(package
 (name sqlkit-pool-eio)
 (synopsis "Connection pool shared by sqlkit's Eio drivers")
 (description
  "Bounded, lazy, single-domain pool with acquisition deadlines and telemetry.\
 \n Used through a driver's own Pool module.")
 (depends
  (ocaml (>= 5.5.0))
  (sqlkit (= :version))
  (eio (>= 1.0))
  (mtime (>= 2.0.0))))

(package
 (name sqlkit-pgwire)
 (synopsis "Pure OCaml PostgreSQL driver for sqlkit, over Eio")
 (description
  "Speaks the PostgreSQL wire protocol directly: no libpq and no C stubs of\
 \n its own. SCRAM-SHA-256, MD5 and password authentication, TLS, pipelined\
 \n prepare, COPY, full diagnostics and cancellation.")
 (depends
  (ocaml (>= 5.5.0))
  (sqlkit (= :version))
  (sqlkit-pool-eio (= :version))
  (eio (>= 1.0))
  (tls-eio (>= 2.0.0))
  (ca-certs (>= 1.0.0))
  (x509 (>= 1.0.0))
  (domain-name (>= 0.4.0))
  (mirage-crypto-rng (>= 2.0.0))
  (digestif (>= 1.2.0))
  (base64 (>= 3.5.0))
  (eqaf (>= 0.9))
  (uri (>= 4.0.0))
  ; real-backend checks only
  (eio_main :with-test)))
