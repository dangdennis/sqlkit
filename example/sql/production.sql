-- name: BackendPid :one!
SELECT pg_backend_pid() AS "pid!";

-- name: TerminateBackend :one!
SELECT pg_terminate_backend(:pid::integer) AS "terminated!";

-- name: CancelBackend :one!
SELECT pg_cancel_backend(:pid::integer) AS "cancelled!";

-- name: BackendSleeping :one!
SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE pid = :pid::integer AND wait_event = 'PgSleep') AS "sleeping!";

-- name: WaitForServer :one!
SELECT 1 AS "completed!" FROM pg_sleep(:seconds::double precision);

-- name: ProductionInsert :exec
INSERT INTO users (organization_id, email)
SELECT :org::uuid, 'production-' || :org::uuid::text || '-' || i::text || '@example.test'
FROM generate_series(1, :count::integer) AS i;

-- name: ProductionUpdate :exec
UPDATE users SET display_name = :label WHERE organization_id = :org::uuid;

-- name: ProductionDelete :exec
DELETE FROM users WHERE organization_id = :org::uuid;

-- name: SessionLimits :one!
SELECT current_setting('statement_timeout') AS "statement!",
       current_setting('lock_timeout') AS "lock!",
       current_setting('idle_in_transaction_session_timeout') AS "idle!";

-- name: PayloadSize :one!
SELECT octet_length(:payload::text) AS "size!";

-- name: SetIntervalStyle :one!
SELECT set_config('IntervalStyle', :style::text, false) AS "style!";
