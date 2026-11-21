/* Minimal libpq binding, just enough to describe a prepared statement.
 *
 * Why this exists rather than reusing postgresql-ocaml: that library does not
 * expose PQftable / PQftablecol, and those two fields are the whole point.
 * They are what tells us which table.column a result column came from, which
 * is what drives nullability (pg_attribute.attnotnull) and shared model type
 * detection. Everything else here is scaffolding to reach them.
 *
 * Going through libpq rather than the wire protocol means authentication --
 * SCRAM-SHA-256, TLS, .pgpass, everything -- is libpq's problem, not ours.
 *
 * Handles are passed as nativeint rather than custom blocks. Lifetimes are
 * managed explicitly by the caller; this is used by a short-lived generator
 * process, not a long-running service. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <libpq-fe.h>
#include <stdlib.h>
#include <string.h>
#include <caml/threads.h>
#include <caml/fail.h>

#define Conn_val(v) ((PGconn *)Nativeint_val(v))
#define Res_val(v) ((PGresult *)Nativeint_val(v))

/* ---------- connection ---------- */

/* Blocking libpq calls run with the OCaml runtime lock released, so a slow
 * server does not stall every other domain. Anything read from the OCaml heap
 * must be copied to C memory first: String_val pointers are invalid once the
 * lock is released and the GC may move the blocks. */
CAMLprim value sqlkit_pq_connect(value conninfo)
{
  CAMLparam1(conninfo);
  char *ci = caml_stat_strdup(String_val(conninfo));
  caml_release_runtime_system();
  PGconn *c = PQconnectdb(ci);
  caml_acquire_runtime_system();
  caml_stat_free(ci);
  CAMLreturn(caml_copy_nativeint((intnat)c));
}

CAMLprim value sqlkit_pq_connect_ok(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  CAMLreturn(Val_bool(c != NULL && PQstatus(c) == CONNECTION_OK));
}

CAMLprim value sqlkit_pq_error_message(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  const char *m = (c == NULL) ? "null connection" : PQerrorMessage(c);
  CAMLreturn(caml_copy_string(m == NULL ? "" : m));
}

CAMLprim value sqlkit_pq_finish(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  if (c != NULL) PQfinish(c);
  CAMLreturn(Val_unit);
}

/* ---------- statements ---------- */

/* nParams = 0 lets the server infer parameter types, which is exactly what we
 * want -- the inferred types are the answer we are after. */
CAMLprim value sqlkit_pq_prepare(value conn, value name, value sql)
{
  CAMLparam3(conn, name, sql);
  PGconn *c = Conn_val(conn);
  char *nm = caml_stat_strdup(String_val(name));
  char *q = caml_stat_strdup(String_val(sql));
  caml_release_runtime_system();
  PGresult *r = PQprepare(c, nm, q, 0, NULL);
  caml_acquire_runtime_system();
  caml_stat_free(nm);
  caml_stat_free(q);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlkit_pq_describe_prepared(value conn, value name)
{
  CAMLparam2(conn, name);
  PGconn *c = Conn_val(conn);
  char *nm = caml_stat_strdup(String_val(name));
  caml_release_runtime_system();
  PGresult *r = PQdescribePrepared(c, nm);
  caml_acquire_runtime_system();
  caml_stat_free(nm);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlkit_pq_exec(value conn, value sql)
{
  CAMLparam2(conn, sql);
  PGconn *c = Conn_val(conn);
  char *q = caml_stat_strdup(String_val(sql));
  caml_release_runtime_system();
  PGresult *r = PQexec(c, q);
  caml_acquire_runtime_system();
  caml_stat_free(q);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

/* ---------- COPY FROM STDIN ---------- */

/* Data is copied to C memory by length (COPY lines are text, but being
   length-clean here costs nothing), and the runtime lock is released around
   the blocking calls like every other libpq entry point. */
CAMLprim value sqlkit_pq_put_copy_data(value conn, value data)
{
  CAMLparam2(conn, data);
  PGconn *c = Conn_val(conn);
  size_t len = caml_string_length(data);
  char *buf = caml_stat_alloc(len);
  memcpy(buf, Bytes_val(data), len);
  caml_release_runtime_system();
  int rc = PQputCopyData(c, buf, (int)len);
  caml_acquire_runtime_system();
  caml_stat_free(buf);
  CAMLreturn(Val_int(rc));
}

CAMLprim value sqlkit_pq_put_copy_end(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  caml_release_runtime_system();
  int rc = PQputCopyEnd(c, NULL);
  caml_acquire_runtime_system();
  CAMLreturn(Val_int(rc));
}

/* NULL maps to 0; the OCaml side tests with result_is_null before use. */
CAMLprim value sqlkit_pq_get_result(value conn)
{
  CAMLparam1(conn);
  PGconn *c = Conn_val(conn);
  caml_release_runtime_system();
  PGresult *r = PQgetResult(c);
  caml_acquire_runtime_system();
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

CAMLprim value sqlkit_pq_result_is_null(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_bool(Nativeint_val(res) == 0));
}

/* ---------- results ---------- */

/* Maps to ExecStatusType; 1 = COMMAND_OK, 2 = TUPLES_OK. */
CAMLprim value sqlkit_pq_result_status(value res)
{
  CAMLparam1(res);
  PGresult *r = Res_val(res);
  CAMLreturn(Val_int(r == NULL ? -1 : (int)PQresultStatus(r)));
}

CAMLprim value sqlkit_pq_result_error(value res)
{
  CAMLparam1(res);
  PGresult *r = Res_val(res);
  const char *m = (r == NULL) ? "null result" : PQresultErrorMessage(r);
  CAMLreturn(caml_copy_string(m == NULL ? "" : m));
}

CAMLprim value sqlkit_pq_clear(value res)
{
  CAMLparam1(res);
  PGresult *r = Res_val(res);
  if (r != NULL) PQclear(r);
  CAMLreturn(Val_unit);
}

CAMLprim value sqlkit_pq_nparams(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_int(PQnparams(Res_val(res))));
}

CAMLprim value sqlkit_pq_paramtype(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_long((long)PQparamtype(Res_val(res), Int_val(i))));
}

CAMLprim value sqlkit_pq_nfields(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_int(PQnfields(Res_val(res))));
}

CAMLprim value sqlkit_pq_fname(value res, value i)
{
  CAMLparam2(res, i);
  const char *n = PQfname(Res_val(res), Int_val(i));
  CAMLreturn(caml_copy_string(n == NULL ? "" : n));
}

CAMLprim value sqlkit_pq_ftype(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_long((long)PQftype(Res_val(res), Int_val(i))));
}

/* The two accessors this whole file exists for. PQftable returns InvalidOid (0)
 * when the column is not a simple reference to a table column -- an expression,
 * a literal, an aggregate. PQftablecol likewise returns 0. */
CAMLprim value sqlkit_pq_ftable(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_long((long)PQftable(Res_val(res), Int_val(i))));
}

CAMLprim value sqlkit_pq_fmod(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_int(PQfmod(Res_val(res), Int_val(i))));
}

CAMLprim value sqlkit_pq_ftablecol(value res, value i)
{
  CAMLparam2(res, i);
  CAMLreturn(Val_int(PQftablecol(Res_val(res), Int_val(i))));
}

/* ---------- tuples (for catalog queries) ---------- */

CAMLprim value sqlkit_pq_ntuples(value res)
{
  CAMLparam1(res);
  CAMLreturn(Val_int(PQntuples(Res_val(res))));
}

CAMLprim value sqlkit_pq_getvalue(value res, value row, value col)
{
  CAMLparam3(res, row, col);
  const char *v = PQgetvalue(Res_val(res), Int_val(row), Int_val(col));
  CAMLreturn(caml_copy_string(v == NULL ? "" : v));
}

CAMLprim value sqlkit_pq_getisnull(value res, value row, value col)
{
  CAMLparam3(res, row, col);
  CAMLreturn(Val_bool(PQgetisnull(Res_val(res), Int_val(row), Int_val(col))));
}

/* ---------- executing ---------- */

/* Copy a string-option array of parameters out of the OCaml heap, so the
 * runtime lock can be released while libpq blocks. A parameter containing NUL
 * is rejected (returns -1): libpq would silently truncate it at the NUL,
 * sending a different value than the caller supplied. */
static int copy_params(value params, int n, char ***out)
{
  char **vals = NULL;
  if (n > 0) vals = (char **)caml_stat_alloc((size_t)n * sizeof(char *));
  for (int i = 0; i < n; i++) {
    value p = Field(params, i);
    if (Is_block(p)) {
      value sv = Field(p, 0);
      size_t len = caml_string_length(sv);
      if (memchr(String_val(sv), 0, len) != NULL) {
        for (int j = 0; j < i; j++)
          if (vals[j]) caml_stat_free(vals[j]);
        if (vals) caml_stat_free(vals);
        return -1;
      }
      vals[i] = caml_stat_strdup(String_val(sv));
    }
    else vals[i] = NULL;
  }
  *out = vals;
  return 0;
}

static void free_params(char **vals, int n)
{
  for (int i = 0; i < n; i++)
    if (vals && vals[i]) caml_stat_free(vals[i]);
  if (vals) caml_stat_free(vals);
}

/* [params] is a string option array; None becomes SQL NULL.
 *
 * paramTypes is NULL on purpose, so the server infers each parameter's type
 * from context. Forcing them to text -- which is what a statically-typed
 * client layer naturally does -- breaks comparisons like `WHERE uuid_col = $1`
 * with "operator does not exist: uuid = text". */
CAMLprim value sqlkit_pq_exec_params(value conn, value sql, value params)
{
  CAMLparam3(conn, sql, params);
  PGconn *c = Conn_val(conn);
  int n = (int)Wosize_val(params);
  char **vals = NULL;
  if (copy_params(params, n, &vals) != 0)
    CAMLreturn(caml_copy_nativeint((intnat)NULL));
  char *q = caml_stat_strdup(String_val(sql));
  caml_release_runtime_system();
  PGresult *r = PQexecParams(c, q, n, NULL, (const char *const *)vals, NULL, NULL, 0);
  caml_acquire_runtime_system();
  caml_stat_free(q);
  free_params(vals, n);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

/* Rows affected by an INSERT/UPDATE/DELETE, as reported by the command tag. */
CAMLprim value sqlkit_pq_cmd_tuples(value res)
{
  CAMLparam1(res);
  const char *s = PQcmdTuples(Res_val(res));
  CAMLreturn(Val_long((s == NULL || *s == '\0') ? 0 : strtoll(s, NULL, 10)));
}

/* PQresultErrorField, for the diagnostic fields that let a caller act on a
 * failure rather than just print it. PG_DIAG_SQLSTATE ('C') is the important
 * one; CONSTRAINT_NAME ('n') turns "something was taken" into "email was
 * taken". Returns "" when the field is absent. */
CAMLprim value sqlkit_pq_result_error_field(value res, value field)
{
  CAMLparam2(res, field);
  PGresult *r = Res_val(res);
  const char *v = (r == NULL) ? NULL : PQresultErrorField(r, Int_val(field));
  CAMLreturn(caml_copy_string(v == NULL ? "" : v));
}

/* Execute a previously prepared statement. Same parameter convention as
 * sqlkit_pq_exec_params: text format, None becomes NULL. */
CAMLprim value sqlkit_pq_exec_prepared(value conn, value name, value params)
{
  CAMLparam3(conn, name, params);
  PGconn *c = Conn_val(conn);
  int n = (int)Wosize_val(params);
  char **vals = NULL;
  if (copy_params(params, n, &vals) != 0)
    CAMLreturn(caml_copy_nativeint((intnat)NULL));
  char *nm = caml_stat_strdup(String_val(name));
  caml_release_runtime_system();
  PGresult *r = PQexecPrepared(c, nm, n, (const char *const *)vals, NULL, NULL, 0);
  caml_acquire_runtime_system();
  caml_stat_free(nm);
  free_params(vals, n);
  CAMLreturn(caml_copy_nativeint((intnat)r));
}

#include <time.h>
#include <stdio.h>
CAMLprim value sqlkit_monotonic_time(value unit) {
  CAMLparam1(unit);
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  CAMLreturn(caml_copy_double((double)ts.tv_sec + (double)ts.tv_nsec / 1e9));
}
CAMLprim value sqlkit_pq_cmd_tuples_text(value res) {
  CAMLparam1(res);
  CAMLreturn(caml_copy_string(PQcmdTuples(Res_val(res))));
}
CAMLprim value sqlkit_pq_connect_timeout(value conninfo, value seconds) {
  CAMLparam2(conninfo, seconds);
  char *ci = caml_stat_strdup(String_val(conninfo));
  char timeout[32];
  snprintf(timeout, sizeof(timeout), "%ld", Long_val(seconds));
  const char *keys[] = {"dbname", "connect_timeout", NULL};
  const char *values[] = {ci, timeout, NULL};
  caml_release_runtime_system();
  PGconn *c = PQconnectdbParams(keys, values, 1);
  caml_acquire_runtime_system();
  caml_stat_free(ci);
  CAMLreturn(caml_copy_nativeint((intnat)c));
}

/* ---------- asynchronous API ----------
 *
 * Used by the Eio driver. Nothing below blocks on the server, so nothing
 * releases the runtime lock: each call either queues bytes, moves bytes
 * already available on the socket, or reads libpq's own state. Waiting happens
 * in OCaml, on the descriptor from sqlkit_pq_socket. Because the lock is held
 * and nothing allocates during the libpq call, String_val pointers can be
 * passed straight through; libpq copies them into its output buffer.
 *
 * The one exception is PQconnectStart, which resolves host names with a
 * blocking getaddrinfo. That limitation is libpq's. */

#include <pthread.h>
#include <unistd.h>

_Static_assert(PGRES_COPY_IN == 4, "ExecStatusType changed");
_Static_assert(PGRES_PIPELINE_SYNC == 10, "ExecStatusType changed");
_Static_assert(PGRES_PIPELINE_ABORTED == 11, "ExecStatusType changed");
_Static_assert(PGRES_POLLING_FAILED == 0 && PGRES_POLLING_READING == 1 &&
               PGRES_POLLING_WRITING == 2 && PGRES_POLLING_OK == 3,
               "PostgresPollingStatusType changed");
_Static_assert(PQTRANS_IDLE == 0, "PGTransactionStatusType changed");

CAMLprim value sqlkit_pq_connect_start(value conninfo)
{
  CAMLparam1(conninfo);
  char *ci = caml_stat_strdup(String_val(conninfo));
  caml_release_runtime_system();
  PGconn *c = PQconnectStart(ci);
  caml_acquire_runtime_system();
  caml_stat_free(ci);
  CAMLreturn(caml_copy_nativeint((intnat)c));
}

CAMLprim value sqlkit_pq_conn_is_null(value conn)
{
  return Val_bool(Nativeint_val(conn) == 0);
}

/* 150004 for 15.4; 0 when the connection is not established. */
CAMLprim value sqlkit_pq_server_version(value conn)
{
  return Val_int(PQserverVersion(Conn_val(conn)));
}

CAMLprim value sqlkit_pq_connect_poll(value conn)
{
  return Val_int((int)PQconnectPoll(Conn_val(conn)));
}

/* A descriptor is an int on every platform this binding supports, which is
 * also how the Unix library represents Unix.file_descr there. -1 when the
 * connection has no socket; the OCaml side checks sqlkit_pq_socket_fd first. */
CAMLprim value sqlkit_pq_socket(value conn)
{
  return Val_int(PQsocket(Conn_val(conn)));
}

CAMLprim value sqlkit_pq_set_nonblocking(value conn)
{
  return Val_bool(PQsetnonblocking(Conn_val(conn), 1) == 0);
}

CAMLprim value sqlkit_pq_enter_pipeline(value conn)
{
  return Val_bool(PQenterPipelineMode(Conn_val(conn)) == 1);
}

CAMLprim value sqlkit_pq_exit_pipeline(value conn)
{
  return Val_bool(PQexitPipelineMode(Conn_val(conn)) == 1);
}

CAMLprim value sqlkit_pq_pipeline_sync(value conn)
{
  return Val_bool(PQpipelineSync(Conn_val(conn)) == 1);
}

CAMLprim value sqlkit_pq_transaction_idle(value conn)
{
  return Val_bool(PQtransactionStatus(Conn_val(conn)) == PQTRANS_IDLE);
}

CAMLprim value sqlkit_pq_send_query(value conn, value sql)
{
  return Val_bool(PQsendQuery(Conn_val(conn), String_val(sql)) == 1);
}

CAMLprim value sqlkit_pq_send_prepare(value conn, value name, value sql)
{
  return Val_bool(
    PQsendPrepare(Conn_val(conn), String_val(name), String_val(sql), 0, NULL) == 1);
}

/* [params] is a string option array in text format; the caller has already
 * rejected NUL bytes. The pointer table is the only allocation, and it is C
 * memory, so the OCaml heap cannot move underneath the call. */
static const char **borrow_params(value params, int n)
{
  if (n == 0) return NULL;
  const char **vals = (const char **)caml_stat_alloc((size_t)n * sizeof(char *));
  for (int i = 0; i < n; i++) {
    value p = Field(params, i);
    vals[i] = Is_block(p) ? String_val(Field(p, 0)) : NULL;
  }
  return vals;
}

CAMLprim value sqlkit_pq_send_query_params(value conn, value sql, value params)
{
  int n = (int)Wosize_val(params);
  const char **vals = borrow_params(params, n);
  int rc = PQsendQueryParams(Conn_val(conn), String_val(sql), n, NULL, vals, NULL, NULL, 0);
  if (vals) caml_stat_free((void *)vals);
  return Val_bool(rc == 1);
}

CAMLprim value sqlkit_pq_send_query_prepared(value conn, value name, value params)
{
  int n = (int)Wosize_val(params);
  const char **vals = borrow_params(params, n);
  int rc = PQsendQueryPrepared(Conn_val(conn), String_val(name), n, vals, NULL, NULL, 0);
  if (vals) caml_stat_free((void *)vals);
  return Val_bool(rc == 1);
}

/* 0: everything sent. 1: the socket would block, wait and call again. -1: failed. */
CAMLprim value sqlkit_pq_flush(value conn)
{
  return Val_int(PQflush(Conn_val(conn)));
}

CAMLprim value sqlkit_pq_consume_input(value conn)
{
  return Val_bool(PQconsumeInput(Conn_val(conn)) == 1);
}

CAMLprim value sqlkit_pq_is_busy(value conn)
{
  return Val_bool(PQisBusy(Conn_val(conn)) == 1);
}

/* Only called when PQisBusy is false, so it returns without reading the socket. */
CAMLprim value sqlkit_pq_get_result_nb(value conn)
{
  CAMLparam1(conn);
  CAMLreturn(caml_copy_nativeint((intnat)PQgetResult(Conn_val(conn))));
}

/* 1: queued. 0: would block, flush and retry. -1: failed. */
CAMLprim value sqlkit_pq_put_copy_data_nb(value conn, value data)
{
  return Val_int(
    PQputCopyData(Conn_val(conn), String_val(data), (int)caml_string_length(data)));
}

CAMLprim value sqlkit_pq_put_copy_end_nb(value conn)
{
  return Val_int(PQputCopyEnd(Conn_val(conn), NULL));
}

/* Ask the server to stop whatever this connection is running.
 *
 * PQcancel opens a second connection and blocks on it, and before libpq 17
 * there is no nonblocking form. The PGcancel object is self-contained (it holds
 * the address, backend pid and key, not the PGconn), so the request runs on a
 * detached thread that never touches the OCaml runtime, and the caller is free
 * to PQfinish the connection immediately. Best effort: the result is dropped. */
struct cancel_job {
  PGcancel *cancel;
  int report;
};

static void *cancel_thread(void *arg)
{
  struct cancel_job *job = (struct cancel_job *)arg;
  char errbuf[256];
  PQcancel(job->cancel, errbuf, sizeof(errbuf));
  PQfreeCancel(job->cancel);
  /* Tell whoever is waiting that the request is out, then let go of the pipe. */
  (void)!write(job->report, "x", 1);
  close(job->report);
  free(job);
  return NULL;
}

CAMLprim value sqlkit_pq_cancel_detached(value conn, value report)
{
  PGcancel *cancel = PQgetCancel(Conn_val(conn));
  if (cancel == NULL) return Val_false;
  struct cancel_job *job = malloc(sizeof(struct cancel_job));
  if (job == NULL) {
    PQfreeCancel(cancel);
    return Val_false;
  }
  job->cancel = cancel;
  job->report = Int_val(report);
  pthread_t thread;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  int rc = pthread_create(&thread, &attr, cancel_thread, job);
  pthread_attr_destroy(&attr);
  if (rc != 0) {
    PQfreeCancel(cancel);
    free(job);
    return Val_false;
  }
  return Val_true;
}
