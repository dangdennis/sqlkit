type t = {
  statement_timeout_ms : int;
  lock_timeout_ms : int;
  idle_transaction_timeout_ms : int;
}

let default =
  {
    statement_timeout_ms = 30000;
    lock_timeout_ms = 5000;
    idle_transaction_timeout_ms = 60000;
  }

let unlimited =
  { statement_timeout_ms = 0; lock_timeout_ms = 0; idle_transaction_timeout_ms = 0 }

let validate t =
  if
    List.exists
      (fun n -> n < 0 || n > 2147483647)
      [ t.statement_timeout_ms; t.lock_timeout_ms; t.idle_transaction_timeout_ms ]
  then Error "server timeouts must be between 0 and 2147483647 milliseconds"
  else Ok ()

let statements t =
  List.map
    (fun (name, n) -> Printf.sprintf "SET %s TO '%dms'" name n)
    [
      ("statement_timeout", t.statement_timeout_ms);
      ("lock_timeout", t.lock_timeout_ms);
      ("idle_in_transaction_session_timeout", t.idle_transaction_timeout_ms);
    ]
