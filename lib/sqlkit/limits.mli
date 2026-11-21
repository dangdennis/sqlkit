type t = {
  statement_timeout_ms : int;
  lock_timeout_ms : int;
  idle_transaction_timeout_ms : int;
}
(** Server-side limits applied to each new connection. Zero disables a limit. *)

val default : t
val unlimited : t
val validate : t -> (unit, string) result
val statements : t -> string list
