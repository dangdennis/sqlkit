(** Events contain no SQL, parameters, decoded values, or server error messages. Observers
    should enqueue events promptly; ordinary observer exceptions are ignored. *)
type kind = Query | Transaction | Pool_wait | Cleanup

type failure =
  | Connect
  | Execute
  | Encode
  | Decode
  | Cardinality
  | Invalid_argument
  | Callback
  | Timeout
  | Connection_unusable
  | Cleanup_failure

type outcome =
  | Success
  | Failure of { kind : failure; sqlstate : Sqlstate.t option }
  | Cancelled

type event = { kind : kind; name : string; duration_s : float; outcome : outcome }
type observer = event -> unit

val ignore : observer
