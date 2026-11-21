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

let ignore _ = ()
