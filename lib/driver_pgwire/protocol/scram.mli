(** Client side of SCRAM-SHA-256 (RFC 5802, RFC 7677) as PostgreSQL uses it, without
    channel binding, and the legacy MD5 response. Pure: the caller supplies the nonce. *)

val mechanism : string

val client_first : nonce:string -> string
(** [nonce] must be printable ASCII without commas; base64 of random bytes is the usual
    choice. *)

type expected
(** The server signature a genuine server will send. *)

val max_iterations : int
(** A challenge asking for more is refused. *)

val client_final :
  ?pause:(unit -> unit) ->
  password:string ->
  nonce:string ->
  server_first:string ->
  unit ->
  (string * expected, string) result
(** The client's final message. Fails when the server's first message is malformed, does
    not extend our nonce, names an extension, or asks for an unreasonable iteration count.
    [pause] is called every few thousand iterations of the key derivation, so a
    cooperative scheduler can run and a deadline can fire. *)

val verify : expected -> server_final:string -> bool
(** Constant-time check that the server knew the password verifier. *)

val md5 : user:string -> password:string -> salt:string -> string
