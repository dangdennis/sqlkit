(** Connection strings, in the two forms libpq accepts: a [postgresql://] URI or
    [key=value] pairs. Unset fields fall back to the [PG*] environment variables and then
    to defaults.

    Nothing is silently dropped: a parameter or [PG*] variable this driver cannot honour,
    a malformed URI, a repeated key and a NUL byte in any field are all errors. *)

type sslmode = Disable | Prefer | Require | Verify_ca | Verify_full

type auth =
  | Scram_sha_256
  | Md5
  | Password
  | No_authentication
      (** [Password] is the password sent as it is; [No_authentication] is a server that
          asks for nothing. *)

type t = {
  host : string;  (** a name, an address, or a directory holding a Unix socket *)
  port : int;
  user : string;
  password : string option;
  database : string;
  sslmode : sslmode;
  sslrootcert : string option;
  require_auth : auth list option;
      (** The methods the caller accepts, from [require_auth]; [None] when unset. *)
  connect_timeout : float option;
  application_name : string option;
  options : string option;
}

val parse : string -> (t, string) result
(** As in libpq: [sslmode=require] with [sslrootcert] verifies the chain ([verify-ca]);
    [verify-ca] needs [sslrootcert]; [sslrootcert=system] means the system trust store
    with [verify-full]. [sslmode=allow] is read as [prefer]. *)

val of_env : unit -> string
