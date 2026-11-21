(** Connection strings, in the two forms libpq accepts: a [postgresql://] URI or
    [key=value] pairs. Unset fields fall back to the [PG*] environment variables and then
    to defaults. A parameter this driver cannot honour is an error, never ignored. *)

type sslmode = Disable | Prefer | Require | Verify_ca | Verify_full

type t = {
  host : string;  (** a name, an address, or a directory holding a Unix socket *)
  port : int;
  user : string;
  password : string option;
  database : string;
  sslmode : sslmode;
  sslrootcert : string option;
  application_name : string option;
  options : string option;
}

val parse : string -> (t, string) result
val of_env : unit -> string
