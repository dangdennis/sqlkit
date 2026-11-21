(** PostgreSQL frontend/backend protocol 3.0: message layout only, no I/O.

    Builders append one complete message to a buffer. {!decode} turns one framed backend
    message into a value and never raises: anything malformed is an [Error], which the
    driver treats as a dead connection. *)

(** {1 Frontend} *)

val ssl_request : string
val startup : (string * string) list -> string

val cancel_request : pid:int32 -> key:string -> string
(** [key] is the secret exactly as the server sent it in [Backend_key_data]. *)

val password : string -> string
val sasl_initial : mechanism:string -> string -> string
val sasl_response : string -> string
val parse : Buffer.t -> name:string -> sql:string -> unit

val bind : Buffer.t -> statement:string -> string option array -> unit
(** Unnamed portal, text parameters, text results. [None] is NULL. *)

val execute : Buffer.t -> unit
val sync : Buffer.t -> unit
val query : Buffer.t -> string -> unit
val copy_data : Buffer.t -> string -> unit
val copy_done : Buffer.t -> unit
val copy_fail : Buffer.t -> string -> unit

val max_frontend_message : int
(** Largest message PostgreSQL accepts. Builders raise [Invalid_argument] rather than
    write a length or count that does not fit its field; callers check first. *)

val max_parameters : int

val statement_size : sql:string -> string option array -> int
(** Upper bound on the bytes one Parse + Bind for these arguments occupies. *)

(** {1 Backend} *)

type auth =
  | Auth_ok
  | Cleartext
  | Md5 of string  (** four bytes of salt *)
  | Sasl of string list  (** mechanisms offered *)
  | Sasl_continue of string
  | Sasl_final of string
  | Unsupported of int

type transaction = Idle | In_transaction | Failed

type fields = {
  severity : string;
  message : string;
  sqlstate : string option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}

type backend =
  | Authentication of auth
  | Backend_key_data of { pid : int32; key : string }
  | Parameter_status of string * string
  | Ready_for_query of transaction
  | Parse_complete
  | Bind_complete
  | Close_complete
  | No_data
  | Empty_query
  | Portal_suspended
  | Row_description
  | Parameter_description
  | Data_row of Sqlkit.Value.t array
  | Command_complete of string
  | Error_response of fields
  | Notice_response of fields
  | Copy_in_response
  | Copy_out_response
  | Copy_both_response
  | Copy_data
  | Copy_done
  | Notification
  | Negotiate_protocol_version

val max_message : int
(** Largest payload the driver accepts once a session is established, in bytes. *)

val max_startup_message : int
(** Largest payload accepted before authentication has finished. *)

val decode : char -> string -> (backend, string) result
(** [decode tag payload], where [payload] excludes the tag and the length word. *)

val affected : string -> int option
(** Row count from a command tag, for the commands that report one. *)
