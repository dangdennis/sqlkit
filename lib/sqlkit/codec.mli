(** Codec failures are values. Match [reason] when useful, or use {!pp} to handle all
    failures generically. Message wording is not a machine-readable contract. *)
type dimension_problem =
  | Invalid_extent
  | Too_many_dimensions
  | Element_count_mismatch of { expected : int; actual : int }
  | Ragged
  | Bounds_mismatch
  | Not_a_list

type reason =
  | Invalid_syntax of { format : string; detail : string; offset : int option }
  | Invalid_value of { expected : string; value : string }
  | Out_of_range of { expected : string; value : string }
  | Unexpected_null
  | Type_mismatch of { expected : string; actual : string }
  | Missing_column of { index : int; count : int }
  | Missing_field of { index : int; count : int }
  | Field_count of { type_name : string; expected : int; actual : int }
  | Unknown_enum of { type_name : string; value : string; allowed : string list }
  | Invalid_dimensions of dimension_problem
  | Invalid_delimiter of char
  | Inconsistent_parameters of string list
  | Nul_byte
  | Custom of string

type segment =
  | Column of { index : int; name : string option }
  | Parameter of string
  | Parameter_index of int
  | Field of { index : int; name : string option }
  | Array_index of int list
  | Multirange_index of int
  | Lower_bound
  | Upper_bound
  | Copy_row of int
      (** Paths run from outermost to innermost. Column, field, parameter, multirange and
          COPY row indexes are zero-based. [Array_index] contains PostgreSQL subscripts,
          which may be negative or start above one. *)

type error = { reason : reason; path : segment list }

val error : reason -> error

val custom : string -> error
(** Adapt a custom codec's human-readable diagnostic without inventing a reason. *)

val syntax : ?offset:int -> string -> string -> error
(** Format name, detail and optional zero-based byte offset within the containing textual
    value (not necessarily the outermost SQL value). *)

val invalid_value : string -> string -> error
(** Expected representation and rejected value. *)

val to_string : error -> string
val pp : Format.formatter -> error -> unit
val context : segment -> ('a, error) result -> ('a, error) result

val text : string -> (string, error) result
(** Reject NUL in PostgreSQL text. Binary bytea is handled separately. *)

exception Codec_error of error

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val of_option : 'e -> 'a option -> ('a, 'e) result
val traverse : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
val traversei : (int -> 'a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
val option : ('a -> ('b, 'e) result) -> 'a option -> ('b option, 'e) result
val get_exn : ('a, error) result -> 'a
