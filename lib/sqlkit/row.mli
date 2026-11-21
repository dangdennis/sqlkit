type t = Value.t array
(** Positional decoders. Invalid values and indexes return structured errors. *)

type error = Codec.error

val named : string -> ('a, error) result -> ('a, error) result
(** Attach the generated column name to its existing index. *)

val pp : Format.formatter -> error -> unit
val to_string : error -> string
val int64 : t -> int -> (int64, error) result
val int : t -> int -> (int, error) result
val bool : t -> int -> (bool, error) result
val string : t -> int -> (string, error) result
val octets : t -> int -> (string, error) result
val float : t -> int -> (float, error) result
val uuid : t -> int -> (Uuidm.t, error) result
val ptime : t -> int -> (Ptime.t, error) result
val decimal : t -> int -> (Decimal.t, error) result
val json : t -> int -> (Yojson.Safe.t, error) result
val date : t -> int -> (Ptime.date, error) result
val time_of_day : t -> int -> (Ptime.Span.t, error) result
val interval : t -> int -> (Interval.t, error) result
val option : (t -> int -> ('a, error) result) -> t -> int -> ('a option, error) result
val custom : (string -> ('a, Codec.error) result) -> t -> int -> ('a, error) result
val list : (string -> ('a, Codec.error) result) -> t -> int -> ('a list, error) result

module Elem : sig
  val int64 : string -> (int64, Codec.error) result
  val int : string -> (int, Codec.error) result
  val bool : string -> (bool, Codec.error) result
  val string : string -> (string, Codec.error) result
  val octets : string -> (string, Codec.error) result
  val float : string -> (float, Codec.error) result
  val uuid : string -> (Uuidm.t, Codec.error) result
  val ptime : string -> (Ptime.t, Codec.error) result
  val decimal : string -> (Decimal.t, Codec.error) result
  val json : string -> (Yojson.Safe.t, Codec.error) result
  val date : string -> (Ptime.date, Codec.error) result
  val time_of_day : string -> (Ptime.Span.t, Codec.error) result
  val interval : string -> (Interval.t, Codec.error) result
end
