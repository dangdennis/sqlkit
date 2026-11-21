(** PostgreSQL [interval] values: three independent fields, because a month has no fixed
    length and a day is not always 24 hours. Parsing accepts all four [IntervalStyle]
    output formats; printing emits a unit-spelled form the server accepts under any style.
*)

type t = private { months : int; days : int; micros : int64 }

val zero : t

val make :
  ?years:int ->
  ?months:int ->
  ?days:int ->
  ?hours:int ->
  ?minutes:int ->
  ?seconds:int ->
  ?micros:int64 ->
  unit ->
  (t, Codec.error) result

(** Check all arithmetic and PostgreSQL component bounds. Months/days are signed 32-bit;
    microseconds are signed 64-bit. Reserved infinity tuples are rejected. Intermediate
    overflow is rejected even when later terms could cancel it. Components retain their
    signs and are not normalized across months/days/time. *)

val make_opt :
  ?years:int ->
  ?months:int ->
  ?days:int ->
  ?hours:int ->
  ?minutes:int ->
  ?seconds:int ->
  ?micros:int64 ->
  unit ->
  t option

val make_exn :
  ?years:int ->
  ?months:int ->
  ?days:int ->
  ?hours:int ->
  ?minutes:int ->
  ?seconds:int ->
  ?micros:int64 ->
  unit ->
  t

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit

val of_string : string -> (t, Codec.error) result
(** Parse finite intervals with the same bounds as [make]. Fractional seconds are exact:
    digits beyond six decimal places must be zero. *)

val of_string_opt : string -> t option
val of_string_exn : string -> t
