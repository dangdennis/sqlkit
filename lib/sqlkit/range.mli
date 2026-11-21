(** PostgreSQL ranges and multiranges. Constructors are public and preserve supplied
    bounds. Construction does not validate endpoint ordering or canonicalize values:
    PostgreSQL owns those subtype-dependent rules. A constructed value can therefore be
    rejected by the server; successful encoding does not imply server validity. Multirange
    lists likewise need not be sorted or merged before submission.

    Element callbacks return codec results; bound failures include their location. [_opt]
    adapters discard diagnostics; [_exn] adapters raise [Codec.Codec_error]. *)

type 'a bound = Unbounded | Inclusive of 'a | Exclusive of 'a
type 'a t = Empty | Bounds of { lower : 'a bound; upper : 'a bound }

val of_string :
  (string -> ('a, Codec.error) result) -> string -> ('a t, Codec.error) result

val of_string_opt : (string -> ('a, Codec.error) result) -> string -> 'a t option
val of_string_exn : (string -> ('a, Codec.error) result) -> string -> 'a t

val to_string :
  ('a -> (string, Codec.error) result) -> 'a t -> (string, Codec.error) result

val multirange_of_string :
  (string -> ('a, Codec.error) result) -> string -> ('a t list, Codec.error) result

val multirange_of_string_opt :
  (string -> ('a, Codec.error) result) -> string -> 'a t list option

val multirange_of_string_exn : (string -> ('a, Codec.error) result) -> string -> 'a t list

val multirange_to_string :
  ('a -> (string, Codec.error) result) -> 'a t list -> (string, Codec.error) result

val to_string_opt : ('a -> (string, Codec.error) result) -> 'a t -> string option
val to_string_exn : ('a -> (string, Codec.error) result) -> 'a t -> string

val multirange_to_string_opt :
  ('a -> (string, Codec.error) result) -> 'a t list -> string option

val multirange_to_string_exn : ('a -> (string, Codec.error) result) -> 'a t list -> string
