(** Composite text codecs. Empty unquoted fields represent SQL NULL. [field] returns an
    error for an invalid index or an element parser error. Element parsers must honor
    their result contract. *)

val of_string : ?names:string array -> string -> (string option array, Codec.error) result
val of_string_opt : ?names:string array -> string -> string option array option
val of_string_exn : ?names:string array -> string -> string option array
val to_string : string option array -> string

val field :
  ?name:string ->
  (string -> ('a, Codec.error) result) ->
  string option array ->
  int ->
  ('a option, Codec.error) result
