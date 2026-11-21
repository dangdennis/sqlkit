(** Lossless PostgreSQL arrays. Dimensions include lower bounds and elements retain NULLs.
    Invalid dimensions, malformed input and lossy list conversions return [Error]. Element
    parsers and printers must honor their result contract. [_opt] adapters discard
    diagnostics; [_exn] adapters raise [Codec.Codec_error]. *)

type dimension = { lower : int; length : int }
type 'a t

val make : dimensions:dimension list -> 'a option list -> ('a t, Codec.error) result
val make_opt : dimensions:dimension list -> 'a option list -> 'a t option
val make_exn : dimensions:dimension list -> 'a option list -> 'a t
val dimensions : 'a t -> dimension list
val elements : 'a t -> 'a option list
val of_list : 'a list -> 'a t
val to_list : 'a t -> ('a list, Codec.error) result
val to_list_opt : 'a t -> 'a list option
val to_list_exn : 'a t -> 'a list

val of_string :
  ?delimiter:char ->
  (string -> ('a, Codec.error) result) ->
  string ->
  ('a t, Codec.error) result

val of_string_opt :
  ?delimiter:char -> (string -> ('a, Codec.error) result) -> string -> 'a t option

val of_string_exn :
  ?delimiter:char -> (string -> ('a, Codec.error) result) -> string -> 'a t

val to_string :
  ?delimiter:char ->
  ('a -> (string, Codec.error) result) ->
  'a t ->
  (string, Codec.error) result

val to_string_opt :
  ?delimiter:char -> ('a -> (string, Codec.error) result) -> 'a t -> string option

val to_string_exn :
  ?delimiter:char -> ('a -> (string, Codec.error) result) -> 'a t -> string
