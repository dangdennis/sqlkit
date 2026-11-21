val quote : string -> string

val token :
  string -> int ref -> stop:(char -> bool) -> (string * bool, Codec.error) result

val context : Codec.segment -> ('a, Codec.error) result -> ('a, Codec.error) result
