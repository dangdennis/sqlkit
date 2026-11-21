(* A domain type, to show sqlkit.toml mapping a column onto it. *)
type t = Uuidm.t

let of_string s =
  match Uuidm.of_string s with
  | Some u -> Ok u
  | None -> Error (Sqlkit.Codec.invalid_value "user id" s)

let to_string u = Ok (Uuidm.to_string u)
let equal = Uuidm.equal
