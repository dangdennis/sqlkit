open Codec

type t = Value.t array
type error = Codec.error

let column i r = context (Column { index = i; name = None }) r

let named name r =
  Result.map_error
    (fun e ->
      match e.path with
      | Column c :: rest -> { e with path = Column { c with name = Some name } :: rest }
      | _ -> e)
    r

let pp = Codec.pp
let to_string = Codec.to_string

let get r i =
  if i >= 0 && i < Array.length r then Ok r.(i)
  else column i (Error (error (Missing_column { index = i; count = Array.length r })))

let normalize_timestamp s =
  let s = String.map (fun c -> if c = ' ' then 'T' else c) s in
  let n = String.length s in
  let rec scan i =
    if i < 10 then `None
    else match s.[i] with '+' | '-' -> `At i | 'Z' | 'z' -> `Zulu | _ -> scan (i - 1)
  in
  match scan (n - 1) with
  | `Zulu -> s
  | `None -> s ^ "Z"
  | `At i -> (
      match n - i with
      | 3 -> s ^ ":00"
      | 5 -> String.sub s 0 (i + 3) ^ ":" ^ String.sub s (i + 3) 2
      | _ -> s)

module Elem = struct
  let string s = Ok s
  let int s = of_option (invalid_value "int" s) (int_of_string_opt s)
  let int64 s = of_option (invalid_value "int64" s) (Int64.of_string_opt s)
  let float s = of_option (invalid_value "float" s) (float_of_string_opt s)

  let bool = function
    | "t" | "true" | "TRUE" | "y" | "1" -> Ok true
    | "f" | "false" | "FALSE" | "n" | "0" -> Ok false
    | value -> Error (invalid_value "bool" value)

  let uuid s = of_option (invalid_value "uuid" s) (Uuidm.of_string s)

  let ptime s =
    match Ptime.of_rfc3339 ~strict:false (normalize_timestamp s) with
    | Ok (t, _, _) -> Ok t
    | Error _ -> Error (invalid_value "timestamp" s)

  (* These dependencies expose exception-only parsers. Translate their documented
     parse failures here; sqlkit parsers themselves propagate results. *)
  let json s =
    try Ok (Yojson.Safe.from_string s)
    with Yojson.Json_error _ -> Error (invalid_value "json" s)

  let decimal s =
    try Ok (Decimal.of_string s)
    with Failure _ | Invalid_argument _ -> Error (invalid_value "numeric" s)

  let date s =
    match String.split_on_char '-' s with
    | [ y; m; d ] ->
        let* y = int y in
        let* m = int m in
        let* d = int d in
        let date = (y, m, d) in
        if Ptime.of_date date = None then Error (invalid_value "date" s) else Ok date
    | _ -> Error (invalid_value "date" s)

  let time_of_day s =
    match String.split_on_char ':' s with
    | [ h; m; sec ] ->
        let* h = int h in
        let* m = int m in
        let* sec = float sec in
        if
          h < 0 || h > 24 || m < 0 || m >= 60
          || (not (Float.is_finite sec))
          || sec < 0. || sec >= 60.
          || (h = 24 && (m <> 0 || sec <> 0.))
        then Error (invalid_value "time" s)
        else
          of_option (invalid_value "time" s)
            (Ptime.Span.of_float_s (float_of_int ((h * 3600) + (m * 60)) +. sec))
    | _ -> Error (invalid_value "time" s)

  let interval = Interval.of_string

  let octets s =
    let n = String.length s and b = Buffer.create (String.length s) in
    if n >= 2 && String.sub s 0 2 = "\\x" then
      if n mod 2 <> 0 then Error (syntax "bytea" "odd hex length")
      else
        let rec hex i =
          if i = n then Ok (Buffer.contents b)
          else
            match int_of_string_opt ("0x" ^ String.sub s i 2) with
            | Some v when v >= 0 && v <= 255 ->
                Buffer.add_char b (Char.chr v);
                hex (i + 2)
            | _ -> Error (syntax "bytea" "invalid hex")
        in
        hex 2
    else
      let rec escape i =
        if i = n then Ok (Buffer.contents b)
        else if s.[i] <> '\\' then (
          Buffer.add_char b s.[i];
          escape (i + 1))
        else if i + 1 < n && s.[i + 1] = '\\' then (
          Buffer.add_char b '\\';
          escape (i + 2))
        else if i + 3 < n then
          let raw = String.sub s (i + 1) 3 in
          if not (String.for_all (fun c -> c >= '0' && c <= '7') raw) then
            Error (syntax "bytea" "invalid octal escape")
          else
            match int_of_string_opt ("0o" ^ raw) with
            | Some v when v <= 255 ->
                Buffer.add_char b (Char.chr v);
                escape (i + 4)
            | _ -> Error (syntax "bytea" "octal escape out of range")
        else Error (syntax "bytea" "incomplete escape")
      in
      escape 0
end

let text expected parse r i =
  let* v = get r i in
  match v with
  | Value.Text s -> column i (parse s)
  | Value.Null -> column i (Error (error Unexpected_null))
  | v -> column i (Error (error (Type_mismatch { expected; actual = Value.type_name v })))

let string = text "text" Elem.string

let int r i =
  let* v = get r i in
  match v with Value.Int n -> Ok n | _ -> text "int" Elem.int r i

let int64 r i =
  let* v = get r i in
  match v with Value.Int n -> Ok (Int64.of_int n) | _ -> text "int64" Elem.int64 r i

let bool r i =
  let* v = get r i in
  match v with Value.Bool b -> Ok b | _ -> text "bool" Elem.bool r i

let octets r i =
  let* v = get r i in
  match v with Value.Octets s -> Ok s | _ -> text "bytea" Elem.octets r i

let float r i =
  let* v = get r i in
  match v with
  | Value.Float f -> Ok f
  | Value.Int n -> Ok (float_of_int n)
  | _ -> text "float" Elem.float r i

let decimal r i =
  let* v = get r i in
  match v with
  | Value.Int n -> Ok (Decimal.of_int n)
  | _ -> text "numeric" Elem.decimal r i

let uuid = text "uuid" Elem.uuid
let ptime = text "timestamp" Elem.ptime
let json = text "json" Elem.json
let date = text "date" Elem.date
let time_of_day = text "time" Elem.time_of_day
let interval = text "interval" Elem.interval
let custom parse = text "custom type" parse

let option decode r i =
  let* v = get r i in
  match v with Value.Null -> Ok None | _ -> Result.map Option.some (decode r i)

let list parse =
  custom (fun s ->
      let* a = Pg_array.of_string parse s in
      Pg_array.to_list a)
