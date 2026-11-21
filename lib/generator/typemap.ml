(* See typemap.mli. *)

type custom = { c_ocaml : string; c_of_string : string; c_to_string : string }

type t =
  | Int
  | Int64
  | Float
  | Bool
  | String
  | Bytes
  | Uuid
  | Ptime
  | Decimal
  | Json
  | Date
  | Time_of_day
  | Interval
  | Enum of string * string list (* ocaml type name, labels in sort order *)
  | Custom of custom (* from sqlkit.toml *)
  | Array of t * char
  | Composite of string
  | Range of t
  | Multirange of t
  | Option of t

let rec ocaml_type = function
  | Int -> "int"
  | Int64 -> "int64"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "string"
  | Uuid -> "Uuidm.t"
  | Ptime -> "Ptime.t"
  | Decimal -> "Decimal.t"
  | Json -> "Yojson.Safe.t"
  | Date -> "Ptime.date"
  | Time_of_day -> "Ptime.Span.t"
  | Interval -> "Sqlkit.Interval.t"
  | Enum (n, _) -> n
  | Custom c -> c.c_ocaml
  | Array (t, _) -> "(" ^ ocaml_type t ^ ") Sqlkit.Pg_array.t"
  | Composite n -> n
  | Range t -> "(" ^ ocaml_type t ^ ") Sqlkit.Range.t"
  | Multirange t -> "(" ^ ocaml_type t ^ ") Sqlkit.Range.t list"
  | Option t -> ocaml_type t ^ " option"

(* A function of type [Sqlkit.Row.t -> int -> _] *)
let rec decoder = function
  | Int -> "Sqlkit.Row.int"
  | Int64 -> "Sqlkit.Row.int64"
  | Float -> "Sqlkit.Row.float"
  | Bool -> "Sqlkit.Row.bool"
  | String -> "Sqlkit.Row.string"
  | Bytes -> "Sqlkit.Row.octets"
  | Uuid -> "Sqlkit.Row.uuid"
  | Ptime -> "Sqlkit.Row.ptime"
  | Decimal -> "Sqlkit.Row.decimal"
  | Json -> "Sqlkit.Row.json"
  | Date -> "Sqlkit.Row.date"
  | Time_of_day -> "Sqlkit.Row.time_of_day"
  | Interval -> "Sqlkit.Row.interval"
  | Enum (n, _) -> n ^ "_of_row"
  | Custom c -> Printf.sprintf "(Sqlkit.Row.custom %s)" c.c_of_string
  | (Array _ | Composite _ | Range _ | Multirange _) as t ->
      Printf.sprintf "(Sqlkit.Row.custom %s)" (elem_parser t)
  | Option t -> Printf.sprintf "(Sqlkit.Row.option %s)" (decoder t)

(* Array elements arrive as raw text, so they need [string -> _] parsers rather
   than the row-indexing decoders above. *)
and elem_parser = function
  | Int -> "Sqlkit.Row.Elem.int"
  | Int64 -> "Sqlkit.Row.Elem.int64"
  | Float -> "Sqlkit.Row.Elem.float"
  | Bool -> "Sqlkit.Row.Elem.bool"
  | String -> "Sqlkit.Row.Elem.string"
  | Bytes -> "Sqlkit.Row.Elem.octets"
  | Uuid -> "Sqlkit.Row.Elem.uuid"
  | Ptime -> "Sqlkit.Row.Elem.ptime"
  | Decimal -> "Sqlkit.Row.Elem.decimal"
  | Json -> "Sqlkit.Row.Elem.json"
  | Date -> "Sqlkit.Row.Elem.date"
  | Time_of_day -> "Sqlkit.Row.Elem.time_of_day"
  | Interval -> "Sqlkit.Row.Elem.interval"
  | Enum (n, _) -> n ^ "_of_string"
  | Custom c -> c.c_of_string
  | Array (t, delimiter) ->
      Printf.sprintf "(Sqlkit.Pg_array.of_string ~delimiter:%C %s)" delimiter
        (elem_parser t)
  | Composite n -> n ^ "_of_string"
  | Range t -> Printf.sprintf "(Sqlkit.Range.of_string %s)" (elem_parser t)
  | Multirange t ->
      Printf.sprintf "(Sqlkit.Range.multirange_of_string %s)" (elem_parser t)
  | Option t -> elem_parser t

(* A function of type [_ -> Sqlkit.Value.t] *)
let rec encoder t =
  match t with
  | Int -> "(fun x -> Ok (Sqlkit.Value.of_int x))"
  | Int64 -> "(fun x -> Ok (Sqlkit.Value.of_int64 x))"
  | Float -> "(fun x -> Ok (Sqlkit.Value.of_float x))"
  | Bool -> "(fun x -> Ok (Sqlkit.Value.of_bool x))"
  | String -> "(fun x -> Ok (Sqlkit.Value.of_string x))"
  | Bytes -> "(fun x -> Ok (Sqlkit.Value.of_octets x))"
  | Uuid -> "(fun x -> Ok (Sqlkit.Value.of_uuid x))"
  | Ptime -> "(fun x -> Ok (Sqlkit.Value.of_ptime x))"
  | Decimal -> "(fun x -> Ok (Sqlkit.Value.of_decimal x))"
  | Json -> "(fun x -> Ok (Sqlkit.Value.of_json x))"
  | Date -> "(fun x -> Ok (Sqlkit.Value.of_date x))"
  | Time_of_day -> "(fun x -> Ok (Sqlkit.Value.of_time_of_day x))"
  | Interval -> "(fun x -> Ok (Sqlkit.Value.of_interval x))"
  | Enum (n, _) -> "(fun x -> Ok (" ^ n ^ "_to_value x))"
  | Custom c ->
      Printf.sprintf "(fun x -> Result.map Sqlkit.Value.of_string (%s x))" c.c_to_string
  | (Array _ | Composite _ | Range _ | Multirange _) as t ->
      Printf.sprintf "(fun x -> Result.map Sqlkit.Value.of_string (%s x))"
        (elem_printer t)
  | Option t ->
      Printf.sprintf
        "(fun x -> match x with None -> Ok Sqlkit.Value.Null | Some x -> %s x)"
        (encoder t)

and elem_printer = function
  | Int -> "(fun x -> Ok (Sqlkit.Value.Print.int x))"
  | Int64 -> "(fun x -> Ok (Sqlkit.Value.Print.int64 x))"
  | Float -> "(fun x -> Ok (Sqlkit.Value.Print.float x))"
  | Bool -> "(fun x -> Ok (Sqlkit.Value.Print.bool x))"
  | String -> "Sqlkit.Codec.text"
  | Bytes -> "(fun x -> Ok (Sqlkit.Value.Print.octets x))"
  | Uuid -> "(fun x -> Ok (Sqlkit.Value.Print.uuid x))"
  | Ptime -> "(fun x -> Ok (Sqlkit.Value.Print.ptime x))"
  | Decimal -> "(fun x -> Ok (Sqlkit.Value.Print.decimal x))"
  | Json -> "(fun x -> Ok (Sqlkit.Value.Print.json x))"
  | Date -> "(fun x -> Ok (Sqlkit.Value.Print.date x))"
  | Time_of_day -> "(fun x -> Ok (Sqlkit.Value.Print.time_of_day x))"
  | Interval -> "(fun x -> Ok (Sqlkit.Value.Print.interval x))"
  | Enum (n, _) -> "(fun x -> Ok (" ^ n ^ "_to_string x))"
  | Custom c ->
      Printf.sprintf "(fun x -> Result.bind (%s x) Sqlkit.Codec.text)" c.c_to_string
  | Array (t, delimiter) ->
      Printf.sprintf "(Sqlkit.Pg_array.to_string ~delimiter:%C %s)" delimiter
        (elem_printer t)
  | Composite n -> n ^ "_to_string"
  | Range t -> Printf.sprintf "(Sqlkit.Range.to_string %s)" (elem_printer t)
  | Multirange t ->
      Printf.sprintf "(Sqlkit.Range.multirange_to_string %s)" (elem_printer t)
  | Option t -> elem_printer t

let is_option = function Option _ -> true | _ -> false
let strip_option = function Option t -> t | t -> t

(* int8 spans the full signed 64-bit PostgreSQL range. *)
let base_of_pg_name = function
  | "bool" -> Some Bool
  | "int2" | "int4" -> Some Int
  | "int8" -> Some Int64
  | "float4" | "float8" -> Some Float
  | "numeric" -> Some Decimal
  | "text" | "varchar" | "bpchar" | "char" | "name" | "citext" -> Some String
  | "bytea" -> Some Bytes
  | "uuid" -> Some Uuid
  | "timestamp" | "timestamptz" -> Some Ptime
  | "date" -> Some Date
  | "time" -> Some Time_of_day
  | "interval" -> Some Interval
  (* timetz is a type even the PostgreSQL docs discourage; it stays textual *)
  | "timetz" -> Some String
  | "json" | "jsonb" -> Some Json
  | "inet" | "cidr" | "macaddr" | "macaddr8" -> Some String
  | _ -> None

(* ---------- enum naming ---------- *)

let sanitize s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') then
        Buffer.add_char b c
      else Buffer.add_char b '_')
    s;
  Buffer.contents b

(* 'active' -> Active, 'in-progress' -> In_progress *)
let constructor_of_label label =
  let s = sanitize (String.lowercase_ascii label) in
  let s = if s = "" then "Empty" else s in
  let s = if s.[0] >= '0' && s.[0] <= '9' then "N" ^ s else s in
  String.make 1 (Char.uppercase_ascii s.[0]) ^ String.sub s 1 (String.length s - 1)

let of_type ?(custom = fun _ -> None) ?(rename = Pg_type.generated_name) pg ~nullable =
  let rec map seen pg =
    match custom pg.Pg_type.id with
    | Some c -> Some (Custom c)
    | None -> (
        if List.mem pg.id seen then None
        else
          let child id = map (pg.id :: seen) (Pg_type.at pg id) in
          match Pg_type.kind pg with
          | Pg_type.Base ->
              if pg.id.schema = "pg_catalog" || pg.id.name = "citext" then
                base_of_pg_name pg.id.name
              else None
          | Pg_type.Enum labels -> Some (Enum (rename pg.id, labels))
          | Pg_type.Domain d -> child d.base
          | Pg_type.Array a ->
              Option.map (fun x -> Array (x, a.delimiter)) (child a.element)
          | Pg_type.Composite _ -> Some (Composite (rename pg.id))
          | Pg_type.Range r -> Option.map (fun t -> Range t) (child r)
          | Pg_type.Multirange r -> (
              match Pg_type.kind (Pg_type.at pg r) with
              | Pg_type.Range sub -> Option.map (fun t -> Multirange t) (child sub)
              | _ -> None)
          | Pg_type.Unsupported _ -> None)
  in
  Option.map (fun t -> if nullable then Option t else t) (map [] pg)
