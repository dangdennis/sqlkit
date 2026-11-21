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

let error reason = { reason; path = [] }
let custom message = error (Custom message)
let syntax ?offset format detail = error (Invalid_syntax { format; detail; offset })
let invalid_value expected value = error (Invalid_value { expected; value })
let ( let* ) = Result.bind
let of_option error = function Some x -> Ok x | None -> Error error

let traversei f xs =
  let rec loop i acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
        let* y = f i x in
        loop (i + 1) (y :: acc) xs
  in
  loop 0 [] xs

let traverse f xs = traversei (fun _ -> f) xs
let option f = function None -> Ok None | Some x -> Result.map Option.some (f x)
let context segment r = Result.map_error (fun e -> { e with path = segment :: e.path }) r
let text s = if String.contains s '\000' then Error (error Nul_byte) else Ok s

let reason_to_string = function
  | Invalid_syntax { format; detail; offset } -> (
      format ^ ": " ^ detail
      ^ match offset with None -> "" | Some i -> Printf.sprintf " at byte %d" i)
  | Invalid_value { expected; value } ->
      Printf.sprintf "expected %s, got %S" expected value
  | Out_of_range { expected; value } -> Printf.sprintf "%S is outside %s" value expected
  | Unexpected_null -> "unexpected NULL"
  | Type_mismatch { expected; actual } ->
      Printf.sprintf "expected %s, got %s" expected actual
  | Missing_column { index; count } ->
      Printf.sprintf "column %d is outside a row of %d columns" index count
  | Missing_field { index; count } ->
      Printf.sprintf "field %d is outside a composite of %d fields" index count
  | Field_count { type_name; expected; actual } ->
      Printf.sprintf "%s: expected %d fields, got %d" type_name expected actual
  | Unknown_enum { type_name; value; _ } ->
      Printf.sprintf "%s: unknown enum label %S" type_name value
  | Invalid_dimensions problem -> (
      "array: "
      ^
      match problem with
      | Invalid_extent -> "invalid or overflowing dimensions"
      | Too_many_dimensions -> "more than six dimensions"
      | Element_count_mismatch { expected; actual } ->
          Printf.sprintf "expected %d elements, got %d" expected actual
      | Ragged -> "ragged dimensions"
      | Bounds_mismatch -> "bounds disagree with contents"
      | Not_a_list -> "not a list with lower bound one")
  | Invalid_delimiter c -> Printf.sprintf "invalid array delimiter %C" c
  | Inconsistent_parameters names ->
      "parameters " ^ String.concat ", " names ^ " must be supplied together"
  | Nul_byte -> "PostgreSQL text cannot contain NUL"
  | Custom message -> message

let segment_to_string = function
  | Column { index; name } -> (
      match name with
      | None -> Printf.sprintf "column %d" index
      | Some name -> Printf.sprintf "column %s (%d)" name index)
  | Parameter name -> "parameter " ^ name
  | Parameter_index i -> Printf.sprintf "parameter %d" i
  | Field { index; name } -> (
      match name with
      | None -> Printf.sprintf "field %d" index
      | Some name -> Printf.sprintf "field %s (%d)" name index)
  | Array_index indices ->
      "array" ^ String.concat "" (List.map (Printf.sprintf "[%d]") indices)
  | Multirange_index i -> Printf.sprintf "multirange element %d" i
  | Lower_bound -> "lower bound"
  | Upper_bound -> "upper bound"
  | Copy_row i -> Printf.sprintf "COPY row %d" i

let to_string e =
  String.concat ": " (List.map segment_to_string e.path @ [ reason_to_string e.reason ])

let pp fmt e = Format.pp_print_string fmt (to_string e)

exception Codec_error of error

let get_exn = function Ok x -> x | Error e -> raise (Codec_error e)

let () =
  Printexc.register_printer (function
    | Codec_error e -> Some ("Sqlkit.Codec_error: " ^ to_string e)
    | _ -> None)
