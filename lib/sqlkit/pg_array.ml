open Codec

type dimension = { lower : int; length : int }
type 'a t = { dimensions : dimension list; elements : 'a option list }

let make ~dimensions elements =
  let rec size n = function
    | [] -> Ok n
    | d :: ds ->
        if d.length <= 0 || n > max_int / d.length || d.lower > max_int - d.length + 1
        then Error (error (Invalid_dimensions Invalid_extent))
        else size (n * d.length) ds
  in
  if List.length dimensions > 6 then
    Error (error (Invalid_dimensions Too_many_dimensions))
  else
    let* size = size (if dimensions = [] then 0 else 1) dimensions in
    if size <> List.length elements then
      Error
        (error
           (Invalid_dimensions
              (Element_count_mismatch { expected = size; actual = List.length elements })))
    else Ok { dimensions; elements }

let make_exn ~dimensions elements = get_exn (make ~dimensions elements)
let make_opt ~dimensions elements = Result.to_option (make ~dimensions elements)
let dimensions a = a.dimensions
let elements a = a.elements

let of_list xs =
  {
    dimensions = (if xs = [] then [] else [ { lower = 1; length = List.length xs } ]);
    elements = List.map Option.some xs;
  }

let to_list a =
  match a.dimensions with
  | [] | [ { lower = 1; _ } ] ->
      traversei
        (fun i x -> context (Array_index [ i + 1 ]) (of_option (error Unexpected_null) x))
        a.elements
  | _ -> Error (error (Invalid_dimensions Not_a_list))

let to_list_opt a = Result.to_option (to_list a)
let to_list_exn a = get_exn (to_list a)
let valid_delimiter = function '{' | '}' | '"' | '\\' | '\000' -> false | _ -> true

let rec subscripts dimensions index =
  match dimensions with
  | [] -> []
  | d :: rest ->
      let stride = List.fold_left (fun n d -> n * d.length) 1 rest in
      (d.lower + (index / stride)) :: subscripts rest (index mod stride)

let of_string ?(delimiter = ',') parse input =
  let s = String.trim input in
  let n = String.length s and p = ref 0 in
  let expect c =
    if !p >= n || s.[!p] <> c then
      Error (syntax ~offset:!p "array" (Printf.sprintf "expected %C" c))
    else (
      incr p;
      Ok ())
  in
  let integer stop =
    let start = !p in
    while !p < n && s.[!p] <> stop do
      incr p
    done;
    if !p = n then Error (syntax ~offset:!p "array" "missing dimension delimiter")
    else
      of_option
        (invalid_value "array bound" (String.sub s start (!p - start)))
        (int_of_string_opt (String.sub s start (!p - start)))
  in
  let rec bounds acc =
    if !p < n && s.[!p] = '[' then
      if List.length acc >= 6 then Error (error (Invalid_dimensions Too_many_dimensions))
      else begin
        incr p;
        let* lower = integer ':' in
        let* () = expect ':' in
        let* upper = integer ']' in
        let* () = expect ']' in
        if
          upper < lower
          || Int64.sub (Int64.of_int upper) (Int64.of_int lower) >= Int64.of_int max_int
        then Error (error (Invalid_dimensions Invalid_extent))
        else bounds ({ lower; length = upper - lower + 1 } :: acc)
      end
    else Ok (List.rev acc)
  in
  let rec array bounds depth prefix =
    if depth > 6 then Error (error (Invalid_dimensions Too_many_dimensions))
    else
      let* () = expect '{' in
      if !p < n && s.[!p] = '}' then (
        incr p;
        Ok ([], []))
      else
        let lower = match bounds with [] -> 1 | d :: _ -> d.lower in
        let rec items shape values count =
          let* dims, xs =
            if !p < n && s.[!p] = '{' then
              array
                (match bounds with [] -> [] | _ :: rest -> rest)
                (depth + 1)
                (prefix @ [ lower + count ])
            else
              let* raw, quoted =
                context
                  (Array_index (prefix @ [ lower + count ]))
                  (Pg_text.token s p ~stop:(fun c -> c = delimiter || c = '}'))
              in
              if (not quoted) && String.trim raw = "" then
                Error (syntax ~offset:!p "array" "missing element")
              else
                let* value =
                  if (not quoted) && String.lowercase_ascii (String.trim raw) = "null"
                  then Ok None
                  else Ok (Some (if quoted then raw else String.trim raw))
                in
                Ok ([], [ value ])
          in
          match shape with
          | Some d when d <> dims -> Error (error (Invalid_dimensions Ragged))
          | _ ->
              let count = count + 1 and values = List.rev_append xs values in
              if !p < n && s.[!p] = delimiter then (
                incr p;
                items (Some dims) values count)
              else
                let* () = expect '}' in
                Ok (count :: dims, List.rev values)
        in
        items None [] 0
  in
  if not (valid_delimiter delimiter) then Error (error (Invalid_delimiter delimiter))
  else
    let* explicit = bounds [] in
    let* () = if explicit <> [] then expect '=' else Ok () in
    let* shape, xs = array explicit 1 [] in
    if !p <> n then Error (syntax ~offset:!p "array" "trailing text")
    else if explicit <> [] && List.map (fun d -> d.length) explicit <> shape then
      Error (error (Invalid_dimensions Bounds_mismatch))
    else
      let* raw =
        make
          ~dimensions:
            (if explicit = [] then List.map (fun length -> { lower = 1; length }) shape
             else explicit)
          xs
      in
      let* elements =
        traversei
          (fun i x ->
            context (Array_index (subscripts raw.dimensions i)) (option parse x))
          raw.elements
      in
      Ok { raw with elements }

let of_string_opt ?delimiter parse s = Result.to_option (of_string ?delimiter parse s)
let of_string_exn ?delimiter parse s = get_exn (of_string ?delimiter parse s)

let to_string ?(delimiter = ',') print a =
  let index = ref 0 in
  let print_element x =
    let i = !index in
    incr index;
    context (Array_index (subscripts a.dimensions i)) (Result.bind (print x) Codec.text)
  in
  let rec body dims rest =
    match dims with
    | [] -> (
        match rest with
        | [] -> Error (syntax "array" "missing value")
        | None :: xs ->
            incr index;
            Ok ("NULL", xs)
        | Some x :: xs ->
            let* s = print_element x in
            Ok (Pg_text.quote s, xs))
    | d :: ds ->
        let rec items n acc rest =
          if n = 0 then
            Ok ("{" ^ String.concat (String.make 1 delimiter) (List.rev acc) ^ "}", rest)
          else
            let* s, rest = body ds rest in
            items (n - 1) (s :: acc) rest
        in
        items d.length [] rest
  in
  if not (valid_delimiter delimiter) then Error (error (Invalid_delimiter delimiter))
  else if a.dimensions = [] then Ok "{}"
  else
    let* contents, _ = body a.dimensions a.elements in
    let prefix =
      if List.for_all (fun d -> d.lower = 1) a.dimensions then ""
      else
        String.concat ""
          (List.map
             (fun d -> Printf.sprintf "[%d:%d]" d.lower (d.lower + d.length - 1))
             a.dimensions)
        ^ "="
    in
    Ok (prefix ^ contents)

let to_string_opt ?delimiter print value =
  Result.to_option (to_string ?delimiter print value)

let to_string_exn ?delimiter print value = get_exn (to_string ?delimiter print value)
