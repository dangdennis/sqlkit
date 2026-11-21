open Codec

let of_string ?names s =
  let n = String.length s in
  if n < 2 || s.[0] <> '(' || s.[n - 1] <> ')' then
    Error (syntax "composite" "expected parentheses")
  else
    let p = ref 1 in
    let rec fields index acc =
      let* raw, quoted =
        context
          (Field
             {
               index;
               name =
                 (match names with
                 | Some names when index < Array.length names -> Some names.(index)
                 | _ -> None);
             })
          (Pg_text.token s p ~stop:(fun c -> c = ',' || c = ')'))
      in
      let value = if raw = "" && not quoted then None else Some raw in
      if !p = n - 1 then Ok (Array.of_list (List.rev (value :: acc)))
      else if !p < n - 1 && s.[!p] = ',' then (
        incr p;
        fields (index + 1) (value :: acc))
      else Error (syntax ~offset:!p "composite" "trailing or malformed text")
    in
    fields 0 []

let of_string_opt ?names s = Result.to_option (of_string ?names s)
let of_string_exn ?names s = get_exn (of_string ?names s)

let to_string xs =
  "("
  ^ String.concat ","
      (Array.to_list (Array.map (function None -> "" | Some s -> Pg_text.quote s) xs))
  ^ ")"

let field ?name parse xs i =
  if i < 0 || i >= Array.length xs then
    context
      (Field { index = i; name })
      (Error (error (Missing_field { index = i; count = Array.length xs })))
  else context (Field { index = i; name }) (option parse xs.(i))
