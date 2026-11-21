open Codec

type 'a bound = Unbounded | Inclusive of 'a | Exclusive of 'a
type 'a t = Empty | Bounds of { lower : 'a bound; upper : 'a bound }

let read parse s p =
  let n = String.length s in
  if !p + 5 <= n && String.sub s !p 5 = "empty" then (
    p := !p + 5;
    Ok Empty)
  else
    begin if !p >= n || (s.[!p] <> '[' && s.[!p] <> '(') then
      Error (syntax ~offset:!p "range" "opening bound")
    else
      let inclusive = s.[!p] = '[' in
      incr p;
      let* lo, lq = context Lower_bound (Pg_text.token s p ~stop:(( = ) ',')) in
      if !p >= n || s.[!p] <> ',' then Error (syntax ~offset:!p "range" "separator")
      else begin
        incr p;
        let* hi, hq =
          context Upper_bound (Pg_text.token s p ~stop:(fun c -> c = ']' || c = ')'))
        in
        if !p >= n then Error (syntax ~offset:!p "range" "closing bound")
        else begin
          let upper_inclusive = s.[!p] = ']' in
          incr p;
          let bound raw quoted inc =
            if raw = "" && not quoted then Ok Unbounded
            else
              let* v = parse raw in
              Ok (if inc then Inclusive v else Exclusive v)
          in
          let* lower = context Lower_bound (bound lo lq inclusive) in
          let* upper = context Upper_bound (bound hi hq upper_inclusive) in
          Ok (Bounds { lower; upper })
        end
      end
    end

let of_string parse s =
  let s = String.trim s in
  let p = ref 0 in
  let* x = read parse s p in
  if !p <> String.length s then Error (syntax ~offset:!p "range" "trailing text")
  else Ok x

let of_string_opt parse s = Result.to_option (of_string parse s)
let of_string_exn parse s = get_exn (of_string parse s)

let to_string print = function
  | Empty -> Ok "empty"
  | Bounds { lower; upper } ->
      let text = function
        | Unbounded -> Ok ""
        | Inclusive x | Exclusive x ->
            Result.map Pg_text.quote (Result.bind (print x) Codec.text)
      in
      let* lo = context Lower_bound (text lower) in
      let* hi = context Upper_bound (text upper) in
      Ok
        ((match lower with Inclusive _ -> "[" | _ -> "(")
        ^ lo ^ "," ^ hi
        ^ match upper with Inclusive _ -> "]" | _ -> ")")

let multirange_of_string parse s =
  let s = String.trim s in
  let n = String.length s in
  if n < 2 || s.[0] <> '{' || s.[n - 1] <> '}' then
    Error (syntax "multirange" "expected braces")
  else
    let p = ref 1 in
    if n = 2 then Ok []
    else
      let rec loop index acc =
        let* x = context (Multirange_index index) (read parse s p) in
        if !p = n - 1 then Ok (List.rev (x :: acc))
        else if !p < n - 1 && s.[!p] = ',' then (
          incr p;
          loop (index + 1) (x :: acc))
        else Error (syntax ~offset:!p "multirange" "separator or trailing text")
      in
      loop 0 []

let multirange_to_string print xs =
  let* xs = traversei (fun i x -> context (Multirange_index i) (to_string print x)) xs in
  Ok ("{" ^ String.concat "," xs ^ "}")

let multirange_of_string_opt parse s = Result.to_option (multirange_of_string parse s)
let multirange_of_string_exn parse s = get_exn (multirange_of_string parse s)
let to_string_opt print value = Result.to_option (to_string print value)
let to_string_exn print value = get_exn (to_string print value)

let multirange_to_string_opt print value =
  Result.to_option (multirange_to_string print value)

let multirange_to_string_exn print value = get_exn (multirange_to_string print value)
