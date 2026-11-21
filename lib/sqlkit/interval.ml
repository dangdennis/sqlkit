(* See interval.mli for the representation and validation contract. *)
open Codec

type t = { months : int; days : int; micros : int64 }

let zero = { months = 0; days = 0; micros = 0L }
let range expected value = Error (error (Out_of_range { expected; value }))

let add expected a b =
  if (b > 0L && a > Int64.sub Int64.max_int b) || (b < 0L && a < Int64.sub Int64.min_int b)
  then range expected (Printf.sprintf "%Ld + %Ld" a b)
  else Ok (Int64.add a b)

let scale expected n factor =
  if n > Int64.div Int64.max_int factor || n < Int64.div Int64.min_int factor then
    range expected (Printf.sprintf "%Ld * %Ld" n factor)
  else Ok (Int64.mul n factor)

let component name n =
  if n < -2147483648L || n > 2147483647L then range name (Int64.to_string n)
  else Ok (Int64.to_int n)

let finish months days micros =
  let* months = component "interval months (signed 32-bit)" months in
  let* days = component "interval days (signed 32-bit)" days in
  (* PostgreSQL reserves the two all-extreme tuples for infinity. This type
     models finite intervals, so those tuples must not slip through as finite. *)
  if
    (months = -2147483648 && days = -2147483648 && micros = Int64.min_int)
    || (months = 2147483647 && days = 2147483647 && micros = Int64.max_int)
  then range "finite interval" "reserved infinity representation"
  else Ok { months; days; micros }

let make ?(years = 0) ?(months = 0) ?(days = 0) ?(hours = 0) ?(minutes = 0) ?(seconds = 0)
    ?(micros = 0L) () =
  let* years = scale "interval months" (Int64.of_int years) 12L in
  let* months = add "interval months" years (Int64.of_int months) in
  let* hours = scale "interval microseconds" (Int64.of_int hours) 3600000000L in
  let* minutes = scale "interval microseconds" (Int64.of_int minutes) 60000000L in
  let* seconds = scale "interval microseconds" (Int64.of_int seconds) 1000000L in
  let* time = add "interval microseconds" hours minutes in
  let* time = add "interval microseconds" time seconds in
  let* time = add "interval microseconds" time micros in
  finish months (Int64.of_int days) time

let make_exn ?years ?months ?days ?hours ?minutes ?seconds ?micros () =
  get_exn (make ?years ?months ?days ?hours ?minutes ?seconds ?micros ())

let make_opt ?years ?months ?days ?hours ?minutes ?seconds ?micros () =
  Result.to_option (make ?years ?months ?days ?hours ?minutes ?seconds ?micros ())

let equal a b = a.months = b.months && a.days = b.days && a.micros = b.micros

(* One field for the whole time part. PostgreSQL 15 and later read it as 64
   bits; earlier versions read 32, which is one reason 15 is the oldest
   supported version. *)
let to_string t =
  Printf.sprintf "%d mons %d days %Ld microseconds" t.months t.days t.micros

let pp fmt t = Format.pp_print_string fmt (to_string t)
let digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let unsigned s =
  if s <> "" && (s.[0] = '-' || s.[0] = '+') then
    (s.[0] = '-', String.sub s 1 (String.length s - 1))
  else (false, s)

let integer ?(negate = false) s =
  let negative, raw = unsigned s in
  if not (digits raw) then Error (invalid_value "interval integer" s)
  else
    let signed = (if negative <> negate then "-" else "") ^ raw in
    match Int64.of_string_opt signed with
    | Some n -> Ok n
    | None -> range "interval integer (signed 64-bit)" signed

(* Parse decimal seconds exactly. PostgreSQL's output has at most six decimal
   places; extra nonzero digits are rejected instead of silently rounded. *)
let seconds ?(negate = false) s =
  let negative, raw = unsigned s in
  let negative = negative <> negate in
  let sign s = (if negative then "-" else "") ^ s in
  let* whole, fraction =
    match String.split_on_char '.' raw with
    | [ whole ] when digits whole -> Ok (whole, "")
    | [ whole; fraction ] when (whole = "" || digits whole) && digits fraction ->
        if
          String.length fraction > 6
          && not
               (String.for_all (( = ) '0')
                  (String.sub fraction 6 (String.length fraction - 6)))
        then Error (invalid_value "interval seconds with microsecond precision" s)
        else
          Ok
            ( (if whole = "" then "0" else whole),
              String.sub fraction 0 (min 6 (String.length fraction)) )
    | _ -> Error (invalid_value "interval seconds" s)
  in
  let* whole = integer (sign whole) in
  let* whole = scale "interval microseconds" whole 1000000L in
  let* fraction =
    integer (sign (fraction ^ String.make (6 - String.length fraction) '0'))
  in
  add "interval microseconds" whole fraction

let clock ~negate s =
  let negative, raw = unsigned s in
  let negate = negative <> negate in
  let* h, m, sec =
    match String.split_on_char ':' raw with
    | [ h; m ] -> Ok (h, m, "0")
    | [ h; m; sec ] -> Ok (h, m, sec)
    | _ -> Error (syntax "interval" "invalid clock")
  in
  let* () =
    if digits h && digits m && sec <> "" && sec.[0] <> '-' && sec.[0] <> '+' then Ok ()
    else Error (invalid_value "unsigned clock components" s)
  in
  let* minutes = integer m in
  let* sec_check = seconds sec in
  if minutes > 59L || sec_check >= 60000000L then
    Error (invalid_value "clock minutes/seconds below 60" s)
  else
    let* h = integer ~negate h in
    let* h = scale "interval microseconds" h 3600000000L in
    let* m = integer ~negate m in
    let* m = scale "interval microseconds" m 60000000L in
    let* sec = seconds ~negate sec in
    let* time = add "interval microseconds" h m in
    add "interval microseconds" time sec

(* Parser accumulators use int64 even for months/days. Bounds are validated
   before exposing a value; no parser path can construct an unchecked [t]. *)
type parts = { month_part : int64; day_part : int64; time_part : int64 }

let empty = { month_part = 0L; day_part = 0L; time_part = 0L }

let time t delta =
  let* n = add "interval microseconds" t.time_part delta in
  Ok { t with time_part = n }

let months t delta =
  let* n = add "interval months" t.month_part delta in
  Ok { t with month_part = n }

let days t delta =
  let* n = add "interval days" t.day_part delta in
  Ok { t with day_part = n }

let add_unit ~negate t unit raw =
  match String.lowercase_ascii unit with
  | "sec" | "secs" | "second" | "seconds" ->
      let* n = seconds ~negate raw in
      time t n
  | unit -> (
      let* n = integer ~negate raw in
      let scaled factor f expected =
        let* n = scale expected n factor in
        f t n
      in
      match unit with
      | "year" | "years" | "yr" | "yrs" -> scaled 12L months "interval months"
      | "mon" | "mons" | "month" | "months" -> months t n
      | "week" | "weeks" -> scaled 7L days "interval days"
      | "day" | "days" -> days t n
      | "hour" | "hours" -> scaled 3600000000L time "interval microseconds"
      | "min" | "mins" | "minute" | "minutes" ->
          scaled 60000000L time "interval microseconds"
      | "microsecond" | "microseconds" -> time t n
      | _ -> Error (invalid_value "interval unit" unit))

let year_month ~negate s =
  let negative, raw = unsigned s in
  match String.split_on_char '-' raw with
  | [ y; m ] when digits y && digits m ->
      let negate = negative <> negate in
      let* y = integer ~negate y in
      let* m = integer ~negate m in
      let* y = scale "interval months" y 12L in
      let* n = add "interval months" y m in
      Ok (Some n)
  | _ -> Ok None

let of_iso s =
  let n = String.length s in
  let rec go i in_time needs_component seen t =
    if i = n then
      if needs_component || not seen then Error (syntax "interval" "empty ISO component")
      else Ok t
    else if s.[i] = 'T' || s.[i] = 't' then
      if in_time then Error (syntax ~offset:i "interval" "duplicate ISO time marker")
      else go (i + 1) true true seen t
    else
      let j = ref i in
      while
        !j < n && match s.[!j] with '0' .. '9' | '-' | '+' | '.' -> true | _ -> false
      do
        incr j
      done;
      if !j = i || !j = n then Error (syntax ~offset:i "interval" "invalid ISO component")
      else
        let raw = String.sub s i (!j - i) in
        let* unit =
          match (Char.uppercase_ascii s.[!j], in_time) with
          | 'Y', false -> Ok "years"
          | 'M', false -> Ok "months"
          | 'W', false -> Ok "weeks"
          | 'D', false -> Ok "days"
          | 'H', true -> Ok "hours"
          | 'M', true -> Ok "minutes"
          | 'S', true -> Ok "seconds"
          | _ -> Error (syntax ~offset:!j "interval" "invalid ISO unit")
        in
        let* t = add_unit ~negate:false t unit raw in
        go (!j + 1) in_time false true t
  in
  go 1 false true false empty

let parse s =
  if s = "" then Error (syntax "interval" "empty input")
  else if
    String.lowercase_ascii s = "infinity"
    || String.lowercase_ascii s = "-infinity"
    || String.lowercase_ascii s = "+infinity"
  then range "finite interval" s
  else if s.[0] = 'P' || s.[0] = 'p' then of_iso s
  else
    let s =
      if s.[0] = '@' then String.trim (String.sub s 1 (String.length s - 1)) else s
    in
    let toks = String.split_on_char ' ' s |> List.filter (( <> ) "") in
    let toks, ago =
      match List.rev toks with "ago" :: xs -> (List.rev xs, true) | _ -> (toks, false)
    in
    (* SQL-standard output can use one leading minus for the whole day/time
       or year/month group. Mixed signs are printed explicitly. *)
    let toks, negate =
      match toks with
      | first :: rest
        when first <> ""
             && first.[0] = '-'
             && (not
                   (String.exists
                      (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)
                      s))
             && List.for_all (fun x -> x.[0] <> '+' && x.[0] <> '-') rest ->
          (String.sub first 1 (String.length first - 1) :: rest, not ago)
      | _ -> (toks, ago)
    in
    let rec go t = function
      | [] -> Ok t
      | tok :: rest -> (
          if String.contains tok ':' then
            let* n = clock ~negate tok in
            let* t = time t n in
            go t rest
          else
            let* ym = year_month ~negate tok in
            match ym with
            | Some n ->
                let* t = months t n in
                go t rest
            | None -> (
                match rest with
                | unit :: xs when not (String.contains unit ':') ->
                    let* t = add_unit ~negate t unit tok in
                    go t xs
                | _ ->
                    let* n = integer ~negate tok in
                    let* t = days t n in
                    go t rest))
    in
    if toks = [] then Error (syntax "interval" "empty input") else go empty toks

let of_string input =
  let* t = parse (String.trim input) in
  finish t.month_part t.day_part t.time_part

let of_string_opt s = Result.to_option (of_string s)
let of_string_exn s = get_exn (of_string s)
