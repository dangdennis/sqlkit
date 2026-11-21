open Sqlkit

let check name b = if not b then failwith name
let ok = function Ok x -> x | Error _ -> failwith "unexpected error"
let text s = Ok s

let () =
  check "negative row index" (Result.is_error (Row.int [| Value.Int 1 |] (-1)));
  check "missing row column" (Result.is_error (Row.string [||] 0));
  check "nullable missing column is not absence"
    (Result.is_error (Row.option Row.string [||] 0));
  check "nullable NULL is absence" (Row.option Row.string [| Value.Null |] 0 = Ok None);
  List.iter
    (fun s -> check "bad scalar" (Result.is_error (Row.Elem.int s)))
    [ ""; "bad"; "99999999999999999999999" ];
  check "bad JSON" (Result.is_error (Row.Elem.json "{"));
  check "bad decimal" (Result.is_error (Row.Elem.decimal "garbage"));
  check "oversized byte escape" (Result.is_error (Row.Elem.octets "\\777"));
  check "bad byte hex" (Result.is_error (Row.Elem.octets "\\xzz"));
  List.iter
    (fun s -> check "invalid time" (Result.is_error (Row.Elem.time_of_day s)))
    [ "-1:00:00"; "24:00:01"; "00:60:00"; "00:00:nan" ];
  let inner s = Composite.of_string s in
  let bad = Pg_array.of_string inner "{\"(a\"}" in
  check "nested failure carries path"
    (match bad with
    | Error { path = Codec.Array_index [ 1 ] :: _; _ } -> true
    | Error _ -> false
    | Ok _ -> false);
  let a = Pg_array.of_list [ 1 ] in
  check "printer failure is returned"
    (Pg_array.to_string (fun _ -> Error (Codec.custom "cannot encode")) a
    = Codec.context (Codec.Array_index [ 1 ]) (Error (Codec.custom "cannot encode")));
  check "range printer failure has context"
    (Range.to_string
       (fun _ -> Error (Codec.custom "cannot encode"))
       (Range.Bounds { lower = Range.Inclusive 1; upper = Range.Unbounded })
    = Codec.context Codec.Lower_bound (Error (Codec.custom "cannot encode")));
  check "result and option agree"
    (Pg_array.of_string_opt text "{a}" = Result.to_option (Pg_array.of_string text "{a}"));
  check "option discards parse failure" (Pg_array.of_string_opt text "{" = None);
  check "explicit exception wrapper"
    (match Pg_array.of_string_exn text "{" with
    | exception Codec.Codec_error _ -> true
    | _ -> false);
  check "successful exception wrapper"
    (Pg_array.of_string_exn text "{a}" = ok (Pg_array.of_string text "{a}"));
  (* Bounded deterministic malformed-input coverage: no result parser may escape. *)
  let state = Random.State.make [| 91; 17 |] in
  let alphabet = "{}[](),\\\"NULL012-+:PT xyz" in
  for _ = 1 to 10000 do
    let s =
      String.init (Random.State.int state 40) (fun _ ->
          alphabet.[Random.State.int state (String.length alphabet)])
    in
    ignore (Pg_array.of_string text s);
    ignore (Composite.of_string s);
    ignore (Range.of_string text s);
    ignore (Range.multirange_of_string text s);
    ignore (Interval.of_string s)
  done;
  print_endline "result contract: all good"
