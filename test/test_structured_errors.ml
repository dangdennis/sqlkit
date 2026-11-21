open Sqlkit
open Generated

let check name condition = if not condition then failwith name
let ok = function Ok x -> x | Error e -> failwith (Codec.to_string e)
let failure = function Error e -> e | Ok _ -> failwith "expected codec failure"

let expect name path reason r =
  let e = failure r in
  check (name ^ ": path") (e.Codec.path = path);
  check (name ^ ": reason") (reason e.reason)

module Recording = struct
  type conn = { mutable calls : int; rows : Value.t array list }

  let now _ = 0.
  let is_cancellation _ = false
  let with_cleanup = Fun.protect
  let close _ = ()

  let query c ~sql:_ ~params:_ ~columns:_ =
    c.calls <- c.calls + 1;
    Ok c.rows

  let exec c ~sql:_ ~params:_ =
    c.calls <- c.calls + 1;
    Ok (Some 0)

  let copy c ~sql:_ ~rows:_ =
    c.calls <- c.calls + 1;
    Ok 0
end

let () =
  expect "missing column"
    [ Codec.Column { index = 2; name = None } ]
    (function Codec.Missing_column { index = 2; count = 0 } -> true | _ -> false)
    (Row.int [||] 2);
  expect "unexpected NULL"
    [ Codec.Column { index = 0; name = Some "count" } ]
    (( = ) Codec.Unexpected_null)
    (Row.named "count" (Row.int [| Value.Null |] 0));
  expect "wire type mismatch"
    [ Codec.Column { index = 0; name = None } ]
    (function Codec.Type_mismatch { expected = "int"; _ } -> true | _ -> false)
    (Row.int [| Value.Bool true |] 0);
  expect "two-dimensional subscripts"
    [ Codec.Array_index [ -1; 5 ] ]
    (function
      | Codec.Invalid_value { expected = "int"; value = "bad" } -> true | _ -> false)
    (Pg_array.of_string Row.Elem.int "[-2:-1][4:5]={{1,2},{3,bad}}");
  expect "container syntax offset" [ Codec.Array_index [ 1 ] ]
    (function Codec.Invalid_syntax { offset = Some _; _ } -> true | _ -> false)
    (Pg_array.of_string Row.Elem.string "{\"unfinished}");
  expect "dimensions" []
    (function
      | Codec.Invalid_dimensions (Element_count_mismatch { expected = 2; actual = 1 }) ->
          true
      | _ -> false)
    (Pg_array.make ~dimensions:[ { lower = 1; length = 2 } ] [ Some 1 ]);
  expect "NULL list element" [ Codec.Array_index [ 2 ] ] (( = ) Codec.Unexpected_null)
    (Pg_array.to_list (ok (Pg_array.of_string Row.Elem.int "{1,NULL}")));
  expect "multirange upper bound"
    [ Codec.Multirange_index 1; Upper_bound ]
    (function
      | Codec.Invalid_value { expected = "int"; value = "bad" } -> true | _ -> false)
    (Range.multirange_of_string Row.Elem.int "{[1,2),[3,bad)}");
  let bad_range =
    Range.Bounds { lower = Range.Unbounded; upper = Range.Inclusive "bad\000text" }
  in
  expect "multirange printer"
    [ Codec.Multirange_index 1; Upper_bound ]
    (( = ) Codec.Nul_byte)
    (Range.multirange_to_string (fun s -> Ok s) [ Range.Empty; bad_range ]);
  expect "named composite field"
    [ Codec.Field { index = 1; name = Some "age" } ]
    (function Codec.Invalid_value { value = "bad"; _ } -> true | _ -> false)
    (Composite.field ~name:"age" Row.Elem.int [| None; Some "bad" |] 1);
  (* Build nested literals through the public text codecs, then exercise the
     actual generated decoder and the runtime error boundary. *)
  let literal =
    Composite.to_string
      [| Some "label"; Some "[-2:-1]={open,bogus}"; Some "1"; Some "[1,2)" |]
  in
  let array =
    ok (Pg_array.make ~dimensions:[ { lower = 4; length = 1 } ] [ Some literal ])
  in
  let row = Array.make Db.Get_compiler_value.columns Value.Null in
  row.(2) <- Value.Text (ok (Pg_array.to_string Codec.text array));
  let path =
    [
      Codec.Column { index = 2; name = Some "payloads" };
      Array_index [ 4 ];
      Field { index = 1; name = Some "states" };
      Array_index [ -1 ];
    ]
  in
  let enum_reason = function
    | Codec.Unknown_enum
        {
          type_name = "compiler_a_status";
          value = "bogus";
          allowed = [ "open"; "closed" ];
        } ->
        true
    | _ -> false
  in
  expect "generated nested enum" path enum_reason (Db.Get_compiler_value.decode row);
  let raw = { Recording.calls = 0; rows = [ row ] } in
  let conn = Driver.make (module Recording) raw in
  let wrapped = Db.get_compiler_value conn ~id:1 in
  check "runtime keeps query and complete cause"
    (match wrapped with
    | Error (Error.Decode { query = "GetCompilerValue"; error }) ->
        error.path = path && enum_reason error.reason
    | _ -> false);
  let diagnostic = match wrapped with Error e -> e | _ -> assert false in
  check "optional inspection"
    (Option.map (fun e -> e.Codec.path) (Error.codec_error diagnostic) = Some path);
  check "generic formatting"
    (String.length (Format.asprintf "%a" Error.pp diagnostic) > 20);
  let lexical = Array.make Db.Get_compiler_value.columns Value.Null in
  lexical.(0) <- Value.Text "(a,\"unterminated)";
  expect "generated lexical field name"
    [
      Codec.Column { index = 0; name = Some "payload" };
      Field { index = 1; name = Some "states" };
    ]
    (function Codec.Invalid_syntax _ -> true | _ -> false)
    (Db.Get_compiler_value.decode lexical);
  row.(2) <- Value.Text "{\"(one,two)\"}";
  expect "generated field count"
    [ Codec.Column { index = 2; name = Some "payloads" }; Array_index [ 1 ] ]
    (function
      | Codec.Field_count { type_name = "compiler_a_payload"; expected = 4; actual = 2 }
        ->
          true
      | _ -> false)
    (Db.Get_compiler_value.decode row);
  let payload : Db.compiler_a_payload =
    { label = Some "bad\000text"; states = None; amount = None; span = None }
  in
  let payloads =
    ok
      (Pg_array.make
         ~dimensions:[ { lower = -2; length = 1 }; { lower = 5; length = 1 } ]
         [ Some payload ])
  in
  let params : Db.Copy_compiler_value.params =
    {
      id = 1;
      payload = None;
      other = None;
      payloads;
      matrix = Pg_array.of_list [];
      positives = Pg_array.of_list [];
      spans = [];
      words = Range.Empty;
    }
  in
  raw.calls <- 0;
  let encoded =
    Db.put_compiler_value conn ~id:1 ~payloads ~matrix:params.matrix
      ~positives:params.positives ~spans:[] ~words:Range.Empty ()
  in
  let encode_path =
    [
      Codec.Parameter "payloads";
      Array_index [ -2; 5 ];
      Field { index = 0; name = Some "label" };
    ]
  in
  check "generated encoding keeps parameter and nested path"
    (match encoded with
    | Error (Error.Encode { error = { path; reason = Nul_byte }; _ }) ->
        path = encode_path
    | _ -> false);
  let good = { params with payloads = Pg_array.of_list [] } in
  check "COPY error identifies row before any execution"
    (match Db.copy_compiler_value conn [ good; params ] with
    | Error (Error.Encode { error = { path; reason = Nul_byte }; _ }) ->
        path = Codec.Copy_row 1 :: encode_path
    | _ -> false);
  check "failed encoders never reach driver" (raw.calls = 0);
  expect "dynamic parameter group" []
    (function Codec.Inconsistent_parameters [ "lo"; "hi" ] -> true | _ -> false)
    (Db.Optional_bounds.sql { lo = Some 1; hi = None });
  let custom = Codec.custom "application-specific constraint" in
  expect "custom codec reason survives" [ Codec.Array_index [ 1 ] ] (( = ) custom.reason)
    (Pg_array.of_string (fun _ -> Error custom) "{x}");
  let parsed = Pg_array.of_string Row.Elem.int "{bad}" in
  check "option adapter discards structured error"
    (Pg_array.of_string_opt Row.Elem.int "{bad}" = None);
  check "exception adapter preserves structured error"
    (match Pg_array.of_string_exn Row.Elem.int "{bad}" with
    | exception Codec.Codec_error e -> Error e = parsed
    | _ -> false);
  print_endline "structured codec errors: all good"
