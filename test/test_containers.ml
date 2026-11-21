let check label condition = if not condition then failwith label
let ok = function Ok x -> x | Error _ -> failwith "unexpected error"
let text s = Ok s

let rejects label f =
  match f () with Error _ -> () | Ok _ -> failwith (label ^ ": accepted malformed input")

let () =
  let module A = Sqlkit.Pg_array in
  let module R = Sqlkit.Range in
  let tricky = [ ""; "NULL"; "a,b"; "a\\b"; "a\"b"; "(x)"; "{x}"; "\n\t" ] in
  List.iteri
    (fun i s ->
      let a =
        A.make_exn
          ~dimensions:[ { lower = i - 4; length = 2 }; { lower = 2; length = 2 } ]
          [ Some s; None; Some "NULL"; Some "" ]
      in
      check "array round trip" (A.of_string text (ok (A.to_string text a)) = Ok a))
    tricky;
  check "empty array" (A.dimensions (ok (A.of_string text "{}")) = []);
  check "escaped NULL is text"
    (A.elements (ok (A.of_string text "{N\\ULL}")) = [ Some "NULL" ]);
  check "custom delimiter"
    (A.to_list (ok (A.of_string ~delimiter:';' text "{\"x;y\";z}")) = Ok [ "x;y"; "z" ]);
  List.iter
    (fun input -> rejects input (fun () -> A.of_string text input))
    [ "{"; "{\"x}"; "{a,}"; "{{a},{b,c}}"; "[0:3]={a}"; "{a}junk"; "{\"a\"b}" ];
  rejects "dimension mismatch" (fun () ->
      A.make ~dimensions:[ { lower = 1; length = 2 } ] [ Some 1 ]);
  rejects "NULL list conversion" (fun () -> A.to_list (ok (A.of_string text "{NULL}")));
  rejects "bounds list conversion" (fun () ->
      A.to_list (ok (A.of_string text "[0:0]={a}")));
  List.iter
    (fun s ->
      let fields = [| Some s; None; Some "" |] in
      check "composite escaping"
        (Sqlkit.Composite.of_string (Sqlkit.Composite.to_string fields) = Ok fields))
    tricky;
  List.iter
    (fun s -> rejects s (fun () -> Sqlkit.Composite.of_string s))
    [ "(\"x)"; "(a)junk"; "(a))"; "(\"x\"y)" ];
  let bounds = [ R.Unbounded; R.Inclusive ""; R.Exclusive "a,\"\\)" ] in
  List.iter
    (fun lower ->
      List.iter
        (fun upper ->
          let r = R.Bounds { lower; upper } in
          check "range round trip" (R.of_string text (ok (R.to_string text r)) = Ok r))
        bounds)
    bounds;
  check "empty range" (R.of_string text "empty" = Ok R.Empty);
  let rs = [ R.Empty; R.Bounds { lower = R.Inclusive "x"; upper = R.Unbounded } ] in
  check "multirange round trip"
    (R.multirange_of_string text (ok (R.multirange_to_string text rs)) = Ok rs);
  List.iter
    (fun s -> rejects s (fun () -> R.of_string text s))
    [ "[a,b"; "[a,b]x"; "[\"a,b)"; "emptyx" ];
  List.iter
    (fun n ->
      check "int64 codec" (Sqlkit.Row.int64 [| Sqlkit.Value.of_int64 n |] 0 = Ok n))
    [ Int64.min_int; Int64.max_int ];
  print_endline "container codecs: all good"
