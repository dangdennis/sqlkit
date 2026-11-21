(* Interval parsing across the four IntervalStyle output formats PostgreSQL
   can be configured to print, plus the encode format we send back. *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

open Sqlkit

let iv ?years ?months ?days ?hours ?minutes ?seconds () =
  Interval.make_exn ?years ?months ?days ?hours ?minutes ?seconds ()

let eq s expected =
  match Interval.of_string_opt s with
  | Some v -> Interval.equal v expected
  | None -> false

let () =
  (* postgres style (the default) *)
  check "postgres: full"
    (eq "1 year 2 mons 3 days 04:05:06.789"
       (Interval.make_exn ~years:1 ~months:2 ~days:3 ~hours:4 ~minutes:5 ~seconds:6
          ~micros:789_000L ()));
  check "postgres: clock only" (eq "04:05:06" (iv ~hours:4 ~minutes:5 ~seconds:6 ()));
  check "postgres: zero" (eq "00:00:00" Interval.zero);
  check "postgres: negative clock"
    (eq "-04:05:06" (iv ~hours:(-4) ~minutes:(-5) ~seconds:(-6) ()));
  check "postgres: interior signs"
    (eq "-1 mons +2 days -03:00:00" (iv ~months:(-1) ~days:2 ~hours:(-3) ()));

  (* postgres_verbose *)
  check "verbose: ago negates" (eq "@ 1 year 2 mons ago" (iv ~years:(-1) ~months:(-2) ()));
  check "verbose: plain" (eq "@ 3 days 4 hours" (iv ~days:3 ~hours:4 ()));

  (* sql_standard *)
  check "sql_standard: year-month" (eq "+1-2" (iv ~years:1 ~months:2 ()));
  check "sql_standard: full"
    (eq "+1-2 +3 +04:05:06"
       (iv ~years:1 ~months:2 ~days:3 ~hours:4 ~minutes:5 ~seconds:6 ()));
  check "sql_standard: negative year-month" (eq "-1-2" (iv ~years:(-1) ~months:(-2) ()));

  (* iso_8601 *)
  check "iso: full"
    (eq "P1Y2M3DT4H5M6.789S"
       (Interval.make_exn ~years:1 ~months:2 ~days:3 ~hours:4 ~minutes:5 ~seconds:6
          ~micros:789_000L ()));
  check "iso: time only" (eq "PT0S" Interval.zero);
  check "iso: negative component" (eq "P-1M2D" (iv ~months:(-1) ~days:2 ()));
  check "iso: weeks" (eq "P2W" (iv ~days:14 ()));

  (* what we send to postgres *)
  check "encode is unit-spelled"
    (Interval.to_string (iv ~months:14 ~days:3 ~hours:4 ())
    = "14 mons 3 days 14400000000 microseconds");
  check "garbage is rejected" (Interval.of_string_opt "not an interval" = None);
  check "empty is rejected" (Interval.of_string_opt "" = None);

  let range_error = function
    | Error { Codec.reason = Codec.Out_of_range _; _ } -> true
    | _ -> false
  in
  List.iter
    (fun (name, result) -> check name (range_error result))
    [
      ("months above int32", Interval.make ~months:2147483648 ());
      ("months below int32", Interval.make ~months:(-2147483649) ());
      ("days above int32", Interval.make ~days:2147483648 ());
      ("days below int32", Interval.make ~days:(-2147483649) ());
      ("year multiplication overflow", Interval.make ~years:max_int ());
      ("hour multiplication overflow", Interval.make ~hours:max_int ());
      ( "positive time addition overflow",
        Interval.make ~seconds:1 ~micros:Int64.max_int () );
      ( "negative time addition overflow",
        Interval.make ~seconds:(-1) ~micros:Int64.min_int () );
      ( "reserved positive infinity",
        Interval.make ~months:2147483647 ~days:2147483647 ~micros:Int64.max_int () );
      ( "reserved negative infinity",
        Interval.make ~months:(-2147483648) ~days:(-2147483648) ~micros:Int64.min_int ()
      );
    ];
  let maximum = Interval.make_exn ~micros:Int64.max_int () in
  let minimum = Interval.make_exn ~micros:Int64.min_int () in
  List.iter
    (fun text -> check ("exact maximum: " ^ text) (eq text maximum))
    [
      "9223372036854775807 microseconds";
      "2562047788:00:54.775807";
      "PT9223372036854.775807S";
      "@ 2562047788 hours 54.775807 secs";
    ];
  List.iter
    (fun text -> check ("exact minimum: " ^ text) (eq text minimum))
    [
      "-9223372036854775808 microseconds";
      "-2562047788:00:54.775808";
      "PT-9223372036854.775808S";
      "@ 2562047788 hours 54.775808 secs ago";
    ];
  List.iter
    (fun text ->
      check ("overflow rejected: " ^ text) (range_error (Interval.of_string text)))
    [
      "2147483648 mons";
      "-2147483649 days";
      "P2147483648M";
      "9223372036854775808 microseconds";
      "-9223372036854775809 microseconds";
      "PT9223372036854.775808S";
      "PT-9223372036854.775809S";
      "@ -9223372036854775808 microseconds ago";
      "9223372036854775807 microseconds 1 microsecond";
      "-9223372036854775808 microseconds -1 microsecond";
      "999999999999999999999999 hours";
      "infinity";
      "-infinity";
    ];
  check "SQL standard global sign"
    (eq "-3 4:05:06.7"
       (Interval.make_exn ~days:(-3) ~hours:(-4) ~minutes:(-5) ~seconds:(-6)
          ~micros:(-700000L) ()));
  check "verbose fractional seconds"
    (eq "@ 6.789 secs" (Interval.make_exn ~seconds:6 ~micros:789000L ()));
  List.iter
    (fun s ->
      check
        ("malformed or inexact rejected: " ^ s)
        (Result.is_error (Interval.of_string s)))
    [ "PT"; "P1DT"; "PT1Y"; "PT0.0000001S"; "01:99:00"; "01:00:60"; "01:-2:03" ];
  check "option constructor drops error" (Interval.make_opt ~months:max_int () = None);
  check "exception constructor retains error"
    (match Interval.make_exn ~months:max_int () with
    | exception Codec.Codec_error e -> Error e = Interval.make ~months:max_int ()
    | _ -> false);
  let rng = Random.State.make [| 73; 19 |] in
  for _ = 1 to 2000 do
    let months = Random.State.bits rng - 500000000 in
    let days = Random.State.bits rng - 500000000 in
    let micros =
      Int64.logxor
        (Int64.shift_left (Int64.of_int (Random.State.bits rng)) 34)
        (Int64.of_int (Random.State.bits rng))
    in
    let t = Interval.make_exn ~months ~days ~micros () in
    if Interval.of_string (Interval.to_string t) <> Ok t then
      failwith "finite components failed to round-trip"
  done;
  print_endline "all good"
