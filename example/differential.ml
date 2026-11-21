(* Differential test: the same statements through the pure OCaml driver and
   through libpq, on two connections to the same server, comparing everything
   a caller can observe: rows, NULLs, affected counts, and every field of an
   error. libpq is the reference; a difference is a bug in one of the drivers
   or a deliberate deviation that should be written down.

     dune exec example/differential.exe -- [CASES] [SEED] *)

let cases, seed =
  match Sys.argv with
  | [| _; n |] -> (int_of_string n, 1)
  | [| _; n; s |] -> (int_of_string n, int_of_string s)
  | _ -> (2000, 1)

type observed =
  | Rows of Sqlkit.Value.t array list
  | Count of int option
  | Copied of int
  | Refused of {
      sqlstate : string option;
      message : string;
      detail : string option;
      hint : string option;
      constraint_name : string option;
      table_name : string option;
      column_name : string option;
    }

(* libpq's message is the whole report (primary line, then DETAIL, HINT and
   so on); ours is the primary line, with the rest in their own fields. The
   first line is what both must agree on. *)
let first_line s =
  match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s

let refused (e : Sqlkit.Driver.error) =
  Refused
    {
      sqlstate = Option.map Sqlkit.Sqlstate.to_string e.sqlstate;
      message = first_line e.message;
      detail = e.detail;
      hint = e.hint;
      constraint_name = e.constraint_name;
      table_name = e.table_name;
      column_name = e.column_name;
    }

type action =
  | Query of string * Sqlkit.Value.t list
  | Exec of string * Sqlkit.Value.t list
  | Copy of string * string list

let run (Sqlkit.Driver.Conn ((module D), raw, _)) = function
  | Query (sql, params) -> (
      match D.query raw ~sql ~params ~columns:0 with
      | Ok r -> Rows r
      | Error e -> refused e)
  | Exec (sql, params) -> (
      match D.exec raw ~sql ~params with Ok n -> Count n | Error e -> refused e)
  | Copy (sql, rows) -> (
      match D.copy raw ~sql ~rows with Ok n -> Copied n | Error e -> refused e)

let show_value = function
  | Sqlkit.Value.Null -> "NULL"
  | Sqlkit.Value.Text s ->
      if String.length s > 60 then
        Printf.sprintf "%S…(%d)" (String.sub s 0 60) (String.length s)
      else Printf.sprintf "%S" s
  | v -> Sqlkit.Value.type_name v

let show = function
  | Rows rows ->
      Printf.sprintf "%d row(s)%s" (List.length rows)
        (match rows with
        | row :: _ -> ": " ^ String.concat ", " (List.map show_value (Array.to_list row))
        | [] -> "")
  | Count None -> "no count"
  | Count (Some n) -> Printf.sprintf "count %d" n
  | Copied n -> Printf.sprintf "copied %d" n
  | Refused r ->
      let o = Option.value ~default:"-" in
      Printf.sprintf "%s %S detail=%s hint=%s constraint=%s table=%s column=%s"
        (o r.sqlstate) r.message (o r.detail) (o r.hint) (o r.constraint_name)
        (o r.table_name) (o r.column_name)

let text s = Sqlkit.Value.Text s

(* ---------- the corpus ---------- *)

let fixed =
  [
    Exec
      ( "CREATE TEMP TABLE d (id int CONSTRAINT d_pk PRIMARY KEY, v text NOT NULL, n \
         numeric CONSTRAINT d_pos CHECK (n >= 0))",
        [] );
    Exec ("CREATE TEMP TABLE c (id int REFERENCES d (id), note text)", []);
    Exec ("INSERT INTO d VALUES (1, 'a', 1), (2, 'b', 2), (3, 'c', 3)", []);
    Query ("SELECT id::text, v, n::text FROM d ORDER BY id", []);
    Query ("SELECT", []);
    Query ("SELECT FROM d", []);
    Query ("SELECT NULL::text, '', ' ', E'\\t\\n\\r', 'ünïcödé 日本語 🙂'", []);
    Query ("SELECT * FROM d WHERE false", []);
    Exec ("INSERT INTO d VALUES (1, 'dup', 1)", []);
    Exec ("INSERT INTO d VALUES (9, NULL, 1)", []);
    Exec ("INSERT INTO d VALUES (9, 'neg', -1)", []);
    Exec ("INSERT INTO c VALUES (77, 'orphan')", []);
    Exec ("UPDATE d SET v = v || '!' WHERE id > $1::int", [ text "1" ]);
    Exec ("UPDATE d SET v = v WHERE false", []);
    Exec ("DELETE FROM d WHERE id = $1::int", [ text "3" ]);
    Exec ("SELECT 1", []);
    Exec ("SET application_name = 'differential'", []);
    Query ("SELECT 1/0", []);
    Query ("SELEC 1", []);
    Query ("SELECT * FROM no_such_table", []);
    Query ("SELECT no_such_column FROM d", []);
    Query ("SELECT $1::int", [ text "not a number" ]);
    Query ("SELECT $1::int", [ text "99999999999999999999" ]);
    Query ("SELECT $1::uuid", [ text "nope" ]);
    Query ("SELECT $1::date", [ text "2026-02-30" ]);
    Query ("SELECT $1::json", [ text "{broken" ]);
    Query ("SELECT pg_sleep(0), 'after sleep'", []);
    Query ("SELECT $1::text, $2::text, $3::text", [ Sqlkit.Value.Null; text ""; text "x" ]);
    Query ("SELECT generate_series(1, 5)::text", []);
    Query ("SELECT repeat('x', 100000)", []);
    Query
      ( "SELECT "
        ^ String.concat ", "
            (List.init 500 (fun i -> Printf.sprintf "%d::text AS c%d" i i)),
        [] );
    Query ("DO $$ BEGIN RAISE NOTICE 'hello'; END $$", []);
    Exec
      ( "DO $$ BEGIN RAISE EXCEPTION 'custom failure' USING ERRCODE = 'P0001', DETAIL = \
         'the detail', HINT = 'the hint'; END $$",
        [] );
    Exec ("BEGIN", []);
    Exec ("INSERT INTO d VALUES (1, 'dup in tx', 1)", []);
    Query ("SELECT 1", []);
    Exec ("COMMIT", []);
    Exec ("BEGIN", []);
    Exec ("SAVEPOINT s", []);
    Exec ("INSERT INTO d VALUES (50, 'saved', 1)", []);
    Exec ("ROLLBACK TO SAVEPOINT s", []);
    Exec ("COMMIT", []);
    Query ("SELECT count(*)::text FROM d", []);
    Copy ("COPY d (id, v, n) FROM STDIN", [ "10\tten\t10"; "11\televen\t\\N" ]);
    Copy ("COPY d (id, v, n) FROM STDIN", [ "12\ttwelve\t12"; "10\tdup\t1" ]);
    Copy ("COPY d (id, v, n) FROM STDIN", [ "13\tshort" ]);
    Copy ("COPY d (id, v, n) FROM STDIN", [ "x\tbad int\t1" ]);
    Copy ("COPY d (id, v, n) FROM STDIN", []);
    Copy ("COPY no_such (id) FROM STDIN", [ "1" ]);
    Copy ("SELECT 1", []);
    Query ("SELECT id::text, v, n::text FROM d ORDER BY id", []);
    Exec ("DEALLOCATE ALL", []);
    Query ("SELECT id::text, v, n::text FROM d ORDER BY id", []);
    Exec ("ALTER TABLE d ADD COLUMN extra text", []);
    Query ("SELECT * FROM d ORDER BY id", []);
    Query ("SELECT $1::text", [ text (String.make 1_000_000 'q') ]);
  ]

let random_string state =
  let pieces =
    [|
      "a";
      "Z";
      "0";
      " ";
      "\t";
      "\n";
      "\r";
      "\\";
      "'";
      "\"";
      "$1";
      "%";
      ",";
      "{";
      "}";
      "\\N";
      "é";
      "日";
      "🙂";
      "\x01";
      "\x7f";
      ";";
      "--";
      "/*";
    |]
  in
  String.concat ""
    (List.init (Random.State.int state 40) (fun _ ->
         pieces.(Random.State.int state (Array.length pieces))))

let random_case state =
  let int () = string_of_int (Random.State.int state 2_000_000 - 1_000_000) in
  let pick l = List.nth l (Random.State.int state (List.length l)) in
  match Random.State.int state 14 with
  | 0 ->
      Query
        ( "SELECT $1::text, length($1::text)::text, md5($1::text)",
          [ text (random_string state) ] )
  | 1 ->
      Query
        ( "SELECT ($1::int + $2::int)::text, ($1::int * $2::int)::text",
          [ text (int ()); text (int ()) ] )
  | 2 ->
      Query
        ( "SELECT ($1::int / $2::int)::text",
          [ text (int ()); text (pick [ "0"; "1"; "7"; int () ]) ] )
  | 3 ->
      Query
        ( "SELECT $1::numeric * $2::numeric, round($1::numeric / NULLIF($2::numeric, 0), \
           10)",
          [
            text
              (Printf.sprintf "%d.%04d"
                 (Random.State.int state 100000)
                 (Random.State.int state 10000));
            text (pick [ "0"; "0.0001"; "3"; "-17.5"; "1e30"; "NaN" ]);
          ] )
  | 4 ->
      Query
        ( "SELECT $1::float8, $1::float4, ($1::float8 * 3)::text",
          [
            text
              (pick
                 [ "0.1"; "1e308"; "-0"; "NaN"; "Infinity"; "1e-320"; "3.14159265358979" ]);
          ] )
  | 5 ->
      Query
        ( "SELECT $1::timestamptz, $1::timestamp, $1::date, ($1::timestamptz + \
           $2::interval)",
          [
            text
              (pick
                 [
                   "2026-11-21 12:00:00+00";
                   "1999-12-31 23:59:59.999999-08";
                   "0001-01-01 00:00:00+00";
                   "infinity";
                   "2026-02-29";
                 ]);
            text (pick [ "1 day"; "-3 months 2 hours"; "100 years"; "0.000001 seconds" ]);
          ] )
  | 6 ->
      Query
        ( "SELECT $1::bool, NOT $1::bool, $1::bool::text",
          [ text (pick [ "t"; "false"; "yes"; "0"; "maybe" ]) ] )
  | 7 ->
      Query
        ( "SELECT $1::bytea, encode($1::bytea, 'base64'), length($1::bytea)::text",
          [ text (pick [ "\\x"; "\\x00ff10"; "\\xdeadbeef"; "plain"; "\\xZZ" ]) ] )
  | 8 ->
      Query
        ( "SELECT $1::jsonb, $1::jsonb -> 'a', jsonb_typeof($1::jsonb)",
          [
            text (pick [ "{}"; "{\"a\": [1, 2, {\"b\": null}]}"; "\"s\""; "12.50"; "[" ]);
          ] )
  | 9 ->
      Query
        ( "SELECT $1::int[], array_length($1::int[], 1)::text, $2::text[]",
          [
            text (pick [ "{}"; "{1,2,3}"; "{NULL,5}"; "{{1,2},{3,4}}"; "{1,x}" ]);
            text (pick [ "{}"; "{a,\"b c\",NULL,\"\"}"; "{\"q\\\"uote\"}" ]);
          ] )
  | 10 ->
      Query
        ( "SELECT $1::int4range, $1::int4range @> $2::int, upper($1::int4range)::text",
          [ text (pick [ "[1,10)"; "empty"; "(,5]"; "[5,1)" ]); text (int ()) ] )
  | 11 ->
      Query
        ( "SELECT g::text, repeat($1::text, g) FROM generate_series(1, $2::int) g",
          [ text (random_string state); text (string_of_int (Random.State.int state 50)) ]
        )
  | 12 ->
      Exec
        ( "INSERT INTO d (id, v, n) VALUES ($1::int, $2, $3::numeric) ON CONFLICT (id) \
           DO UPDATE SET v = excluded.v",
          [
            text (string_of_int (100 + Random.State.int state 50));
            text (random_string state);
            text (pick [ "0"; "5.5"; "-1"; "abc" ]);
          ] )
  | _ ->
      Copy
        ( "COPY c (id, note) FROM STDIN",
          List.init (Random.State.int state 5) (fun _ ->
              Printf.sprintf "%s\t%s"
                (pick [ "1"; "2"; "10"; "999"; "\\N"; "x" ])
                (Sqlkit.Value.Copy.cell (Sqlkit.Value.Text (random_string state)))) )

let describe = function
  | Query (sql, _) -> "query " ^ sql
  | Exec (sql, _) -> "exec " ^ sql
  | Copy (sql, rows) -> Printf.sprintf "copy %s (%d rows)" sql (List.length rows)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = env#mono_clock in
  let get = function Ok c -> c | Error e -> failwith (Sqlkit.Error.to_string e) in
  let conninfo = Sqlkit_eio.conninfo_of_env () in
  let ours = get (Sqlkit_pgwire.connect ~sw ~net:env#net ~clock conninfo) in
  let reference = get (Sqlkit_eio.connect ~sw ~clock conninfo) in
  let state = Random.State.make [| seed |] in
  let differences = ref 0 and compared = ref 0 in
  let compare_one action =
    incr compared;
    let a = run ours action and b = run reference action in
    if a <> b && !differences < 25 then begin
      incr differences;
      Printf.printf "DIFF %s\n  pgwire: %s\n  libpq:  %s\n" (describe action) (show a)
        (show b)
    end
    else if a <> b then incr differences
  in
  List.iter compare_one fixed;
  for _ = 1 to cases do
    compare_one (random_case state)
  done;
  Printf.printf "%d statements compared, seed %d\n" !compared seed;
  if !differences = 0 then print_endline "all good"
  else begin
    Printf.printf "%d difference(s)\n" !differences;
    exit 1
  end
