let ( let* ) = Result.bind
(* Hand-written stand-in for generator output, plus an in-memory driver, so the
   typing story is exercised with no database in the loop. Once bin/ emits real
   modules, this file becomes the shape those modules must match. *)

(* Generated files must carry this: a row record's fields are written by the
   decoder but often only some are read by the caller, which trips warning 69
   in the *defining* module -- i.e. in generated code, for a reason the user
   cannot fix. *)
[@@@warning "-69"]

(* ---------- a fake driver ---------- *)

module Fake = struct
  type conn = {
    mutable last : string * Sqlkit.Value.t list;
    rows : Sqlkit.Value.t array list;
  }

  let now _ = 0.
  let is_cancellation _ = false
  let with_cleanup = Fun.protect
  let close _ = ()

  let query c ~sql ~params ~columns:_ =
    c.last <- (sql, params);
    Ok c.rows

  let exec c ~sql ~params =
    c.last <- (sql, params);
    Ok (Some (List.length c.rows))

  let copy c ~sql ~rows =
    c.last <- (sql, []);
    Ok (List.length rows)
end

let conn rows = Sqlkit.Driver.make (module Fake) { Fake.last = ("", []); rows }

(* ---------- what the generator will emit ---------- *)

(* -- name: GetUser :one
   SELECT id, email, display_name FROM users WHERE id = $1; *)
module Get_user = struct
  type params = { id : int }
  type row = { id : int; email : string; display_name : string option }

  let name = "GetUser"
  let sql (_ : params) = Ok "SELECT id, email, display_name FROM users WHERE id = $1"

  (* annotation required: [row] also has an [id] field and is defined later, so
     an unannotated [{ id }] pattern would resolve to [row]. The generator must
     emit this annotation on every encode. *)
  let encode ({ id } : params) = Ok [ Sqlkit.Value.of_int id ]

  let decode r =
    let* id = Sqlkit.Row.int r 0 in
    let* email = Sqlkit.Row.string r 1 in
    let* display_name = Sqlkit.Row.(option string) r 2 in
    Ok { id; email; display_name }

  let columns = 3
  let cardinality = Sqlkit.Query.One
end

(* -- name: SearchUsers :many *)
module Search_users = struct
  type params = { pattern : string; limit : int }
  type row = { id : int; email : string }

  let name = "SearchUsers"
  let sql (_ : params) = Ok "SELECT id, email FROM users WHERE email ILIKE $1 LIMIT $2"
  let encode { pattern; limit } = Ok Sqlkit.Value.[ of_string pattern; of_int limit ]

  let decode r =
    let* id = Sqlkit.Row.int r 0 in
    let* email = Sqlkit.Row.string r 1 in
    Ok { id; email }

  let columns = 2
  let cardinality = Sqlkit.Query.Many
end

(* -- name: DeleteUser :exec *)
module Delete_user = struct
  type params = { id : int }

  let name = "DeleteUser"
  let sql (_ : params) = Ok "DELETE FROM users WHERE id = $1"
  let encode { id } = Ok [ Sqlkit.Value.of_int id ]
  let cardinality = Sqlkit.Query.Exec
end

(* ---------- the part that matters ---------- *)

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

let user_row id email display_name =
  [|
    Sqlkit.Value.Int id;
    Sqlkit.Value.Text email;
    (match display_name with None -> Sqlkit.Value.Null | Some s -> Sqlkit.Value.Text s);
  |]

let () =
  (* fetch_one: the module argument fixes params to Get_user.params and the
     result to Get_user.row option. Both are inferred here -- no annotations. *)
  let db = conn [ user_row 42 "a@example.com" (Some "Dennis") ] in
  (match Sqlkit.fetch_one (module Get_user) db { id = 42 } with
  | Ok (Some u) ->
      check "fetch_one decodes"
        (u.Get_user.id = 42 && u.Get_user.display_name = Some "Dennis")
  | Ok None -> check "fetch_one decodes" false
  | Error e ->
      print_endline (Sqlkit.Error.to_string e);
      exit 1);

  (* NULL flows into the option field *)
  let db = conn [ user_row 7 "b@example.com" None ] in
  (match Sqlkit.fetch_one (module Get_user) db { id = 7 } with
  | Ok (Some u) -> check "null -> None" (u.Get_user.display_name = None)
  | _ -> check "null -> None" false);

  (* :one with more than one row is an error, not a silent truncation *)
  let db = conn [ user_row 1 "x@y.z" None; user_row 2 "p@q.r" None ] in
  (match Sqlkit.fetch_one (module Get_user) db { id = 1 } with
  | Error (Sqlkit.Error.Cardinality _) -> check "one rejects 2 rows" true
  | _ -> check "one rejects 2 rows" false);

  (* fetch_all *)
  let db = conn [ [| Sqlkit.Value.Int 1; Sqlkit.Value.Text "a@b.c" |] ] in
  (match Sqlkit.fetch_all (module Search_users) db { pattern = "%@b.c"; limit = 10 } with
  | Ok [ u ] -> check "fetch_all decodes" (u.Search_users.email = "a@b.c")
  | _ -> check "fetch_all decodes" false);

  (* a type error in the decoder surfaces as Error.Decode, not an exception *)
  let db = conn [ [| Sqlkit.Value.Text "not-an-int"; Sqlkit.Value.Text "a@b.c" |] ] in
  (match Sqlkit.fetch_all (module Search_users) db { pattern = "%"; limit = 10 } with
  | Error
      (Sqlkit.Error.Decode
         { error = { path = Sqlkit.Codec.Column { index = 0; _ } :: _; _ }; _ }) ->
      check "decode error is caught" true
  | _ -> check "decode error is caught" false);

  (* exec *)
  let db = conn [ user_row 42 "a@b.c" None ] in
  (match Sqlkit.exec (module Delete_user) db { id = 42 } with
  | Ok n -> check "exec returns count" (n = Some 1)
  | Error _ -> check "exec returns count" false);

  (* ---------- SQL sequences the runtime emits ----------

     A recording driver captures every statement, so transaction nesting,
     retry, and cursor plumbing are asserted as the exact SQL conversation --
     no database required. *)
  let module Rec = struct
    type conn = {
      mutable log : string list;
      (* fail the next statement with this prefix, once *)
      mutable fail_on : (string * Sqlkit.Driver.error) option;
    }

    let now _ = 0.
    let is_cancellation _ = false
    let with_cleanup = Fun.protect
    let close _ = ()

    let starts_with p s =
      String.length s >= String.length p && String.sub s 0 (String.length p) = p

    let take c sql =
      c.log <- sql :: c.log;
      match c.fail_on with
      | Some (prefix, e) when starts_with prefix sql ->
          c.fail_on <- None;
          Some e
      | _ -> None

    let query c ~sql ~params:_ ~columns:_ =
      match take c sql with Some e -> Error e | None -> Ok []

    let exec c ~sql ~params:_ =
      match take c sql with Some e -> Error e | None -> Ok (Some 1)

    (* the statement, then each pre-escaped line, land in the same log *)
    let copy c ~sql ~rows =
      match take c sql with
      | Some e -> Error e
      | None ->
          List.iter (fun r -> c.log <- r :: c.log) rows;
          Ok (List.length rows)
  end in
  let fresh () = { Rec.log = []; fail_on = None } in
  let conn_of c = Sqlkit.Driver.make (module Rec) c in
  let log c = List.rev c.Rec.log in
  let starts_with p s =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in

  (* plain transaction: BEGIN, body, COMMIT *)
  let c = fresh () in
  let conn = conn_of c in
  ignore
    (Sqlkit.transaction conn (fun tx -> Sqlkit.exec (module Delete_user) tx { id = 1 }));
  check "transaction sequence"
    (log c = [ "BEGIN"; "DELETE FROM users WHERE id = $1"; "COMMIT" ]);

  (* error body: ROLLBACK, not COMMIT *)
  let c = fresh () in
  ignore (Sqlkit.transaction (conn_of c) (fun _ -> Error (Sqlkit.Error.Connect "no")));
  check "error rolls back" (log c = [ "BEGIN"; "ROLLBACK" ]);

  (* nesting: savepoints with depth-derived names *)
  let c = fresh () in
  let conn = conn_of c in
  ignore
    (Sqlkit.transaction conn (fun tx ->
         Sqlkit.transaction tx (fun _ -> Error (Sqlkit.Error.Connect "inner")) |> ignore;
         Ok ()));
  check "savepoint sequence"
    (log c
    = [
        "BEGIN";
        "SAVEPOINT sqlkit_savepoint_1";
        "ROLLBACK TO SAVEPOINT sqlkit_savepoint_1";
        "RELEASE SAVEPOINT sqlkit_savepoint_1";
        "COMMIT";
      ]);

  (* isolation prefix *)
  let c = fresh () in
  ignore (Sqlkit.transaction ~isolation:`Serializable (conn_of c) (fun _ -> Ok ()));
  check "isolation in BEGIN" (log c = [ "BEGIN ISOLATION LEVEL SERIALIZABLE"; "COMMIT" ]);

  (* retry: a 40001 body failure re-runs the whole transaction once *)
  let c = fresh () in
  let serialization =
    Sqlkit.Driver.error ~sqlstate:(Sqlkit.Sqlstate.of_string "40001") "boom"
  in
  c.Rec.fail_on <- Some ("DELETE", serialization);
  (match
     Sqlkit.transaction ~retry:2 (conn_of c) (fun tx ->
         Sqlkit.exec (module Delete_user) tx { id = 1 })
   with
  | Ok _ ->
      check "retry re-runs after 40001"
        (log c
        = [
            "BEGIN";
            "DELETE FROM users WHERE id = $1";
            "ROLLBACK";
            "BEGIN";
            "DELETE FROM users WHERE id = $1";
            "COMMIT";
          ])
  | Error _ -> check "retry re-runs after 40001" false);

  let c = fresh () in
  let conn = conn_of c in
  let calls = ref 0 in
  let original =
    Sqlkit.Error.Execute
      {
        query = "test";
        sql = "test";
        message = "retryable";
        sqlstate = Some (Sqlkit.Sqlstate.of_string "40001");
        detail = None;
        hint = None;
        constraint_name = None;
        table_name = None;
        column_name = None;
      }
  in
  c.fail_on <- Some ("ROLLBACK", Sqlkit.Driver.error "rollback failed");
  let result =
    Sqlkit.transaction ~retry:3 conn (fun _ ->
        incr calls;
        Error original)
  in
  check "failed rollback retains retryable primary"
    (match result with
    | Error (Sqlkit.Error.Cleanup { primary = Some e; _ }) -> e = original
    | _ -> false);
  check "failed rollback is not retryable"
    (!calls = 1
    && match result with Error e -> not (Sqlkit.Error.is_retryable e) | _ -> false);
  check "failed rollback disables handle"
    (Sqlkit.exec (module Delete_user) conn { id = 1 }
    = Error Sqlkit.Error.Connection_unusable);

  (* fetch_fold: DECLARE/FETCH/CLOSE inside a transaction, and the cursor name
     is deterministic -- two calls, identical statements, no cache growth *)
  let run_fold c =
    ignore
      (Sqlkit.fetch_fold
         (module Search_users)
         ~batch:7 (conn_of c) { pattern = "%"; limit = 1 } ~init:0
         ~f:(fun n _ -> n + 1))
  in
  let c = fresh () in
  run_fold c;
  let first = log c in
  check "cursor conversation shape"
    (match first with
    | [ "BEGIN"; declare; fetch; close_; "COMMIT" ] ->
        starts_with "DECLARE sqlkit_cursor_d1 NO SCROLL CURSOR FOR" declare
        && fetch = "FETCH FORWARD 7 FROM sqlkit_cursor_d1"
        && close_ = "CLOSE sqlkit_cursor_d1"
    | _ -> false);
  let c2 = fresh () in
  run_fold c2;
  check "cursor names are deterministic across calls" (log c2 = first);

  (* copy: the driver receives the codegen-built statement and one pre-escaped
     line per row, in order *)
  let module Bulk = struct
    type params = { a : int; note : string option }

    let name = "Bulk"
    let copy_sql = "COPY t (a, note) FROM STDIN"

    let encode { a; note } =
      Ok
        [
          Sqlkit.Value.of_int a;
          (match note with
          | None -> Sqlkit.Value.Null
          | Some s -> Sqlkit.Value.of_string s);
        ]

    let cardinality = Sqlkit.Query.Copy
  end in
  let c = fresh () in
  (match
     Sqlkit.copy
       (module Bulk)
       (conn_of c)
       [ { Bulk.a = 1; note = Some "tab\there" }; { Bulk.a = 2; note = None } ]
   with
  | Ok n -> check "copy returns the row count" (n = 2)
  | Error _ -> check "copy returns the row count" false);
  check "copy conversation: statement then escaped lines"
    (log c = [ "COPY t (a, note) FROM STDIN"; "1\ttab\\there"; "2\t\\N" ]);

  let c = fresh () in
  check "ordinary callback exception is a value"
    (match Sqlkit.transaction (conn_of c) (fun _ -> failwith "callback") with
    | Error (Sqlkit.Error.Callback _) -> true
    | _ -> false);
  check "callback exception rolls back" (log c = [ "BEGIN"; "ROLLBACK" ]);
  let c = fresh () in
  check "negative retry returns error"
    (match Sqlkit.transaction ~retry:(-1) (conn_of c) (fun _ -> Ok ()) with
    | Error (Sqlkit.Error.Invalid_argument _) -> true
    | _ -> false);
  check "argument validation before BEGIN" (log c = []);
  check "zero batch returns error"
    (match
       Sqlkit.fetch_fold
         (module Search_users)
         ~batch:0 (conn_of c) { pattern = "%"; limit = 1 } ~init:0
         ~f:(fun n _ -> n + 1)
     with
    | Error (Sqlkit.Error.Invalid_argument _) -> true
    | _ -> false);
  let module Bad_encode = struct
    include Delete_user

    let encode _ = Error (Sqlkit.Codec.custom "cannot encode user id")
  end in
  check "encoder failure is a value"
    (match Sqlkit.exec (module Bad_encode) (conn_of c) { id = 1 } with
    | Error (Sqlkit.Error.Encode _) -> true
    | _ -> false);
  check "encoder failure does not execute" (log c = []);
  let module Raising_encode = struct
    include Delete_user

    let encode _ = failwith "third-party encoder"
  end in
  check "unexpected user codec exception is bounded"
    (match Sqlkit.exec (module Raising_encode) (conn_of c) { id = 1 } with
    | Error (Sqlkit.Error.Callback _) -> true
    | _ -> false);
  check "explicit transaction wrapper raises"
    (match Sqlkit.transaction_exn (conn_of c) (fun _ -> failwith "callback") with
    | exception Sqlkit.Sql_error (Sqlkit.Error.Callback _) -> true
    | _ -> false);
  print_endline "all good"
