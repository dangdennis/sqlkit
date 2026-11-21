(* sqlkit runtime -- public interface.

   The whole point of this file is the three execution functions below. Each
   takes a generated query module as a *modular explicit* argument, so the
   module you pass determines both the parameter type you must supply and the
   result type you get back:

     val fetch_one : (module Q : Query.ONE) -> Driver.t -> Q.params -> (Q.row option, _) result

   With ordinary first-class modules this type is not writable in curried form:
   [Q.params] and [Q.row] would each need to escape as a separate type variable
   threaded through a [with type params = 'p and type row = 'r] constraint, and
   every caller would have to reconstruct that witness. Here they simply project
   out of the module argument.

   Call sites look like:

     let user = Sqlkit.fetch_one (module Db.Get_user) conn { id = 42 }
     let users = Sqlkit.fetch_all (module Db.Search_users) conn { org_id; limit = 100 }
     let n = Sqlkit.exec (module Db.Delete_user) conn { id = 42 }

   The module argument is written by the code generator in typical use, so the
   explicitness costs the user nothing -- which is exactly why *explicits* are a
   better fit here than implicits. *)

module Limits = Limits
module Telemetry = Telemetry
module Codec = Codec
module Pg_array = Pg_array
module Range = Range
module Composite = Composite
module Value = Value
module Error = Error
module Row = Row
module Driver = Driver
module Query = Query
module Sqlstate = Sqlstate
module Interval = Interval

(* A connection handle. Deliberately NOT parameterised by a phantom region
   tag: the tag cannot generalise through the modular-explicit query
   functions. Full postmortem in DESIGN.md, "Transactions, and the phantom tag
   that died twice". *)
type conn = Driver.t

open Codec

exception Sql_error of Error.t

let get_exn = function Ok v -> v | Error e -> raise (Sql_error e)

let () =
  Printexc.register_printer (function
    | Sql_error e -> Some ("Sqlkit.Sql_error: " ^ Error.to_string e)
    | _ -> None)

let callback conn ~operation f =
  let (Driver.Conn ((module D), _, _)) = conn in
  let control = function
    | Out_of_memory | Stack_overflow | Sys.Break -> true
    | e -> D.is_cancellation e
  in
  match f () with
  | r -> r
  | exception Sql_error e -> Error e
  | exception e when not (control e) ->
      Error (Error.Callback { operation; message = Printexc.to_string e })

let with_cleanup conn ~finally f =
  let (Driver.Conn ((module D), _, _)) = conn in
  D.with_cleanup ~finally f

let ensure (Driver.Conn (_, _, state)) =
  if state.usable then Ok () else Error Error.Connection_unusable

let outcome = function
  | Ok _ -> Telemetry.Success
  | Error e ->
      let kind =
        match e with
        | Error.Connect _ -> Telemetry.Connect
        | Execute _ -> Execute
        | Encode _ -> Encode
        | Decode _ -> Decode
        | Cardinality _ -> Cardinality
        | Invalid_argument _ -> Invalid_argument
        | Callback _ -> Callback
        | Timeout _ -> Timeout
        | Connection_unusable -> Connection_unusable
        | Cleanup _ -> Cleanup_failure
      in
      Telemetry.Failure { kind; sqlstate = Error.sqlstate e }

let notify conn event =
  let (Driver.Conn ((module D), _, state)) = conn in
  match state.observe event with
  | () -> ()
  | exception e
    when not
           (D.is_cancellation e
           ||
           match e with
           | Out_of_memory | Stack_overflow | Sys.Break -> true
           | _ -> false) ->
      ()

let observe conn ~kind ~name f =
  let (Driver.Conn ((module D), c, _)) = conn in
  let started = D.now c and finished = ref Telemetry.Cancelled in
  with_cleanup conn
    ~finally:(fun () ->
      notify conn
        {
          Telemetry.kind;
          name;
          duration_s = max 0. (D.now c -. started);
          outcome = !finished;
        })
    (fun () ->
      let r = callback conn ~operation:name f in
      finished := outcome r;
      r)

let encode_result name r =
  Result.map_error (fun error -> Error.Encode { query = name; error }) r

let encode_params ?row name r =
  let validate () =
    let* params = r in
    let* _ =
      traversei (fun i v -> context (Parameter_index i) (Value.validate v)) params
    in
    Ok params
  in
  encode_result name
    (match row with None -> validate () | Some i -> context (Copy_row i) (validate ()))

let decode_result name r =
  Result.map_error (fun (error : Row.error) -> Error.Decode { query = name; error }) r

let execute_error ~query ~sql (d : Driver.error) =
  match d.timeout_seconds with
  | Some seconds -> Error.Timeout { operation = query; phase = Query; seconds }
  | None ->
      Error.Execute
        {
          query;
          sql;
          message = d.Driver.message;
          sqlstate = d.Driver.sqlstate;
          detail = d.Driver.detail;
          hint = d.Driver.hint;
          constraint_name = d.Driver.constraint_name;
          table_name = d.Driver.table_name;
          column_name = d.Driver.column_name;
        }

let run_query (conn : conn) ~name ~sql ~params ~columns =
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      match D.query c ~sql ~params ~columns with
      | Ok rows -> Ok rows
      | Error (d : Driver.error) -> Error (execute_error ~query:name ~sql d))

let fetch_all (module Q : Query.MANY) (conn : conn) (p : Q.params) :
    (Q.row list, Error.t) result =
  observe conn ~kind:Telemetry.Query ~name:Q.name @@ fun () ->
  let* () = ensure conn in
  let* sql = encode_result Q.name (Q.sql p) in
  let* params = encode_params Q.name (Q.encode p) in
  let* rows = run_query conn ~name:Q.name ~sql ~params ~columns:Q.columns in
  traverse (fun r -> decode_result Q.name (Q.decode r)) rows

let fetch_one (module Q : Query.ONE) (conn : conn) (p : Q.params) :
    (Q.row option, Error.t) result =
  observe conn ~kind:Telemetry.Query ~name:Q.name @@ fun () ->
  let* () = ensure conn in
  let* sql = encode_result Q.name (Q.sql p) in
  let* params = encode_params Q.name (Q.encode p) in
  let* rows = run_query conn ~name:Q.name ~sql ~params ~columns:Q.columns in
  match rows with
  | [] -> Ok None
  | [ r ] -> Result.map Option.some (decode_result Q.name (Q.decode r))
  | rows ->
      Error
        (Error.Cardinality
           { query = Q.name; expected = "at most 1"; got = List.length rows })

let fetch_one_strict (module Q : Query.ONE_STRICT) (conn : conn) (p : Q.params) :
    (Q.row, Error.t) result =
  observe conn ~kind:Telemetry.Query ~name:Q.name @@ fun () ->
  let* () = ensure conn in
  let* sql = encode_result Q.name (Q.sql p) in
  let* params = encode_params Q.name (Q.encode p) in
  let* rows = run_query conn ~name:Q.name ~sql ~params ~columns:Q.columns in
  match rows with
  | [ r ] -> decode_result Q.name (Q.decode r)
  | rows ->
      Error
        (Error.Cardinality
           { query = Q.name; expected = "exactly 1"; got = List.length rows })

let exec (module Q : Query.EXEC) (conn : conn) (p : Q.params) :
    (int option, Error.t) result =
  observe conn ~kind:Telemetry.Query ~name:Q.name @@ fun () ->
  let* () = ensure conn in
  let* sql = encode_result Q.name (Q.sql p) in
  let* params = encode_params Q.name (Q.encode p) in
  let (Driver.Conn ((module D), c, _)) = conn in
  Result.map_error (execute_error ~query:Q.name ~sql) (D.exec c ~sql ~params)

let copy (module Q : Query.COPY) (conn : conn) (rows : Q.params list) :
    (int, Error.t) result =
  observe conn ~kind:Telemetry.Query ~name:Q.name @@ fun () ->
  let* () = ensure conn in
  let* lines =
    traversei
      (fun row r -> Result.map Value.Copy.line (encode_params ~row Q.name (Q.encode r)))
      rows
  in
  let (Driver.Conn ((module D), c, _)) = conn in
  Result.map_error
    (execute_error ~query:Q.name ~sql:Q.copy_sql)
    (D.copy c ~sql:Q.copy_sql ~rows:lines)

let close (conn : conn) =
  let (Driver.Conn ((module D), c, state)) = conn in
  if not state.usable then Ok ()
  else begin
    state.usable <- false;
    callback conn ~operation:"close" (fun () ->
        D.close c;
        Ok ())
  end

let close_exn conn = get_exn (close conn)

(* ---------- transactions ---------- *)

let statement (conn : conn) sql =
  callback conn ~operation:"transaction" @@ fun () ->
  let* () = ensure conn in
  match conn with
  | Driver.Conn ((module D), c, _) -> (
      match D.exec c ~sql ~params:[] with
      | Ok _ -> Ok ()
      | Error (d : Driver.error) -> Error (execute_error ~query:"transaction" ~sql d))

let transaction ?isolation ?(retry = 0) (conn : conn) f =
  observe conn ~kind:Telemetry.Transaction ~name:"transaction" @@ fun () ->
  let* () = ensure conn in
  let (Driver.Conn (_, _, state)) = conn in
  let depth = state.depth in
  if retry < 0 then
    Error
      (Error.Invalid_argument
         { operation = "transaction"; message = "retry must be nonnegative" })
  else if !depth > 0 && isolation <> None then
    Error
      (Error.Invalid_argument
         {
           operation = "transaction";
           message = "isolation cannot be changed inside an enclosing transaction";
         })
  else
    let nested = !depth > 0 in
    let sp = Printf.sprintf "sqlkit_savepoint_%d" !depth in
    let begin_sql =
      if nested then "SAVEPOINT " ^ sp
      else
        match isolation with
        | None -> "BEGIN"
        | Some `Read_committed -> "BEGIN ISOLATION LEVEL READ COMMITTED"
        | Some `Repeatable_read -> "BEGIN ISOLATION LEVEL REPEATABLE READ"
        | Some `Serializable -> "BEGIN ISOLATION LEVEL SERIALIZABLE"
    in
    let rollback () =
      let* () =
        statement conn (if nested then "ROLLBACK TO SAVEPOINT " ^ sp else "ROLLBACK")
      in
      if nested then statement conn ("RELEASE SAVEPOINT " ^ sp) else Ok ()
    in
    let rec attempt remaining =
      let* () = statement conn begin_sql in
      incr depth;
      let completed = ref false and cleanup_error = ref None in
      let out =
        with_cleanup conn
          ~finally:(fun () ->
            decr depth;
            if not !completed then begin
              let r = observe conn ~kind:Telemetry.Cleanup ~name:"rollback" rollback in
              match r with
              | Ok () -> ()
              | Error e ->
                  let discard = close conn in
                  cleanup_error :=
                    Some
                      (match discard with
                      | Ok () -> e
                      | Error d -> Error.Cleanup { primary = Some e; cleanup = d })
            end)
          (fun () ->
            let* v = callback conn ~operation:"transaction" (fun () -> f conn) in
            let* () =
              statement conn (if nested then "RELEASE SAVEPOINT " ^ sp else "COMMIT")
            in
            completed := true;
            Ok v)
      in
      match (!cleanup_error, out) with
      | Some cleanup, _ ->
          Error
            (Error.Cleanup
               { primary = (match out with Error e -> Some e | Ok _ -> None); cleanup })
      | None, Error e
        when (not nested) && remaining > 0 && state.usable && Error.is_retryable e ->
          attempt (remaining - 1)
      | None, out -> out
    in
    attempt retry

let transaction_exn ?isolation ?retry conn f =
  get_exn (transaction ?isolation ?retry conn (fun tx -> Ok (f tx)))

(* ---------- streaming ---------- *)

(* Batched streaming over a server-side cursor, composed entirely from the
   operations Driver.S already has -- DECLARE with the query's own parameters,
   FETCH FORWARD in batches, CLOSE -- so both drivers stream without changes.

   A WITHOUT HOLD cursor needs a transaction; [transaction] provides one, and
   its savepoint nesting means streaming inside a caller's transaction works.
   The fold therefore runs inside a transaction, and its effects roll back if
   the fold raises. *)

let fetch_fold (module Q : Query.MANY) ?(batch = 500) (conn : conn) (p : Q.params)
    ~(init : 'acc) ~(f : 'acc -> Q.row -> 'acc) : ('acc, Error.t) result =
  observe conn ~kind:Telemetry.Query ~name:Q.name @@ fun () ->
  let* () = ensure conn in
  if batch <= 0 then
    Error
      (Error.Invalid_argument
         { operation = "fetch_fold"; message = "batch must be positive" })
  else
    let* sql = encode_result Q.name (Q.sql p) in
    let* params = encode_params Q.name (Q.encode p) in
    transaction conn @@ fun tx ->
    (* The cursor name is derived from transaction depth, not a global counter:
     statements on one connection are sequential per depth, so the name set is
     finite and the libpq statement cache cannot grow one entry per call. *)
    let (Driver.Conn (_, _, state)) = tx in
    let depth = state.depth in
    let cur = Printf.sprintf "sqlkit_cursor_d%d" !depth in
    let declare () =
      match tx with
      | Driver.Conn ((module D), c, _) -> (
          match
            D.exec c
              ~sql:(Printf.sprintf "DECLARE %s NO SCROLL CURSOR FOR %s" cur sql)
              ~params
          with
          | Ok _ -> Ok ()
          | Error (d : Driver.error) -> Error (execute_error ~query:Q.name ~sql d))
    in
    let fetch_sql = Printf.sprintf "FETCH FORWARD %d FROM %s" batch cur in
    let rec loop acc =
      match run_query tx ~name:Q.name ~sql:fetch_sql ~params:[] ~columns:Q.columns with
      | Error e -> Error e
      | Ok [] -> Ok acc
      | Ok rows ->
          let rec fold acc = function
            | [] -> Ok acc
            | r :: rs ->
                let* row = decode_result Q.name (Q.decode r) in
                fold (f acc row) rs
          in
          let* acc = fold acc rows in
          if List.length rows < batch then Ok acc else loop acc
    in
    match declare () with
    | Error e -> Error e
    | Ok () -> (
        let out = loop init in
        match statement tx ("CLOSE " ^ cur) with
        | Ok () -> out
        | Error cleanup ->
            Error
              (Error.Cleanup
                 {
                   primary = (match out with Error e -> Some e | Ok _ -> None);
                   cleanup;
                 }))

let fetch_iter (module Q : Query.MANY) ?batch (conn : conn) (p : Q.params)
    ~(f : Q.row -> unit) : (unit, Error.t) result =
  fetch_fold (module Q) ?batch conn p ~init:() ~f:(fun () r -> f r)
