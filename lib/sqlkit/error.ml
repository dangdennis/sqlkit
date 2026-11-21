type timeout_phase = Pool_acquire | Query | Connect

type t =
  | Timeout of { operation : string; phase : timeout_phase; seconds : float }
  | Connection_unusable
  | Cleanup of { primary : t option; cleanup : t }
  | Invalid_argument of { operation : string; message : string }
  | Encode of { query : string; error : Codec.error }
  | Callback of { operation : string; message : string }
  | Connect of string
  | Execute of {
      query : string;
      sql : string;
      message : string;
      sqlstate : Sqlstate.t option;
      detail : string option;
      hint : string option;
      constraint_name : string option;
      table_name : string option;
      column_name : string option;
    }
  | Decode of { query : string; error : Codec.error }
  | Cardinality of { query : string; expected : string; got : int }

let rec to_string = function
  | Timeout { operation; seconds; _ } ->
      Printf.sprintf "%s timed out after %.3gs" operation seconds
  | Connection_unusable -> "connection is closed, discarded, or outside its borrow"
  | Cleanup { primary; cleanup } ->
      (match primary with None -> "cleanup failed" | Some e -> to_string e)
      ^ "\n  cleanup failed: " ^ to_string cleanup
  | Invalid_argument { operation; message } -> operation ^ ": " ^ message
  | Encode { query; error } -> "query " ^ query ^ ": " ^ Codec.to_string error
  | Callback { operation; message } -> operation ^ ": callback failed: " ^ message
  | Connect m -> Printf.sprintf "connection failed: %s" m
  | Execute { query; sql; message; sqlstate; detail; constraint_name; _ } ->
      let code =
        match sqlstate with
        | None -> ""
        | Some s -> Printf.sprintf " [%s %s]" (Sqlstate.to_string s) (Sqlstate.name s)
      in
      let extra =
        String.concat ""
          [
            (match constraint_name with None -> "" | Some c -> "\n  constraint: " ^ c);
            (match detail with None -> "" | Some d -> "\n  detail: " ^ d);
          ]
      in
      Printf.sprintf "query %s failed%s: %s%s\n  sql: %s" query code message extra sql
  | Decode { query; error } -> "query " ^ query ^ ": " ^ Codec.to_string error
  | Cardinality { query; expected; got } ->
      Printf.sprintf "query %s: expected %s row(s), got %d" query expected got

let pp fmt e = Format.pp_print_string fmt (to_string e)

(* Accessors, so callers do not have to destructure a constructor that may grow
   more fields. *)
let rec sqlstate = function
  | Execute { sqlstate; _ } -> sqlstate
  | Cleanup { primary = Some e; _ } -> sqlstate e
  | _ -> None

let rec constraint_name = function
  | Cleanup { primary = Some e; _ } -> constraint_name e
  | Execute { constraint_name; _ } -> constraint_name
  | _ -> None

let rec detail = function
  | Execute { detail; _ } -> detail
  | Cleanup { primary = Some e; _ } -> detail e
  | _ -> None

let is_retryable = function
  | Execute { sqlstate = Some s; _ } -> Sqlstate.is_serialization_failure s
  | _ -> false

let is_unique_violation e =
  match sqlstate e with Some s -> Sqlstate.is_unique_violation s | None -> false

let rec codec_error = function
  | Encode { error; _ } | Decode { error; _ } -> Some error
  | Cleanup { primary = Some e; _ } -> codec_error e
  | _ -> None
