(* ---------- frontend ---------- *)

(* PostgreSQL refuses anything longer, and a length that does not fit its
   field must never be written: truncated to 32 bits it would make the server
   read the rest of the payload as new messages. The driver checks sizes before
   building; these raise so that a missed check cannot become a wrapped length. *)
let max_frontend_message = 0x3FFFFFFF
let max_parameters = 0xFFFF

let add_int32 b n =
  if n < 0 || n > max_frontend_message then invalid_arg "Wire: length out of range";
  Buffer.add_int32_be b (Int32.of_int n)

let add_count b n =
  if n < 0 || n > max_parameters then invalid_arg "Wire: count out of range";
  Buffer.add_int16_be b n

let add_cstring b s =
  Buffer.add_string b s;
  Buffer.add_char b '\000'

(* Length words count themselves but not the tag byte. *)
let header b tag body =
  Buffer.add_char b tag;
  add_int32 b (body + 4)

let untagged body =
  let b = Buffer.create (String.length body + 4) in
  add_int32 b (String.length body + 4);
  Buffer.add_string b body;
  Buffer.contents b

let int32_be n =
  let b = Bytes.create 4 in
  Bytes.set_int32_be b 0 n;
  Bytes.to_string b

let ssl_request = untagged (int32_be 80877103l)

let startup params =
  let b = Buffer.create 128 in
  Buffer.add_int32_be b 196608l;
  List.iter
    (fun (k, v) ->
      add_cstring b k;
      add_cstring b v)
    params;
  Buffer.add_char b '\000';
  untagged (Buffer.contents b)

let cancel_request ~pid ~key = untagged (int32_be 80877102l ^ int32_be pid ^ key)

let tagged tag fill =
  let body = Buffer.create 64 in
  fill body;
  let b = Buffer.create (Buffer.length body + 5) in
  header b tag (Buffer.length body);
  Buffer.add_buffer b body;
  Buffer.contents b

let password s = tagged 'p' (fun b -> add_cstring b s)

let sasl_initial ~mechanism data =
  tagged 'p' (fun b ->
      add_cstring b mechanism;
      add_int32 b (String.length data);
      Buffer.add_string b data)

let sasl_response data = tagged 'p' (fun b -> Buffer.add_string b data)

let parse b ~name ~sql =
  header b 'P' (String.length name + 1 + String.length sql + 1 + 2);
  add_cstring b name;
  add_cstring b sql;
  Buffer.add_int16_be b 0

let bind b ~statement params =
  let values =
    Array.fold_left
      (fun n p -> n + 4 + match p with Some s -> String.length s | None -> 0)
      0 params
  in
  header b 'B' (1 + String.length statement + 1 + 2 + 2 + values + 2);
  add_cstring b "";
  add_cstring b statement;
  Buffer.add_int16_be b 0;
  add_count b (Array.length params);
  Array.iter
    (function
      | None -> Buffer.add_int32_be b (-1l)
      | Some s ->
          add_int32 b (String.length s);
          Buffer.add_string b s)
    params;
  Buffer.add_int16_be b 0

let execute b =
  header b 'E' 5;
  add_cstring b "";
  add_int32 b 0

let sync b = header b 'S' 0

let query b sql =
  header b 'Q' (String.length sql + 1);
  add_cstring b sql

let copy_data b data =
  header b 'd' (String.length data);
  Buffer.add_string b data

let copy_done b = header b 'c' 0

let statement_size ~sql params =
  Array.fold_left
    (fun n p -> n + 4 + match p with Some s -> String.length s | None -> 0)
    (String.length sql + 64)
    params

let copy_fail b reason =
  header b 'f' (String.length reason + 1);
  add_cstring b reason

(* ---------- backend ---------- *)

type auth =
  | Auth_ok
  | Cleartext
  | Md5 of string
  | Sasl of string list
  | Sasl_continue of string
  | Sasl_final of string
  | Unsupported of int

type transaction = Idle | In_transaction | Failed

type fields = {
  severity : string;
  message : string;
  sqlstate : string option;
  detail : string option;
  hint : string option;
  constraint_name : string option;
  table_name : string option;
  column_name : string option;
}

type backend =
  | Authentication of auth
  | Backend_key_data of { pid : int32; key : string }
  | Parameter_status of string * string
  | Ready_for_query of transaction
  | Parse_complete
  | Bind_complete
  | Close_complete
  | No_data
  | Empty_query
  | Portal_suspended
  | Row_description
  | Parameter_description
  | Data_row of Sqlkit.Value.t array
  | Command_complete of string
  | Error_response of fields
  | Notice_response of fields
  | Copy_in_response
  | Copy_out_response
  | Copy_both_response
  | Copy_data
  | Copy_done
  | Notification
  | Negotiate_protocol_version

let max_message = 1 lsl 30
let max_startup_message = 64 * 1024

(* Every read below is bounds-checked by the standard library; [decode] turns
   the resulting exception into an error, so a hostile or corrupt payload cannot
   read outside the string. *)
exception Malformed of string

let cstring p pos =
  match String.index_from_opt p pos '\000' with
  | Some stop -> (String.sub p pos (stop - pos), stop + 1)
  | None -> raise (Malformed "unterminated string")

let rest p pos = String.sub p pos (String.length p - pos)

let rec cstrings p pos acc =
  if pos >= String.length p || p.[pos] = '\000' then List.rev acc
  else
    let s, next = cstring p pos in
    cstrings p next (s :: acc)

let authentication p =
  match Int32.to_int (String.get_int32_be p 0) with
  | 0 -> Auth_ok
  | 3 -> Cleartext
  | 5 -> Md5 (String.sub p 4 4)
  | 10 -> Sasl (cstrings p 4 [])
  | 11 -> Sasl_continue (rest p 4)
  | 12 -> Sasl_final (rest p 4)
  | code -> Unsupported code

let data_row p =
  let pos = ref 2 in
  Array.init (String.get_uint16_be p 0) (fun _ ->
      let len = Int32.to_int (String.get_int32_be p !pos) in
      pos := !pos + 4;
      if len < 0 then Sqlkit.Value.Null
      else begin
        let v = String.sub p !pos len in
        pos := !pos + len;
        Sqlkit.Value.Text v
      end)

let fields p =
  let rec go pos acc =
    if pos >= String.length p || p.[pos] = '\000' then acc
    else
      let v, next = cstring p (pos + 1) in
      go next ((p.[pos], v) :: acc)
  in
  let all = go 0 [] in
  let get c = List.assoc_opt c all in
  {
    severity = Option.value (get 'S') ~default:"ERROR";
    message = Option.value (get 'M') ~default:"";
    sqlstate = get 'C';
    detail = get 'D';
    hint = get 'H';
    constraint_name = get 'n';
    table_name = get 't';
    column_name = get 'c';
  }

let transaction p =
  match p.[0] with
  | 'I' -> Idle
  | 'T' -> In_transaction
  | 'E' -> Failed
  | c -> raise (Malformed (Printf.sprintf "transaction status %C" c))

let decode tag p =
  match
    match tag with
    | 'R' -> Authentication (authentication p)
    | 'K' -> Backend_key_data { pid = String.get_int32_be p 0; key = rest p 4 }
    | 'S' ->
        let k, next = cstring p 0 in
        let v, _ = cstring p next in
        Parameter_status (k, v)
    | 'Z' -> Ready_for_query (transaction p)
    | '1' -> Parse_complete
    | '2' -> Bind_complete
    | '3' -> Close_complete
    | 'n' -> No_data
    | 'I' -> Empty_query
    | 's' -> Portal_suspended
    | 'T' -> Row_description
    | 't' -> Parameter_description
    | 'D' -> Data_row (data_row p)
    | 'C' -> Command_complete (fst (cstring p 0))
    | 'E' -> Error_response (fields p)
    | 'N' -> Notice_response (fields p)
    | 'G' -> Copy_in_response
    | 'H' -> Copy_out_response
    | 'W' -> Copy_both_response
    | 'd' -> Copy_data
    | 'c' -> Copy_done
    | 'A' -> Notification
    | 'v' -> Negotiate_protocol_version
    | c -> raise (Malformed (Printf.sprintf "unknown message type %C" c))
  with
  | m -> Ok m
  | exception Malformed why -> Error ("malformed server message: " ^ why)
  | exception Invalid_argument _ ->
      Error (Printf.sprintf "malformed server message: truncated %C" tag)

let affected tag =
  match String.split_on_char ' ' tag with
  | ("INSERT" | "UPDATE" | "DELETE" | "SELECT" | "MERGE" | "MOVE" | "FETCH" | "COPY")
    :: counts -> (
      match List.rev counts with n :: _ -> int_of_string_opt n | [] -> None)
  | _ -> None
