type sslmode = Disable | Prefer | Require | Verify_ca | Verify_full

type t = {
  host : string;
  port : int;
  user : string;
  password : string option;
  database : string;
  sslmode : sslmode;
  sslrootcert : string option;
  application_name : string option;
  options : string option;
}

let ( let* ) = Result.bind

(* key=value pairs separated by whitespace; a value may be single-quoted, and
   inside quotes a backslash escapes the next character. *)
let keywords s =
  let n = String.length s in
  let space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec skip i = if i < n && space s.[i] then skip (i + 1) else i in
  let rec pairs i acc =
    let i = skip i in
    if i >= n then Ok (List.rev acc)
    else
      match String.index_from_opt s i '=' with
      | None -> Error "expected key=value in connection string"
      | Some eq ->
          let key = String.trim (String.sub s i (eq - i)) in
          let b = Buffer.create 16 in
          let start = skip (eq + 1) in
          let rec quoted j =
            if j >= n then Error "unterminated quote in connection string"
            else if s.[j] = '\'' then Ok (j + 1)
            else if s.[j] = '\\' && j + 1 < n then (
              Buffer.add_char b s.[j + 1];
              quoted (j + 2))
            else (
              Buffer.add_char b s.[j];
              quoted (j + 1))
          in
          let rec bare j =
            if j >= n || space s.[j] then j
            else if s.[j] = '\\' && j + 1 < n then (
              Buffer.add_char b s.[j + 1];
              bare (j + 2))
            else (
              Buffer.add_char b s.[j];
              bare (j + 1))
          in
          let* stop =
            if start < n && s.[start] = '\'' then quoted (start + 1) else Ok (bare start)
          in
          if key = "" then Error "empty key in connection string"
          else pairs stop ((key, Buffer.contents b) :: acc)
  in
  pairs 0 []

let uri s =
  let u = Uri.of_string s in
  let query =
    List.filter_map
      (fun (k, vs) -> match vs with [ v ] -> Some (k, v) | _ -> None)
      (Uri.query u)
  in
  let some k v = match v with Some v when v <> "" -> [ (k, v) ] | _ -> [] in
  let path = Uri.pct_decode (Uri.path u) in
  let database =
    if String.length path > 1 then Some (String.sub path 1 (String.length path - 1))
    else None
  in
  (* later bindings win, so query parameters override the authority part *)
  Ok
    (some "host" (Uri.host u)
    @ some "port" (Option.map string_of_int (Uri.port u))
    @ some "user" (Uri.user u)
    @ some "password" (Uri.password u)
    @ some "dbname" database @ query)

let sslmode_of_string = function
  | "disable" -> Ok Disable
  | "allow" | "prefer" -> Ok Prefer
  | "require" -> Ok Require
  | "verify-ca" -> Ok Verify_ca
  | "verify-full" -> Ok Verify_full
  | other -> Error (Printf.sprintf "unknown sslmode %S" other)

let known =
  [
    "host";
    "port";
    "user";
    "password";
    "dbname";
    "sslmode";
    "sslrootcert";
    "application_name";
    "options";
    "connect_timeout";
  ]

let parse s =
  let* pairs =
    if
      String.starts_with ~prefix:"postgresql://" s
      || String.starts_with ~prefix:"postgres://" s
    then uri s
    else keywords s
  in
  match List.find_opt (fun (k, _) -> not (List.mem k known)) pairs with
  | Some (k, _) -> Error (Printf.sprintf "unsupported connection parameter %S" k)
  | None ->
      let env k =
        match Sys.getenv_opt k with Some v when v <> "" -> Some v | _ -> None
      in
      let get key var =
        match List.assoc_opt key (List.rev pairs) with
        | Some v when v <> "" -> Some v
        | _ -> Option.bind var env
      in
      let host = Option.value (get "host" (Some "PGHOST")) ~default:"localhost" in
      let* port =
        match get "port" (Some "PGPORT") with
        | None -> Ok 5432
        | Some p -> (
            match int_of_string_opt p with
            | Some n when n > 0 && n < 65536 -> Ok n
            | _ -> Error (Printf.sprintf "invalid port %S" p))
      in
      let* user =
        match get "user" (Some "PGUSER") with
        | Some u -> Ok u
        | None -> (
            match env "USER" with
            | Some u -> Ok u
            | None -> Error "no user in connection string, PGUSER or USER")
      in
      let* sslmode =
        match get "sslmode" (Some "PGSSLMODE") with
        | None -> Ok Prefer
        | Some m -> sslmode_of_string m
      in
      if String.contains host ',' then Error "multiple hosts are not supported"
      else
        Ok
          {
            host;
            port;
            user;
            password = get "password" (Some "PGPASSWORD");
            database = Option.value (get "dbname" (Some "PGDATABASE")) ~default:user;
            sslmode;
            sslrootcert = get "sslrootcert" (Some "PGSSLROOTCERT");
            application_name = get "application_name" (Some "PGAPPNAME");
            options = get "options" (Some "PGOPTIONS");
          }

let of_env () =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some u when String.trim u <> "" -> u
  | _ ->
      let get k d = match Sys.getenv_opt k with Some v when v <> "" -> v | _ -> d in
      let quote v =
        let b = Buffer.create (String.length v + 2) in
        Buffer.add_char b '\'';
        String.iter
          (fun c ->
            if c = '\'' || c = '\\' then Buffer.add_char b '\\';
            Buffer.add_char b c)
          v;
        Buffer.add_char b '\'';
        Buffer.contents b
      in
      Printf.sprintf "host=%s port=%s user=%s dbname=%s%s"
        (quote (get "PGHOST" "127.0.0.1"))
        (quote (get "PGPORT" "5432"))
        (quote (get "PGUSER" "postgres"))
        (quote (get "PGDATABASE" "postgres"))
        (match Sys.getenv_opt "PGPASSWORD" with
        | Some p when p <> "" -> " password=" ^ quote p
        | _ -> "")
