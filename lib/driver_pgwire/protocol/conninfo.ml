type sslmode = Disable | Prefer | Require | Verify_ca | Verify_full
type auth = Scram_sha_256 | Md5 | Password | No_authentication

type t = {
  host : string;
  port : int;
  user : string;
  password : string option;
  database : string;
  sslmode : sslmode;
  sslrootcert : string option;
  require_auth : auth list option;
  connect_timeout : float option;
  application_name : string option;
  options : string option;
}

let ( let* ) = Result.bind
let digits s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

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

(* The URI library accepts almost anything and guesses, which for a credential
   means sending part of a password to whatever host the guess produced. The
   shape is checked here first, and only then handed over for decoding. A
   password containing [@], [:], [/], [?] or [#] must be percent-encoded. *)
let well_formed s =
  let after_scheme =
    let i = String.index s ':' in
    String.sub s (i + 3) (String.length s - i - 3)
  in
  let authority =
    let stop =
      List.fold_left
        (fun stop c ->
          match String.index_opt after_scheme c with Some i -> min stop i | None -> stop)
        (String.length after_scheme) [ '/'; '?' ]
    in
    String.sub after_scheme 0 stop
  in
  let hostport =
    match String.rindex_opt authority '@' with
    | Some i -> String.sub authority (i + 1) (String.length authority - i - 1)
    | None -> authority
  in
  let port_ok p =
    p = ""
    || String.length p > 1
       && p.[0] = ':'
       && digits (String.sub p 1 (String.length p - 1))
  in
  if String.contains s '#' then
    Error "connection URI must not contain '#'; percent-encode it"
  else if List.length (String.split_on_char '@' authority) > 2 then
    Error "connection URI has more than one '@'; percent-encode it in the password"
  else if String.length hostport > 0 && hostport.[0] = '[' then
    match String.index_opt hostport ']' with
    | Some i when port_ok (String.sub hostport (i + 1) (String.length hostport - i - 1))
      ->
        Ok ()
    | _ -> Error "malformed address in connection URI"
  else
    match String.split_on_char ':' hostport with
    | [ _ ] -> Ok ()
    | [ _; port ] when digits port -> Ok ()
    | _ -> Error "malformed host or port in connection URI"

let uri s =
  let* () = well_formed s in
  let u = Uri.of_string s in
  (* the library splits values on commas; put them back *)
  let query = List.map (fun (k, vs) -> (k, String.concat "," vs)) (Uri.query u) in
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

let all_methods = [ Scram_sha_256; Md5; Password; No_authentication ]

(* libpq's require_auth: a comma-separated list of methods, or of negated
   methods, never a mixture. *)
let require_auth_of_string s =
  let name m =
    match String.trim m with
    | "scram-sha-256" -> Ok Scram_sha_256
    | "md5" -> Ok Md5
    | "password" -> Ok Password
    | "none" -> Ok No_authentication
    | other -> Error (Printf.sprintf "unsupported require_auth method %S" other)
  in
  let items = List.map String.trim (String.split_on_char ',' s) in
  let negated m = String.length m > 0 && m.[0] = '!' in
  let rec names acc = function
    | [] -> Ok (List.rev acc)
    | m :: rest ->
        let m = if negated m then String.sub m 1 (String.length m - 1) else m in
        let* a = name m in
        names (a :: acc) rest
  in
  if items = [] || List.mem "" items then Error "empty require_auth"
  else if List.for_all negated items then
    let* refused = names [] items in
    Ok (List.filter (fun m -> not (List.mem m refused)) all_methods)
  else if List.exists negated items then Error "require_auth mixes methods and negations"
  else names [] items

let known =
  [
    "host";
    "port";
    "user";
    "password";
    "dbname";
    "sslmode";
    "sslrootcert";
    "require_auth";
    "application_name";
    "options";
    "connect_timeout";
  ]

(* Parameters whose only accepted value is the one that asks for nothing. They
   are allowed so that a connection string written for libpq, spelling out its
   defaults, does not have to be edited; any other value is refused. *)
let inert =
  [
    ("channel_binding", [ "disable" ]);
    ("gssencmode", [ "disable" ]);
    ("target_session_attrs", [ "any" ]);
    ("sslnegotiation", [ "postgres" ]);
  ]

(* Variables libpq would act on and this driver cannot. Ignoring one would
   quietly weaken or redirect a connection the operator configured through the
   environment. *)
let unsupported_environment () =
  let set k = match Sys.getenv_opt k with Some v when v <> "" -> Some v | _ -> None in
  let refuse k = Option.map (fun _ -> k) (set k) in
  let unless k accepted =
    match set k with Some v when not (List.mem v accepted) -> Some k | _ -> None
  in
  List.find_map Fun.id
    [
      refuse "PGHOSTADDR";
      refuse "PGSERVICE";
      refuse "PGSERVICEFILE";
      refuse "PGSSLCERT";
      refuse "PGSSLKEY";
      refuse "PGPASSFILE";
      refuse "PGKRBSRVNAME";
      refuse "PGGSSLIB";
      unless "PGCHANNELBINDING" [ "disable" ];
      unless "PGSSLNEGOTIATION" [ "postgres" ];
      unless "PGTARGETSESSIONATTRS" [ "any" ];
      unless "PGGSSENCMODE" [ "disable" ];
    ]

let parse s =
  let* pairs =
    if
      String.starts_with ~prefix:"postgresql://" s
      || String.starts_with ~prefix:"postgres://" s
    then uri s
    else keywords s
  in
  let* pairs =
    match
      List.find_opt
        (fun (k, v) ->
          match List.assoc_opt k inert with
          | Some ok -> not (List.mem v ok)
          | None -> false)
        pairs
    with
    | Some (k, v) -> Error (Printf.sprintf "unsupported value %S for %s" v k)
    | None -> Ok (List.filter (fun (k, _) -> not (List.mem_assoc k inert)) pairs)
  in
  let keys = List.map fst pairs in
  match
    ( List.find_opt (fun k -> not (List.mem k known)) keys,
      List.find_opt (fun k -> List.length (List.filter (( = ) k) keys) > 1) keys,
      unsupported_environment () )
  with
  | Some k, _, _ -> Error (Printf.sprintf "unsupported connection parameter %S" k)
  | None, Some k, _ ->
      Error (Printf.sprintf "connection parameter %S given more than once" k)
  | None, None, Some k ->
      Error (Printf.sprintf "unsupported environment variable %s is set" k)
  | None, None, None ->
      let env k =
        match Sys.getenv_opt k with Some v when v <> "" -> Some v | _ -> None
      in
      let get key var =
        match List.assoc_opt key pairs with
        | Some v when v <> "" -> Some v
        | _ -> Option.bind var env
      in
      let host = Option.value (get "host" (Some "PGHOST")) ~default:"localhost" in
      let* port =
        match get "port" (Some "PGPORT") with
        | None -> Ok 5432
        | Some p -> (
            match if digits p then int_of_string_opt p else None with
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
      let* mode =
        match get "sslmode" (Some "PGSSLMODE") with
        | None -> Ok Prefer
        | Some m -> sslmode_of_string m
      in
      let* sslmode, sslrootcert =
        match (mode, get "sslrootcert" (Some "PGSSLROOTCERT")) with
        | (Disable | Prefer), Some "system" | Disable, Some _ ->
            Error "sslrootcert needs sslmode=require or stronger"
        | _, Some "system" -> Ok (Verify_full, None)
        | Require, (Some _ as root) -> Ok (Verify_ca, root)
        | Verify_ca, None -> Error "sslmode=verify-ca needs sslrootcert"
        | mode, root -> Ok (mode, root)
      in
      let* require_auth =
        match get "require_auth" (Some "PGREQUIREAUTH") with
        | None -> Ok None
        | Some r -> Result.map Option.some (require_auth_of_string r)
      in
      let* connect_timeout =
        match get "connect_timeout" (Some "PGCONNECT_TIMEOUT") with
        | None -> Ok None
        | Some t -> (
            match if digits t then int_of_string_opt t else None with
            | Some 0 -> Ok None
            | Some n -> Ok (Some (float_of_int n))
            | None -> Error (Printf.sprintf "invalid connect_timeout %S" t))
      in
      let info =
        {
          host;
          port;
          user;
          password = get "password" (Some "PGPASSWORD");
          database = Option.value (get "dbname" (Some "PGDATABASE")) ~default:user;
          sslmode;
          sslrootcert;
          require_auth;
          connect_timeout;
          application_name = get "application_name" (Some "PGAPPNAME");
          options = get "options" (Some "PGOPTIONS");
        }
      in
      (* After decoding: a percent-encoded NUL would end a field early on the
         wire and let the remainder be read as further startup parameters. *)
      let nul = function Some v -> String.contains v '\000' | None -> false in
      if String.contains host ',' then Error "multiple hosts are not supported"
      else if
        List.exists nul
          [
            Some info.host;
            Some info.user;
            info.password;
            Some info.database;
            info.sslrootcert;
            info.application_name;
            info.options;
          ]
      then Error "NUL byte in a connection parameter"
      else Ok info

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
