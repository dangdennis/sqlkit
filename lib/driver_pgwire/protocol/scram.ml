let mechanism = "SCRAM-SHA-256"
let hmac ~key s = Digestif.SHA256.(to_raw_string (hmac_string ~key s))
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

let xor a b =
  String.init (String.length a) (fun i -> Char.chr (Char.code a.[i] lxor Char.code b.[i]))

(* PBKDF2 with HMAC-SHA-256 and one output block, which is all SCRAM asks for.
   The server picks the iteration count, so the work is done in slices with a
   pause between them: the caller's scheduler keeps running and its deadline
   can end the derivation. *)
let slice = 2048

let salted_password ~pause ~password ~salt ~iterations =
  let first = hmac ~key:password (salt ^ "\000\000\000\001") in
  let rec go u acc n =
    if n = 0 then acc
    else begin
      if n mod slice = 0 then pause ();
      let u = hmac ~key:password u in
      go u (xor acc u) (n - 1)
    end
  in
  go first first (iterations - 1)

let bare nonce = "n=,r=" ^ nonce
let client_first ~nonce = "n,," ^ bare nonce

type expected = string

(* A server is free to pick the cost, so an absurd one is a way to burn the
   client's CPU before it has authenticated anybody. PostgreSQL's default is
   4096; this leaves two orders of magnitude of headroom. *)
let max_iterations = 600_000

let attribute key parts =
  List.find_map
    (fun part ->
      if String.length part >= 2 && part.[0] = key && part.[1] = '=' then
        Some (String.sub part 2 (String.length part - 2))
      else None)
    parts

let decimal s =
  if
    s <> ""
    && String.length s <= 9
    && String.for_all (function '0' .. '9' -> true | _ -> false) s
  then int_of_string_opt s
  else None

let client_final ?(pause = ignore) ~password ~nonce ~server_first () =
  let parts = String.split_on_char ',' server_first in
  if attribute 'm' parts <> None then
    Error "server requires an unsupported SCRAM extension"
  else
    match (attribute 'r' parts, attribute 's' parts, attribute 'i' parts) with
    | Some combined, Some salt, Some iterations -> (
        match (Base64.decode salt, decimal iterations) with
        | Ok salt, Some iterations
          when salt <> "" && iterations >= 1 && iterations <= max_iterations
               && String.length combined > String.length nonce
               && String.starts_with ~prefix:nonce combined ->
            let salted = salted_password ~pause ~password ~salt ~iterations in
            let client_key = hmac ~key:salted "Client Key" in
            let without_proof = "c=biws,r=" ^ combined in
            let auth = String.concat "," [ bare nonce; server_first; without_proof ] in
            let proof = xor client_key (hmac ~key:(sha256 client_key) auth) in
            let signature = hmac ~key:(hmac ~key:salted "Server Key") auth in
            Ok (without_proof ^ ",p=" ^ Base64.encode_string proof, signature)
        | _ -> Error "server sent an unacceptable SCRAM challenge")
    | _ -> Error "server sent a malformed SCRAM challenge"

let verify expected ~server_final =
  match attribute 'v' (String.split_on_char ',' server_final) with
  | Some v -> (
      match Base64.decode v with Ok v -> Eqaf.equal v expected | Error _ -> false)
  | None -> false

let md5 ~user ~password ~salt =
  let hex s = Digestif.MD5.(to_hex (digest_string s)) in
  "md5" ^ hex (hex (password ^ user) ^ salt)
