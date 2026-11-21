(* Exercises the generator's real output (example/generated/db.ml) against a
   stub driver. Compiling it proves the emitter produces valid OCaml; this
   proves the emitted decoders, encoders and wrappers actually work. *)

open Generated

let check what b =
  if b then Printf.printf "ok   %s\n" what
  else (
    Printf.printf "FAIL %s\n" what;
    exit 1)

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  go 0

let uuid_s = "1b4e28ba-2fa1-11d2-883f-0016d3cca427"
let ts = Sqlkit.Value.Text "2026-07-28 09:00:00+00"

module Stub = struct
  type conn = { mutable last_params : Sqlkit.Value.t list }

  let now _ = 0.
  let is_cancellation _ = false
  let with_cleanup = Fun.protect
  let close _ = ()

  let query c ~sql ~params ~columns:_ =
    c.last_params <- params;
    if contains sql "organization_id, email" then
      (* GetUserFull: all 7 columns of users *)
      Ok
        [
          [|
            Sqlkit.Value.Text uuid_s;
            Sqlkit.Value.Text uuid_s;
            Sqlkit.Value.Text "dennis@example.com";
            Sqlkit.Value.Null;
            Sqlkit.Value.Text "banned";
            Sqlkit.Value.Text "1234.50";
            ts;
          |];
        ]
    else if contains sql "post_count" then
      Ok
        [
          [| Sqlkit.Value.Text "a@b.c"; Sqlkit.Value.Null; Sqlkit.Value.Int 0 |];
          [| Sqlkit.Value.Text "c@d.e"; Sqlkit.Value.Text "Hello"; Sqlkit.Value.Int 3 |];
        ]
    else if contains sql "display_name" then
      Ok
        [
          [|
            Sqlkit.Value.Text uuid_s;
            Sqlkit.Value.Text "dennis@example.com";
            Sqlkit.Value.Text "Dennis";
            Sqlkit.Value.Text "active";
            Sqlkit.Value.Text "1234.50";
            ts;
          |];
        ]
    else Ok [ [| Sqlkit.Value.Text uuid_s; Sqlkit.Value.Text "x@y.z"; ts |] ]

  let exec c ~sql:_ ~params =
    c.last_params <- params;
    Ok (Some 1)

  let copy _ ~sql:_ ~rows = Ok (List.length rows)
end

let state = { Stub.last_params = [] }
let conn = Sqlkit.Driver.make (module Stub) state
let uuid = Option.get (Uuidm.of_string uuid_s)

let () =
  (* :one, with an enum and a numeric *)
  (match Db.get_user_exn conn ~id:uuid with
  | Some u ->
      check "get_user decodes" (Option.get u.Db.email = "dennis@example.com");
      check "enum decodes" (Option.get u.Db.status = Db.Active);
      check "numeric decodes" (Decimal.to_string (Option.get u.Db.balance) = "1234.50");
      check "timestamptz decodes" (Ptime.to_year (Option.get u.Db.created_at) = 2026);
      check "uuid decodes" (Uuidm.equal (Option.get u.Db.id) uuid)
  | None -> check "get_user decodes" false);

  (* the shared model type: one helper, two queries *)
  let email_of (u : Db.user_row) = Option.get u.Db.email in
  (match Db.get_user_full_exn conn ~id:uuid with
  | Some u ->
      check "shared model decodes" (email_of u = "dennis@example.com");
      check "second enum label" (Option.get u.Db.status = Db.Banned)
  | None -> check "shared model decodes" false);

  (* :many, with the ? and ! overrides *)
  let rows = Db.count_posts_by_user_exn conn in
  check "many returns 2" (List.length rows = 2);
  check "? override -> option" ((List.nth rows 0).Db.title = None);
  check "! override -> non-option" ((List.nth rows 1).Db.post_count = 3L);

  (* :exec *)
  check "exec returns count" (Db.delete_user_exn conn ~id:uuid = Some 1);

  (* optional argument omitted -> NULL reaches the driver *)
  ignore (Db.set_display_name_exn conn ~id:uuid ());
  check "omitted optional encodes NULL"
    (match state.Stub.last_params with Sqlkit.Value.Null :: _ -> true | _ -> false);

  (* optional argument supplied *)
  ignore (Db.set_display_name_exn conn ~id:uuid ~display_name:"Dennis" ());
  check "supplied optional encodes Text"
    (match state.Stub.last_params with
    | Sqlkit.Value.Text "Dennis" :: _ -> true
    | _ -> false);

  (* encoders: uuid goes out as text *)
  ignore (Db.get_user conn ~id:uuid);
  check "uuid encodes"
    (match state.Stub.last_params with
    | [ Sqlkit.Value.Text s ] -> s = uuid_s
    | _ -> false);

  state.last_params <- [ Sqlkit.Value.Text "untouched" ];
  check "partial optional block returns error"
    (match Db.optional_bounds conn ~lo:1 () with
    | Error (Sqlkit.Error.Encode _) -> true
    | _ -> false);
  check "invalid selection does not execute"
    (state.last_params = [ Sqlkit.Value.Text "untouched" ]);
  check "invalid custom codec returns decode error"
    (Result.is_error (Db.Get_user.decode [| Sqlkit.Value.Text "invalid uuid" |]));
  check "invalid enum returns decode error"
    (Result.is_error
       (Db.Get_user.decode
          [|
            Sqlkit.Value.Text uuid_s;
            Sqlkit.Value.Text "a";
            Sqlkit.Value.Null;
            Sqlkit.Value.Text "unknown";
            Sqlkit.Value.Text "1";
            ts;
          |]));
  print_endline "all good"
