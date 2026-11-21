open Codec

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      if c = '"' || c = '\\' then Buffer.add_char b '\\';
      Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let token s pos ~stop =
  let n = String.length s and b = Buffer.create 16 in
  let quoted = !pos < n && s.[!pos] = '"' in
  let escaped = ref false in
  let rec read () =
    if !pos >= n then
      if quoted then
        Error (syntax ~offset:!pos "container" "unterminated quoted container value")
      else Ok ()
    else if (not quoted) && stop s.[!pos] then Ok ()
    else begin
      let c = s.[!pos] in
      incr pos;
      if c = '\\' then begin
        escaped := true;
        if !pos >= n then Error (syntax ~offset:!pos "container" "unterminated escape")
        else (
          Buffer.add_char b s.[!pos];
          incr pos;
          read ())
      end
      else if c = '"' then
        if not quoted then Error (syntax ~offset:!pos "container" "unexpected quote")
        else if !pos < n && s.[!pos] = '"' then (
          Buffer.add_char b '"';
          incr pos;
          read ())
        else Ok ()
      else (
        Buffer.add_char b c;
        read ())
    end
  in
  if quoted then incr pos;
  let* () = read () in
  Ok (Buffer.contents b, quoted || !escaped)

let context = Codec.context
