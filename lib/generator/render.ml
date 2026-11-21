(* See render.mli. No decisions are made here; anything requiring judgement
   belongs in [Resolve]. *)

open Resolve

let bprintf = Printf.bprintf

let emit_record b name fields =
  bprintf b "type %s =\n" name;
  List.iteri
    (fun i f ->
      bprintf b "  %c %s : %s\n"
        (if i = 0 then '{' else ';')
        f.fname (Typemap.ocaml_type f.ftype))
    fields;
  bprintf b "  }\n\n"

let emit_enum_types b enums =
  List.iter
    (fun (name, labels) ->
      bprintf b "type %s =\n" name;
      if labels = [] then bprintf b "  |\n";
      List.iter (fun l -> bprintf b "  | %s\n" (Typemap.constructor_of_label l)) labels;
      bprintf b "\n")
    enums

(* A SQL docstring containing a comment terminator, or an unbalanced comment
   opener, would break the generated (** ... *) block; neutralise both by
   splitting the two-character sequences. *)
let sanitize_doc line =
  let b = Buffer.create (String.length line) in
  String.iteri
    (fun i c ->
      if c = '*' && i + 1 < String.length line && line.[i + 1] = ')' then
        Buffer.add_string b "* "
      else if c = '(' && i + 1 < String.length line && line.[i + 1] = '*' then
        Buffer.add_string b "( "
      else Buffer.add_char b c)
    line;
  Buffer.contents b

let emit_doc b (q : Parse.t) =
  match q.Parse.doc with
  | [] -> ()
  | lines ->
      bprintf b "(** %s *)\n" (String.concat "\n    " (List.map sanitize_doc lines))

(* mandatory args first (in SQL order), then optional; a trailing unit only when
   there is at least one optional *)
let split_args ps = List.partition (fun f -> not (Typemap.is_option f.ftype)) ps

let arg_sig f =
  if Typemap.is_option f.ftype then
    Printf.sprintf "?%s:%s" f.fname (Typemap.ocaml_type (Typemap.strip_option f.ftype))
  else Printf.sprintf "%s:%s" f.fname (Typemap.ocaml_type f.ftype)

let arg_use f = (if Typemap.is_option f.ftype then "?" else "~") ^ f.fname

let result_type r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> Printf.sprintf "%s option" (Option.value r.row_type ~default:"unit")
  | Parse.One_strict -> Option.value r.row_type ~default:"unit"
  | Parse.Many -> Printf.sprintf "%s list" (Option.value r.row_type ~default:"unit")
  | Parse.Exec -> "int option"
  | Parse.Copy -> "int"

let runner r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> "Sqlkit.fetch_one"
  | Parse.One_strict -> "Sqlkit.fetch_one_strict"
  | Parse.Many -> "Sqlkit.fetch_all"
  | Parse.Exec -> "Sqlkit.exec"
  | Parse.Copy -> "Sqlkit.copy"

let query_sig r =
  match r.d.Describe.query.Parse.cardinality with
  | Parse.One -> "Sqlkit.Query.ONE"
  | Parse.One_strict -> "Sqlkit.Query.ONE_STRICT"
  | Parse.Many -> "Sqlkit.Query.MANY"
  | Parse.Exec -> "Sqlkit.Query.EXEC"
  | Parse.Copy -> "Sqlkit.Query.COPY"

let fn_name r = Parse.to_snake r.d.Describe.query.Parse.name
let mod_name r = r.d.Describe.query.Parse.module_name

let emit_signature b r =
  let m = mod_name r in
  let q = r.d.Describe.query in
  bprintf b "module %s : sig\n" m;
  if r.ps = [] then bprintf b "  type params = unit\n\n"
  else begin
    bprintf b "  type params =\n";
    List.iteri
      (fun i f ->
        bprintf b "    %c %s : %s\n"
          (if i = 0 then '{' else ';')
          f.fname (Typemap.ocaml_type f.ftype))
      r.ps;
    bprintf b "    }\n\n"
  end;
  (match r.row_type with
  | Some row ->
      bprintf b "  include %s with type params := params and type row = %s\n"
        (query_sig r) row
  | None -> bprintf b "  include %s with type params := params\n" (query_sig r));
  bprintf b "end\n\n";
  match r.copy with
  | Some _ ->
      (* one COPY round-trip per call: the argument is the whole row list *)
      emit_doc b q;
      bprintf b
        "val %s : Sqlkit.conn -> %s.params list -> (int, Sqlkit.Error.t) result\n\n"
        (fn_name r) m;
      bprintf b
        "(** Raising {!%s}.\n\
        \    @raise Sqlkit.Sql_error on connection or execution failure. *)\n"
        (fn_name r);
      bprintf b "val %s_exn : Sqlkit.conn -> %s.params list -> int\n\n" (fn_name r) m
  | None ->
      let mandatory, optional = split_args r.ps in
      let args = List.map arg_sig (mandatory @ optional) in
      let args = if optional = [] then args else args @ [ "unit" ] in
      let chain = String.concat " -> " ("Sqlkit.conn" :: args) in
      emit_doc b q;
      bprintf b "val %s : %s -> (%s, Sqlkit.Error.t) result\n\n" (fn_name r) chain
        (result_type r);
      bprintf b
        "(** Raising {!%s}.\n\
        \    @raise Sqlkit.Sql_error on connection, execution or decode failure. *)\n"
        (fn_name r);
      bprintf b "val %s_exn : %s -> %s\n\n" (fn_name r) chain (result_type r)

let emit_implementation b r =
  let m = mod_name r in
  let q = r.d.Describe.query in
  bprintf b "module %s = struct\n" m;
  if r.ps = [] then bprintf b "  type params = unit\n"
  else begin
    bprintf b "  type params =\n";
    List.iteri
      (fun i f ->
        bprintf b "    %c %s : %s\n"
          (if i = 0 then '{' else ';')
          f.fname (Typemap.ocaml_type f.ftype))
      r.ps;
    bprintf b "    }\n"
  end;
  (match r.row_type with Some row -> bprintf b "  type row = %s\n" row | None -> ());
  bprintf b "\n  let name = %S\n" q.Parse.name;
  (match r.copy with
  | Some (table, cols) ->
      bprintf b "  let copy_sql = %S\n\n"
        (Printf.sprintf "COPY %s (%s) FROM STDIN" table (String.concat ", " cols))
  | None -> ());
  (match q.Parse.dynamic with
  | None when r.copy <> None -> ()
  | None -> bprintf b "  let sql (_ : params) = Ok %S\n\n" q.Parse.sql
  | Some d ->
      (* one pre-verified SQL text per inclusion combination, indexed by which
         optional blocks are active *)
      bprintf b "\n  let variants =\n    [|\n";
      Array.iter (fun v -> bprintf b "      %S;\n" v) d.Parse.variant_sqls;
      bprintf b "    |]\n");
  (match q.Parse.dynamic with
  | None -> ()
  | Some d ->
      bprintf b "\n  let sql (p : params) =\n";
      Array.iteri
        (fun k bp ->
          match bp with
          | [ one ] ->
              bprintf b
                "    let* b%d = match p.%s with Some _ -> Ok true | None -> Ok false in\n"
                k one
          | many ->
              let somes = String.concat ", " (List.map (fun _ -> "Some _") many) in
              let nones = String.concat ", " (List.map (fun _ -> "None") many) in
              let tuple = String.concat ", " (List.map (fun n -> "p." ^ n) many) in
              bprintf b
                "    let* b%d =\n\
                \      match (%s) with\n\
                \      | %s -> Ok true\n\
                \      | %s -> Ok false\n\
                \      | _ -> Error (Sqlkit.Codec.error \
                 (Sqlkit.Codec.Inconsistent_parameters [%s]))\n\
                \    in\n"
                k tuple somes nones
                (String.concat "; " (List.map (Printf.sprintf "%S") many)))
        d.Parse.block_params;
      let mask =
        String.concat " lor "
          (List.init d.Parse.nblocks (fun k ->
               Printf.sprintf "(if b%d then %d else 0)" k (1 lsl k)))
      in
      bprintf b "    Ok variants.(%s)\n\n" mask);
  (* [p.field] rather than a pattern: params and row routinely share field names
     and OCaml would resolve an unannotated pattern to the later type *)
  let blocks = block_param_names q in
  bprintf b "  let encode (%s) =\n" (if r.ps = [] then "()" else "p : params");
  List.iteri
    (fun i f ->
      if List.mem f.fname blocks then
        bprintf b
          "    let* v%d = Sqlkit.Codec.context (Sqlkit.Codec.Parameter %S) (match p.%s \
           with None -> Ok [] | Some v -> Result.map (fun x -> [x]) (Result.bind (%s v) \
           Sqlkit.Value.validate)) in\n"
          i f.fname f.fname
          (Typemap.encoder (Typemap.strip_option f.ftype))
      else
        bprintf b
          "    let* v%d = Sqlkit.Codec.context (Sqlkit.Codec.Parameter %S) (Result.map \
           (fun x -> [x]) (Result.bind (%s p.%s) Sqlkit.Value.validate)) in\n"
          i f.fname (Typemap.encoder f.ftype) f.fname)
    r.ps;
  bprintf b "    Ok (List.concat [%s])\n"
    (String.concat "; " (List.mapi (fun i _ -> "v" ^ string_of_int i) r.ps));
  (match r.row_type with
  | None -> ()
  | Some row ->
      bprintf b "\n  let decode r : (%s, Sqlkit.Row.error) result =\n" row;
      List.iteri
        (fun i f ->
          bprintf b "    let* v%d = Sqlkit.Row.named %S (%s r %d) in\n" i f.fname
            (Typemap.decoder f.ftype) i)
        r.cols;
      bprintf b "    Ok ({ %s } : %s)\n"
        (String.concat "; "
           (List.mapi (fun i f -> f.fname ^ " = v" ^ string_of_int i) r.cols))
        row);
  (match r.row_type with
  | Some _ -> bprintf b "\n  let columns = %d\n" (List.length r.cols)
  | None -> ());
  bprintf b "\n  let cardinality = %s\n"
    (match r.d.Describe.query.Parse.cardinality with
    | Parse.One -> "Sqlkit.Query.One"
    | Parse.One_strict -> "Sqlkit.Query.One_strict"
    | Parse.Many -> "Sqlkit.Query.Many"
    | Parse.Exec -> "Sqlkit.Query.Exec"
    | Parse.Copy -> "Sqlkit.Query.Copy");
  bprintf b "end\n\n";
  match r.copy with
  | Some _ ->
      bprintf b "let %s conn rows = Sqlkit.copy (module %s) conn rows\n" (fn_name r) m;
      bprintf b "let %s_exn conn rows = Sqlkit.get_exn (%s conn rows)\n\n" (fn_name r)
        (fn_name r)
  | None ->
      let mandatory, optional = split_args r.ps in
      let all = mandatory @ optional in
      let uses =
        match all with [] -> "" | _ -> " " ^ String.concat " " (List.map arg_use all)
      in
      let tail = if optional = [] then "" else " ()" in
      let param_value =
        if r.ps = [] then "()"
        else "{ " ^ m ^ "." ^ String.concat "; " (List.map (fun f -> f.fname) r.ps) ^ " }"
      in
      bprintf b "let %s conn%s%s = %s (module %s) conn %s\n" (fn_name r) uses tail
        (runner r) m param_value;
      bprintf b "let %s_exn conn%s%s = Sqlkit.get_exn (%s conn%s%s)\n\n" (fn_name r) uses
        tail (fn_name r) uses tail

let header src =
  Printf.sprintf
    "(* Generated by sqlkit from %s -- do not edit.\n\n\
    \   Regenerate with: sqlkit generate *)\n\n"
    src

let emit_composites b composites =
  List.iteri
    (fun i (name, fields) ->
      bprintf b "%s %s = " (if i = 0 then "type" else "and") name;
      if fields = [] then bprintf b "unit\n"
      else begin
        bprintf b "{\n";
        List.iter
          (fun f -> bprintf b "  %s : %s;\n" f.fname (Typemap.ocaml_type f.ftype))
          fields;
        bprintf b "}\n"
      end)
    composites;
  if composites <> [] then bprintf b "\n"

let emit_composite_codecs b composites =
  List.iteri
    (fun i (name, fields) ->
      bprintf b "%s %s_of_string s : (%s, Sqlkit.Codec.error) result =\n"
        (if i = 0 then "let rec" else "and")
        name name;
      bprintf b "  let* fields = Sqlkit.Composite.of_string ~names:[|%s|] s in\n"
        (String.concat "; " (List.map (fun f -> Printf.sprintf "%S" f.fname) fields));
      let count = if fields = [] then 1 else List.length fields in
      bprintf b
        "  if Array.length fields <> %d then Error (Sqlkit.Codec.error \
         (Sqlkit.Codec.Field_count {type_name=%S;expected=%d;actual=Array.length \
         fields})) else\n"
        count name count;
      List.iteri
        (fun j f ->
          bprintf b "  let* v%d = Sqlkit.Composite.field ~name:%S %s fields %d in\n" j
            f.fname
            (Typemap.elem_parser (Typemap.strip_option f.ftype))
            j)
        fields;
      if fields = [] then bprintf b "  Ok ()\n"
      else
        bprintf b "  Ok ({ %s } : %s)\n"
          (String.concat "; "
             (List.mapi (fun j f -> f.fname ^ " = v" ^ string_of_int j) fields))
          name;
      bprintf b "and %s_to_string (v : %s) =\n" name name;
      if fields = [] then bprintf b "  let () = v in Ok \"()\"\n"
      else begin
        List.iteri
          (fun j f ->
            bprintf b
              "  let* v%d = Sqlkit.Codec.context (Sqlkit.Codec.Field {index=%d;name=Some \
               %S}) (Sqlkit.Codec.option %s v.%s) in\n"
              j j f.fname
              (Typemap.elem_printer (Typemap.strip_option f.ftype))
              f.fname)
          fields;
        bprintf b "  Ok (Sqlkit.Composite.to_string [| %s |])\n"
          (String.concat "; " (List.mapi (fun j _ -> "v" ^ string_of_int j) fields))
      end)
    composites;
  if composites <> [] then
    List.iter
      (fun (n, _) -> bprintf b "let _ = %s_of_string, %s_to_string\n" n n)
      composites

let source ~src ~enums ~composites ~rows resolved =
  if
    List.exists
      (fun r ->
        match (r.d.Describe.query.Parse.cardinality, r.row_type) with
        | (Parse.One | Parse.One_strict | Parse.Many), None -> true
        | _ -> false)
      resolved
  then Diag.error "render: result query is missing its resolved row type"
  else
    let mli = Buffer.create 4096 in
    let ml = Buffer.create 8192 in
    Buffer.add_string mli (header src);
    Buffer.add_string ml (header src);
    (* generated row records have fields the caller may not read; warning 69 fires
     in the defining module, which the caller cannot fix *)
    Buffer.add_string ml "[@@@warning \"-69-30-39\"]\n\n";
    Buffer.add_string ml "let ( let* ) = Result.bind\nlet _ = ( let* )\n\n";
    emit_enum_types mli enums;
    emit_enum_types ml enums;
    List.iter
      (fun (name, labels) ->
        bprintf mli "val %s_to_string : %s -> string\n\n" name name;
        bprintf ml "let %s_to_string (v : %s) = match v with\n" name name;
        if labels = [] then bprintf ml "  | _ -> .\n";
        List.iter
          (fun l -> bprintf ml "  | %s -> %S\n" (Typemap.constructor_of_label l) l)
          labels;
        bprintf ml "\n";
        bprintf ml "let %s_to_value x = Sqlkit.Value.of_string (%s_to_string x)\n" name
          name;
        bprintf ml "let _ = %s_to_value\n\n" name;
        bprintf ml "let %s_of_string s : (%s, Sqlkit.Codec.error) result = match s with\n"
          name name;
        List.iter
          (fun l -> bprintf ml "  | %S -> Ok %s\n" l (Typemap.constructor_of_label l))
          labels;
        bprintf ml
          "  | value -> Error (Sqlkit.Codec.error (Sqlkit.Codec.Unknown_enum \
           {type_name=%S;value;allowed=[%s]}))\n\n"
          name
          (String.concat "; " (List.map (Printf.sprintf "%S") labels));
        bprintf ml "let %s_of_row r i = Sqlkit.Row.custom %s_of_string r i\n" name name;
        bprintf ml "let _ = %s_of_row\n\n" name)
      enums;
    emit_composites mli composites;
    emit_composites ml composites;
    emit_composite_codecs ml composites;
    List.iter
      (fun (name, fields) ->
        emit_record mli name fields;
        emit_record ml name fields)
      rows;
    List.iter
      (fun r ->
        emit_signature mli r;
        emit_implementation ml r)
      resolved;
    Ok (Buffer.contents mli, Buffer.contents ml)
