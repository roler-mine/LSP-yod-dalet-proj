module T = Lsp.Types

type t = {
  uri : T.DocumentUri.t;
  file : string option;
  rev : int;
  parse_rev : int;
  text : string;
  index : Text_index.t;
  imports : Preprocess.import list;
  compool_def : string option;
  defines : Preprocess.define list;
  pre_diags : T.Diagnostic.t list;
  import_diags : T.Diagnostic.t list;
  parse_diags : T.Diagnostic.t list;
  ast : Ast.program option;
  diags : T.Diagnostic.t list;
}

let recompute_diags (d : t) : t =
  { d with diags = d.pre_diags @ d.import_diags @ d.parse_diags }

let reparse (doc : t) : t =
  let pre = Preprocess.run ~file:doc.file ~text:doc.text in
  let out = Parse.parse_text ~file:doc.file ~dump_ast:false ~text:pre.text in
  let doc =
    {
      doc with
      parse_rev = doc.rev;
      imports = pre.imports;
      compool_def = pre.compool_def;
      defines = pre.defines;
      pre_diags = pre.diags;
      parse_diags = out.diags;
      ast = out.ast;
      (* keep import_diags; workspace fills it *)
    }
  in
  recompute_diags doc

let make ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string) : t =
  let doc =
    {
      uri;
      file;
      rev = 1;
      parse_rev = 1;
      text;
      index = Text_index.of_string text;
      imports = [];
      compool_def = None;
      defines = [];
      pre_diags = [];
      import_diags = [];
      parse_diags = [];
      ast = None;
      diags = [];
    }
  in
  reparse doc

let make_unparsed ~(uri : T.DocumentUri.t) ~(file : string option)
    ~(text : string) ~(parse_diags : T.Diagnostic.t list) : t =
  let doc =
    {
      uri;
      file;
      rev = 1;
      parse_rev = 0;
      text;
      index = Text_index.of_string text;
      imports = [];
      compool_def = None;
      defines = [];
      pre_diags = [];
      import_diags = [];
      parse_diags;
      ast = None;
      diags = [];
    }
  in
  recompute_diags doc

let diagnostics (d : t) = d.diags
let imports (d : t) = d.imports
let text (d : t) = d.text

let ensure_parsed (d : t) : t =
  if d.parse_rev = d.rev && d.ast = None && d.parse_diags = [] then reparse d
  else d

let drop_ast (d : t) : t =
  if d.ast = None || d.parse_rev <> d.rev || d.parse_diags <> [] then d
  else { d with ast = None }

let compare_pos (a : T.Position.t) (b : T.Position.t) : int =
  if a.line <> b.line then compare a.line b.line
  else compare a.character b.character

let normalize_range (r : T.Range.t) : T.Range.t =
  if compare_pos r.start r.end_ <= 0 then r
  else { T.Range.start = r.end_; end_ = r.start }

let ast_dump (d : t) =
  match (ensure_parsed d).ast with
  | None -> None
  | Some ast ->
      let opts = { Ast.Debug.default_opts with show_locs = true } in
      Some (Ast.Debug.to_string ~opts ast)

let offset_of_pos (idx : Text_index.t) (p : T.Position.t) : int option =
  let clamp lo hi x = if x < lo then lo else if x > hi then hi else x in
  let line_count = Text_index.line_count idx in
  if line_count <= 0 then Some 0
  else
    let line = clamp 0 (line_count - 1) p.line in
    match Text_index.line_start_offset idx ~line with
    | None -> Some 0
    | Some start -> (
        let line_len =
          match Text_index.line_length idx ~line with Some n -> n | None -> 0
        in
        let col = clamp 0 line_len p.character in
        match Text_index.offset_of_line_col idx ~line ~col with
        | Some off -> Some off
        | None -> Some (start + col))

let text_region_equals (text : string) ~(offset : int) ~(replacement : string) :
    bool =
  let m = String.length replacement in
  if offset < 0 || offset + m > String.length text then false
  else
    let rec loop i =
      if i >= m then true
      else if text.[offset + i] <> replacement.[i] then false
      else loop (i + 1)
    in
    loop 0

let slice_of_range ~(text : string) ~(index : Text_index.t) (r : T.Range.t) :
    string option =
  let r = normalize_range r in
  match (offset_of_pos index r.start, offset_of_pos index r.end_) with
  | Some a, Some b ->
      let a, b = if a <= b then (a, b) else (b, a) in
      if a < 0 || b < a || b > String.length text then None
      else Some (String.sub text a (b - a))
  | _ -> None

let apply_content_change ~(text : string) ~(index : Text_index.t)
    (c : T.TextDocumentContentChangeEvent.t) : string * Text_index.t =
  match c.range with
  | None ->
      let text' = c.text in
      if text' = text then (text, index) else (text', Text_index.of_string text')
  | Some r -> (
      let r = normalize_range r in
      let sp = r.start in
      let ep = r.end_ in
      match (offset_of_pos index sp, offset_of_pos index ep) with
      | Some a, Some b ->
          let a, b = if a <= b then (a, b) else (b, a) in
          let replaced_len = b - a in
          if
            replaced_len = String.length c.text
            && text_region_equals text ~offset:a ~replacement:c.text
          then (text, index)
          else
            let before = String.sub text 0 a in
            let after_len = String.length text - b in
            let after =
              if after_len <= 0 then "" else String.sub text b after_len
            in
            let text' = before ^ c.text ^ after in
            ( text',
              Text_index.replace_range index ~start_off:a ~end_off:b
                ~replacement:c.text ~text:text' )
      | _ ->
          (* Keep the current text on malformed ranges instead of replacing the whole document. *)
          (text, index))

let apply_changes (doc : t) (changes : T.TextDocumentContentChangeEvent.t list)
    : t =
  let text, index, changed =
    List.fold_left
      (fun (t, idx, changed) ch ->
        let t', idx' = apply_content_change ~text:t ~index:idx ch in
        let changed' = changed || not (t' == t && idx' == idx) in
        (t', idx', changed'))
      (doc.text, doc.index, false)
      changes
  in
  if not changed then doc else { doc with rev = doc.rev + 1; text; index }

let apply_changes_no_reparse
    ~(changes : T.TextDocumentContentChangeEvent.t list) (doc : t) : t =
  if changes = [] then doc else apply_changes doc changes

let apply_changes_and_reparse
    ~(changes : T.TextDocumentContentChangeEvent.t list) (doc : t) : t =
  if changes = [] then doc
  else
    let doc' = apply_changes doc changes in
    reparse doc'

let with_import_diags (import_diags : T.Diagnostic.t list) (d : t) : t =
  recompute_diags { d with import_diags }

let with_parse_diags (parse_diags : T.Diagnostic.t list) (d : t) : t =
  recompute_diags { d with parse_diags; ast = None }
