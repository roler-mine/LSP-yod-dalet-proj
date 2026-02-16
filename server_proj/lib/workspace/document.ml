module T = Lsp.Types

type t = {
  uri : T.DocumentUri.t;
  file : string option;

  text : string;
  index : Text_index.t;

  pre_text : string;
  imports : Preprocess.import list;
  compool_def : string option;

  pre_diags : T.Diagnostic.t list;
  import_diags : T.Diagnostic.t list;
  parse_diags : T.Diagnostic.t list;

  ast : Ast.program option;

  diags : T.Diagnostic.t list;
}

let recompute_diags (d:t) : t =
  { d with diags = d.pre_diags @ d.import_diags @ d.parse_diags }

let reparse (doc : t) : t =
  let pre = Preprocess.run ~file:doc.file ~text:doc.text in
  let out = Parse.parse_text ~file:doc.file ~dump_ast:false ~text:pre.text in
  let doc =
    { doc with
      pre_text = pre.text;
      imports = pre.imports;
      compool_def = pre.compool_def;
      pre_diags = pre.diags;
      parse_diags = out.diags;
      ast = out.ast;
      (* keep import_diags; workspace fills it *)
    }
  in
  recompute_diags doc

let make ~(uri:T.DocumentUri.t) ~(file:string option) ~(text:string) : t =
  let doc =
    {
      uri; file;
      text;
      index = Text_index.of_string text;

      pre_text = text;
      imports = [];
      compool_def = None;

      pre_diags = [];
      import_diags = [];
      parse_diags = [];

      ast = None;

      diags = [];
    }
  in
  reparse doc

let diagnostics (d:t) = d.diags
let imports (d:t) = d.imports
let text (d:t) = d.text

let ast_dump (d:t) =
  match d.ast with
  | None -> None
  | Some ast ->
      let opts = { Ast.Debug.default_opts with show_locs = true } in
      Some (Ast.Debug.to_string ~opts ast)

let offset_of_pos (idx : Text_index.t) (p : T.Position.t) : int option =
  let clamp lo hi x =
    if x < lo then lo else if x > hi then hi else x
  in
  let line_count = Text_index.line_count idx in
  if line_count <= 0 then Some 0
  else
    let line = clamp 0 (line_count - 1) p.line in
    match Text_index.line_start_offset idx ~line with
    | None -> Some 0
    | Some start ->
        let line_len =
          match Text_index.line_length idx ~line with
          | Some n -> n
          | None -> 0
        in
        let col = clamp 0 line_len p.character in
        Some (start + col)

let apply_one_change (text : string) (idx : Text_index.t) (c : T.TextDocumentContentChangeEvent.t)
  : (string * Text_index.t) =
  match c.range with
  | None ->
      let text' = c.text in
      (text', Text_index.of_string text')
  | Some r ->
      let sp = r.start in
      let ep = r.end_ in
      match offset_of_pos idx sp, offset_of_pos idx ep with
      | Some a, Some b ->
          let a, b = if a <= b then (a, b) else (b, a) in
          let before = String.sub text 0 a in
          let after_len = String.length text - b in
          let after = if after_len <= 0 then "" else String.sub text b after_len in
          let text' = before ^ c.text ^ after in
          (text', Text_index.of_string text')
      | _ ->
          (* Keep the current text on malformed ranges instead of replacing the whole document. *)
          (text, idx)

let apply_changes (doc : t) (changes : T.TextDocumentContentChangeEvent.t list) : t =
  let (text, index) =
    List.fold_left
      (fun (t, idx) ch -> apply_one_change t idx ch)
      (doc.text, doc.index)
      changes
  in
  { doc with text; index }

let apply_changes_and_reparse ~(changes:T.TextDocumentContentChangeEvent.t list) (doc : t) : t =
  if changes = [] then doc
  else
    let doc' = apply_changes doc changes in
    reparse doc'

let with_import_diags (import_diags:T.Diagnostic.t list) (d:t) : t =
  recompute_diags { d with import_diags }
