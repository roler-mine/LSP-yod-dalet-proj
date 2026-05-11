module T = Lsp.Types

type t = {
  uri : T.DocumentUri.t;
  file : string option;
  lsp_version : int option;
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
  syntax : Syntax_cache.t option;
  diags : T.Diagnostic.t list;
}

type parsed_payload = {
  parsed_rev : int;
  parsed_ast : Ast.program option;
  parsed_syntax : Syntax_cache.t option;
  parsed_diags : T.Diagnostic.t list;
}

let recompute_diags (d : t) : t =
  { d with diags = d.pre_diags @ d.import_diags @ d.parse_diags }

let reparse_with_profile ?previous_syntax ?edit_summary
    ~(profile : Parser.profile) (doc : t) : t =
  let t0 = Perf_log.now_ms () in
  let syntax =
    Syntax_cache.build_with_profile ?previous:previous_syntax ?edit_summary
      ~profile ~file:doc.file ~text:doc.text
      ()
  in
  let elapsed_ms = max 0.0 (Perf_log.now_ms () -. t0) in
  let uri_s = Uri_path.docuri_to_string doc.uri in
  let metrics = Syntax_cache.metrics syntax in
  Perf_log.log_event "file_tokenization" ~uri:uri_s
    ~bytes:(String.length doc.text) ~rev:doc.rev
    ~queue:metrics.lexed_token_count;
  Perf_log.log_event "file_skeleton_build" ~uri:uri_s
    ~bytes:(String.length doc.text) ~rev:doc.rev
    ~queue:(List.length syntax.skeleton.symbols);
  Perf_log.log_event "file_full_parse" ~uri:uri_s
    ~bytes:(String.length doc.text) ~rev:doc.rev
    ~ms:syntax.parse_duration_ms;
  Perf_log.log_event "file_parse_pipeline" ~uri:uri_s
    ~bytes:(String.length doc.text) ~rev:doc.rev ~ms:elapsed_ms;
  let pre = syntax.preprocess in
  let out = syntax.parse in
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
      syntax = Some syntax;
      (* keep import_diags; workspace fills it *)
    }
  in
  recompute_diags doc

let make_with_profile_versioned ~(lsp_version : int option)
    ~(profile : Parser.profile) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : t =
  let doc =
    {
      uri;
      file;
      lsp_version;
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
      syntax = None;
      diags = [];
    }
  in
  reparse_with_profile ~profile doc

let make_with_profile ~(profile : Parser.profile) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : t =
  make_with_profile_versioned ~lsp_version:None ~profile ~uri ~file ~text

let make_versioned ~(lsp_version : int option) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string) : t =
  make_with_profile_versioned ~lsp_version ~profile:Parser.Interactive ~uri
    ~file ~text

let make ~(uri : T.DocumentUri.t) ~(file : string option) ~(text : string) : t =
  make_versioned ~lsp_version:None ~uri ~file ~text

let reparse (doc : t) : t =
  reparse_with_profile ?previous_syntax:doc.syntax
    ~profile:Parser.Interactive doc

let reparse_for_profile ~(profile : Parser.profile) (doc : t) : t =
  reparse_with_profile ?previous_syntax:doc.syntax ~profile doc

let make_unparsed_versioned ~(lsp_version : int option) ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string)
    ~(parse_diags : T.Diagnostic.t list) : t =
  let doc =
    {
      uri;
      file;
      lsp_version;
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
      syntax = None;
      diags = [];
    }
  in
  recompute_diags doc

let make_unparsed ~(uri : T.DocumentUri.t) ~(file : string option)
    ~(text : string) ~(parse_diags : T.Diagnostic.t list) : t =
  make_unparsed_versioned ~lsp_version:None ~uri ~file ~text ~parse_diags

let make_parse_skipped_versioned ~(lsp_version : int option)
    ~(uri : T.DocumentUri.t)
    ~(file : string option) ~(text : string)
    ~(parse_diags : T.Diagnostic.t list) : t =
  let doc =
    {
      uri;
      file;
      lsp_version;
      rev = 1;
      parse_rev = 1;
      text;
      index = Text_index.of_string text;
      imports = [];
      compool_def = None;
      defines = [];
      pre_diags = [];
      import_diags = [];
      parse_diags;
      ast = None;
      syntax = None;
      diags = [];
    }
  in
  recompute_diags doc

let make_parse_skipped ~(uri : T.DocumentUri.t) ~(file : string option)
    ~(text : string) ~(parse_diags : T.Diagnostic.t list) : t =
  make_parse_skipped_versioned ~lsp_version:None ~uri ~file ~text ~parse_diags

let diagnostics (d : t) = d.diags
let imports (d : t) = d.imports
let text (d : t) = d.text
let lsp_version (d : t) = d.lsp_version

let with_lsp_version (lsp_version : int option) (d : t) : t =
  { d with lsp_version }

let ensure_parsed (d : t) : t =
  if d.parse_rev <> d.rev then (
    match Perf_log.current_request_kind () with
    | Some kind when not (Perf_log.request_allows_sync_parse kind) ->
        Perf_log.record_sync_full_parse
          ~uri:(Uri_path.docuri_to_string d.uri)
          ~bytes:(String.length d.text) ~rev:d.rev ();
        d
    | _ ->
        Perf_log.record_sync_full_parse
          ~uri:(Uri_path.docuri_to_string d.uri)
          ~bytes:(String.length d.text) ~rev:d.rev ();
        reparse d)
  else d

let current_parse (d : t) : parsed_payload option =
  if d.parse_rev = d.rev then
    Some
      {
        parsed_rev = d.parse_rev;
        parsed_ast = d.ast;
        parsed_syntax = d.syntax;
        parsed_diags = d.parse_diags;
      }
  else None

let current_ast (d : t) : Ast.program option =
  match current_parse d with
  | Some { parsed_ast = Some ast; _ } -> Some ast
  | _ -> None

let has_current_parse (d : t) : bool =
  d.parse_rev = d.rev && Option.is_some d.ast

let has_current_syntax (d : t) : bool =
  d.parse_rev = d.rev && Option.is_some d.syntax

let drop_ast (d : t) : t =
  if d.ast = None || d.parse_rev <> d.rev || d.parse_diags <> [] then d
  else { d with ast = None; syntax = Option.map Syntax_cache.drop_ast d.syntax }

let compare_pos (a : T.Position.t) (b : T.Position.t) : int =
  if a.line <> b.line then compare a.line b.line
  else compare a.character b.character

let normalize_range (r : T.Range.t) : T.Range.t =
  if compare_pos r.start r.end_ <= 0 then r
  else { T.Range.start = r.end_; end_ = r.start }

let ast_dump ?(max_depth = 64) ?(max_nodes = 4000) (d : t) =
  let d = ensure_parsed d in
  match current_ast d with
  | None -> None
  | Some ast ->
      let opts =
        { Ast.Debug.show_locs = true; max_depth = Some max_depth; max_nodes = Some max_nodes }
      in
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

let apply_changes ?lsp_version (doc : t)
    (changes : T.TextDocumentContentChangeEvent.t list) : t =
  let text, index, changed =
    List.fold_left
      (fun (t, idx, changed) ch ->
        let t', idx' = apply_content_change ~text:t ~index:idx ch in
        let changed' = changed || not (t' == t && idx' == idx) in
        (t', idx', changed'))
      (doc.text, doc.index, false)
      changes
  in
  let doc =
    match lsp_version with
    | None -> doc
    | Some _ -> { doc with lsp_version }
  in
  if not changed then doc
  else
    {
      doc with
      rev = doc.rev + 1;
      text;
      index;
      imports = [];
      compool_def = None;
      defines = [];
      pre_diags = [];
      import_diags = [];
      parse_diags = [];
      ast = None;
      syntax = None;
      diags = [];
    }

let apply_changes_no_reparse ?lsp_version
    ~(changes : T.TextDocumentContentChangeEvent.t list) (doc : t) : t =
  if changes = [] then
    match lsp_version with None -> doc | Some _ -> { doc with lsp_version }
  else apply_changes ?lsp_version doc changes

let summarize_changes (doc : t)
    (changes : T.TextDocumentContentChangeEvent.t list) :
    Syntax_cache.edit_summary option =
  if changes = [] then None
  else
    let text_ref = ref doc.text in
    let idx_ref = ref doc.index in
    let full_sync = ref false in
    let start_min = ref max_int in
    let old_end_max = ref 0 in
    let new_end_max = ref 0 in
    let inserted_chars = ref 0 in
    List.iter
      (fun (ch : T.TextDocumentContentChangeEvent.t) ->
        inserted_chars := !inserted_chars + String.length ch.text;
        (match ch.range with
        | None ->
            full_sync := true;
            start_min := 0;
            old_end_max := String.length !text_ref;
            new_end_max := String.length ch.text
        | Some r -> (
            match (offset_of_pos !idx_ref r.start, offset_of_pos !idx_ref r.end_) with
            | Some a, Some b ->
                let a, b = if a <= b then (a, b) else (b, a) in
                start_min := min !start_min a;
                old_end_max := max !old_end_max b;
                new_end_max := max !new_end_max (a + String.length ch.text)
            | _ -> full_sync := true));
        let text', idx' =
          apply_content_change ~text:!text_ref ~index:!idx_ref ch
        in
        text_ref := text';
        idx_ref := idx')
      changes;
    Some
      {
        Syntax_cache.full_sync = !full_sync;
        start_off =
          (if !start_min = max_int then 0 else max 0 !start_min);
        old_end_off = max 0 !old_end_max;
        new_end_off = max 0 !new_end_max;
        inserted_chars = !inserted_chars;
        change_count = List.length changes;
      }

let apply_changes_and_reparse_with_profile ?lsp_version
    ~(profile : Parser.profile)
    ~(changes : T.TextDocumentContentChangeEvent.t list) (doc : t) : t =
  if changes = [] then
    match lsp_version with None -> doc | Some _ -> { doc with lsp_version }
  else
    let previous_syntax = doc.syntax in
    let edit_summary = summarize_changes doc changes in
    let doc' = apply_changes ?lsp_version doc changes in
    reparse_with_profile ?previous_syntax ?edit_summary ~profile doc'

let apply_changes_and_reparse ?lsp_version
    ~(changes : T.TextDocumentContentChangeEvent.t list) (doc : t) : t =
  apply_changes_and_reparse_with_profile ?lsp_version
    ~profile:Parser.Interactive ~changes doc

let with_import_diags (import_diags : T.Diagnostic.t list) (d : t) : t =
  recompute_diags { d with import_diags }

let with_parse_diags (parse_diags : T.Diagnostic.t list) (d : t) : t =
  recompute_diags { d with parse_diags; ast = None; syntax = None }

let with_parse_skipped (parse_diags : T.Diagnostic.t list) (d : t) : t =
  recompute_diags
    {
      d with
      parse_rev = d.rev;
      imports = [];
      compool_def = None;
      defines = [];
      pre_diags = [];
      import_diags = [];
      parse_diags;
      ast = None;
      syntax = None;
    }
