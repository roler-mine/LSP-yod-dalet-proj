(* Module overview: Central cross-file semantic index for workspace-wide lookup. *)

module T = Lsp.Types
module Metadata = Workspace_symbol_metadata

type file_id = int
type module_id = int
type scope_id = int
type symbol_id = int
type reference_id = int
type name_id = int
type type_id = int
type node_id = int
type diag_id = int
type signature_id = int

let id_of_stable_string (s : string) : int =
  let digest = Digest.to_hex (Digest.string s) in
  let acc = ref 0 in
  String.iter
    (fun c -> acc := ((!acc * 131) + Char.code c) land max_int)
    digest;
  !acc

module PackedRange = struct
  type t = { start_offset : int; end_offset : int }

  let make ~(start_offset : int) ~(end_offset : int) : t =
    if start_offset <= end_offset then { start_offset; end_offset }
    else { start_offset = end_offset; end_offset = start_offset }

  let contains (r : t) (offset : int) : bool =
    r.start_offset <= offset && offset <= r.end_offset

  let length (r : t) : int = max 0 (r.end_offset - r.start_offset)
end

module NameTable = struct
  type t = {
    by_key : (string, name_id) Hashtbl.t;
    text_by_id : (name_id, string) Hashtbl.t;
    mutable next_id : name_id;
  }

  let create () = { by_key = Hashtbl.create 1024; text_by_id = Hashtbl.create 1024; next_id = 1 }

  let clear t =
    Hashtbl.clear t.by_key;
    Hashtbl.clear t.text_by_id;
    t.next_id <- 1

  let normalize_identifier (s : string) : string =
    let normalized = String.uppercase_ascii (String.trim s) in
    let len = String.length normalized in
    if len > 31 then String.sub normalized 0 31 else normalized

  let intern_with_key t ~(key : string) ~(display : string) : name_id =
    match Hashtbl.find_opt t.by_key key with
    | Some id -> id
    | None ->
        let id = t.next_id in
        t.next_id <- t.next_id + 1;
        Hashtbl.add t.by_key key id;
        Hashtbl.add t.text_by_id id display;
        id

  let intern_identifier t raw =
    let normalized = normalize_identifier raw in
    intern_with_key t ~key:("id:" ^ normalized) ~display:normalized

  let intern_raw t raw = intern_with_key t ~key:("raw:" ^ raw) ~display:raw

  let find_identifier t raw =
    Hashtbl.find_opt t.by_key ("id:" ^ normalize_identifier raw)

  let text t id = Hashtbl.find_opt t.text_by_id id
  let count t = Hashtbl.length t.text_by_id
end

module LineIndex = struct
  type t = Text_index.t

  let of_string = Text_index.of_string

  let offset_of_position (idx : t) (pos : T.Position.t) : int option =
    Text_index.offset_of_line_col idx ~line:pos.line ~col:pos.character

  let position_of_offset (idx : t) (offset : int) : T.Position.t =
    let line, character = Text_index.line_col_of_offset idx offset in
    { T.Position.line; character }

  let loc_of_range ~(file : string option) (idx : t) (range : PackedRange.t) :
      Ast.Loc.t =
    let start_line0, start_col = Text_index.line_col_of_offset idx range.start_offset in
    let end_line0, end_col = Text_index.line_col_of_offset idx range.end_offset in
    Ast.Loc.make ~file
      ~start_pos:{ Ast.Loc.line = start_line0 + 1; col = start_col; offset = range.start_offset }
      ~end_pos:{ Ast.Loc.line = end_line0 + 1; col = end_col; offset = range.end_offset }

  let lsp_range_of_range (idx : t) (range : PackedRange.t) : T.Range.t =
    { T.Range.start = position_of_offset idx range.start_offset; end_ = position_of_offset idx range.end_offset }
end

module TokenIndex = struct
  type token_kind =
    | IdentifierToken
    | KeywordToken
    | LiteralToken
    | OperatorToken
    | PunctuationToken
    | UnknownToken

  type token = {
    node_id : node_id;
    kind : token_kind;
    range : PackedRange.t;
    name_id : name_id option;
    raw_text : string option;
  }

  type t = { tokens : token array }

  let empty () = { tokens = [||] }

  let token_kind_of_parser_token = function
    | Parser.ID _ | Parser.FIXED_A _ -> IdentifierToken
    | Parser.STRINGLIT _ | Parser.INTLIT _ | Parser.FLOATLIT _ | Parser.BITLIT _ -> LiteralToken
    | Parser.PLUS | Parser.MINUS | Parser.STAR | Parser.SLASH | Parser.POW | Parser.MOD
    | Parser.EQ | Parser.NE | Parser.LT | Parser.LE | Parser.GT | Parser.GE | Parser.AND
    | Parser.OR | Parser.XOR | Parser.NOT | Parser.EQV | Parser.CONV_L | Parser.CONV_R
    | Parser.AT ->
        OperatorToken
    | Parser.LPAREN | Parser.RPAREN | Parser.COMMA | Parser.SEMI | Parser.COLON | Parser.DOT
    | Parser.BANG ->
        PunctuationToken
    | Parser.EOF -> UnknownToken
    | _ -> KeywordToken

  let identifier_name = function
    | Parser.ID raw | Parser.FIXED_A raw -> Some raw
    | _ -> None

  let raw_text_of_token = function
    | Parser.ID raw | Parser.FIXED_A raw | Parser.STRINGLIT raw | Parser.INTLIT raw
    | Parser.FLOATLIT raw ->
        Some raw
    | Parser.BITLIT (_, _, raw) -> Some raw
    | _ -> None

  let of_lex_tokens (names : NameTable.t) (spans : Preprocess.lex_tok array) : t =
    let tokens =
      spans
      |> Array.to_list
      |> List.filter_map (fun (span : Preprocess.lex_tok) ->
             match span.Parser.tok with
             | Parser.EOF -> None
             | tok ->
                 let range =
                   PackedRange.make ~start_offset:span.start_off
                     ~end_offset:span.end_off
                 in
                 let name_id =
                   match identifier_name tok with
                   | None -> None
                   | Some raw -> Some (NameTable.intern_identifier names raw)
                 in
                 let node_id =
                   id_of_stable_string
                     (Printf.sprintf "token|%d|%d|%s" span.start_off
                        span.end_off
                        (match raw_text_of_token tok with Some s -> s | None -> ""))
                 in
                 Some
                   {
                     node_id;
                     kind = token_kind_of_parser_token tok;
                     range;
                     name_id;
                     raw_text = raw_text_of_token tok;
                   })
      |> List.sort (fun a b ->
             match compare a.range.start_offset b.range.start_offset with
             | 0 -> compare a.range.end_offset b.range.end_offset
             | n -> n)
      |> Array.of_list
    in
    { tokens }

  let find_at_offset (t : t) (offset : int) : token option =
    let best = ref None in
    let lo = ref 0 in
    let hi = ref (Array.length t.tokens - 1) in
    while !lo <= !hi do
      let mid = (!lo + !hi) / 2 in
      let tok = t.tokens.(mid) in
      if offset < tok.range.start_offset then hi := mid - 1
      else if offset > tok.range.end_offset then lo := mid + 1
      else (
        best := Some tok;
        hi := mid - 1)
    done;
    !best

  let all t = Array.to_list t.tokens
  let count t = Array.length t.tokens
end

type module_kind =
  | MainProgram
  | ProcedureModule
  | CompoolModule
  | UnknownModule

type symbol_kind =
  | Item
  | ConstantItem
  | Table
  | Block
  | Type
  | Proc
  | Function
  | FormalParam
  | Label
  | Define
  | StatusValue
  | Compool
  | ExternalDef
  | ExternalRef
  | Builtin
  | AsmExternal
  | UnknownSymbol

type scope_kind =
  | SystemScope
  | CompoolScope
  | ModuleScope
  | ModuleBodyScope
  | SubroutineScope
  | BlockScope
  | TableScope
  | UnknownScope

type export_kind =
  | ExportDef
  | ExportCompoolMember
  | ExportProcedure
  | ExportFunction
  | ExportType
  | ExportAsmStub
  | ExportBuiltin

type import_kind =
  | ImportCompool
  | ImportRefSymbol
  | ImportCopyFile
  | ImportAsmExternal
  | ImportImplicitSystem

type reference_kind =
  | ReadRef
  | WriteRef
  | CallRef
  | TypeRef
  | ImportRef
  | DefRef
  | LabelRef
  | UnknownRef

type jovial_type =
  | IntegerType of { signed : bool; bits : int option }
  | FloatType of { precision : int option }
  | FixedType of { scale : int option; fraction : int option }
  | BitType of { size : int option }
  | CharType of { size : int option }
  | StatusType of name_id list
  | PointerType of type_id option
  | TableType of { dimensions : node_id list; entry_type : type_id option }
  | BlockType of (name_id * type_id option) list
  | SignatureType of { params : type_id list; return_type : type_id option }
  | DisplayType of string
  | UnknownType
  | ErrorType

type external_info = {
  external_name_id : name_id;
  is_def : bool;
  origin : string option;
}

type storage_info = { storage_label : string option }

type file_record = {
  file_id : file_id;
  uri : T.DocumentUri.t;
  uri_key : string;
  path_key : string option;
  lsp_version : int option;
  rev : int;
  parse_rev : int;
  content_hash : string;
  line_index : LineIndex.t;
  token_index : TokenIndex.t;
  ast_available : bool;
  parse_error_count : int;
  detected_module_header : name_id option;
}

type module_record = {
  module_id : module_id;
  file_id : file_id;
  module_name_id : name_id;
  module_kind : module_kind;
  root_scope : scope_id;
  mutable declared_symbols : symbol_id list;
  mutable def_symbols : symbol_id list;
  mutable ref_symbols : symbol_id list;
  mutable imported_compools : name_id list;
}

type scope_record = {
  scope_id : scope_id;
  scope_kind : scope_kind;
  file_id : file_id option;
  module_id : module_id option;
  parent_scope : scope_id option;
  children_scopes : scope_id list;
  range : PackedRange.t option;
  local_symbols : symbol_id list;
}

type symbol_record = {
  symbol_id : symbol_id;
  stable_key : string;
  name_id : name_id;
  spelling : string;
  kind : symbol_kind;
  file_id : file_id;
  module_id : module_id option;
  scope_id : scope_id;
  declaration_range : PackedRange.t;
  name_range : PackedRange.t;
  declaration_text : string option;
  type_id : type_id option;
  signature_id : signature_id option;
  storage_info : storage_info option;
  external_info : external_info option;
  metadata : Metadata.jovial_symbol_metadata;
}

type type_record = {
  type_id : type_id;
  descriptor : jovial_type;
  display : string;
}

type formula_summary = {
  node_id : node_id;
  file_id : file_id;
  module_id : module_id;
  scope_id : scope_id;
  range : PackedRange.t;
  inferred_type_id : type_id;
  constant_value : string option;
  dependency_symbols : symbol_id list;
  formula_errors : diag_id list;
}

type export_entry = {
  symbol_id : symbol_id;
  name_id : name_id;
  owner_module : module_id;
  owner_file : file_id;
  export_kind : export_kind;
  type_id : type_id option;
  signature_id : signature_id option;
}

type import_edge = {
  importing_module : module_id;
  imported_name_id : name_id;
  import_kind : import_kind;
  source_range : PackedRange.t;
}

type external_binding = {
  external_name_id : name_id;
  matching_def : symbol_id option;
  ref_symbols : symbol_id list;
  duplicate_defs : symbol_id list;
  unresolved_refs : symbol_id list;
}

type visible_environment = {
  module_id : module_id;
  local_symbols : symbol_id list;
  imported_symbols : symbol_id list;
  system_symbols : symbol_id list;
  by_name : (name_id, symbol_id list) Hashtbl.t;
}

type reference_record = {
  reference_id : reference_id;
  file_id : file_id;
  module_id : module_id;
  scope_id : scope_id;
  name_id : name_id;
  range : PackedRange.t;
  reference_kind : reference_kind;
  resolved_symbol_id : symbol_id option;
}

type diagnostic_record = {
  diag_id : diag_id;
  file_id : file_id;
  range : PackedRange.t;
  severity : T.DiagnosticSeverity.t option;
  source : string option;
  message : string;
  hint : string option;
}

type persistent_cache_metadata = {
  parser_version : string;
  analyzer_version : string;
  workspace_root : string option;
  settings_hash : string;
}

type dependency_graph = {
  module_imports : (module_id, module_id list) Hashtbl.t;
  module_imported_by : (module_id, module_id list) Hashtbl.t;
  symbol_used_by_modules : (symbol_id, module_id list) Hashtbl.t;
  symbol_references : (symbol_id, reference_id list) Hashtbl.t;
  file_declares_symbols : (file_id, symbol_id list) Hashtbl.t;
  file_uses_symbols : (file_id, symbol_id list) Hashtbl.t;
}

type semantic_signatures = {
  exports_hash : string;
  imports_hash : string;
  local_decls_hash : string;
  types_hash : string;
}

type symbol_hit = {
  symbol : symbol_record;
  reference : reference_record option;
  file : file_record;
  loc : Ast.Loc.t;
}

type t = {
  name_table : NameTable.t;
  files_by_id : (file_id, file_record) Hashtbl.t;
  file_id_by_uri : (string, file_id) Hashtbl.t;
  file_id_by_path : (string, file_id) Hashtbl.t;
  modules_by_id : (module_id, module_record) Hashtbl.t;
  module_id_by_file : (file_id, module_id) Hashtbl.t;
  module_ids_by_name : (name_id, module_id list) Hashtbl.t;
  compool_module_ids_by_name : (name_id, module_id list) Hashtbl.t;
  scopes_by_id : (scope_id, scope_record) Hashtbl.t;
  symbols_by_id : (symbol_id, symbol_record) Hashtbl.t;
  symbol_ids_by_stable_key : (string, symbol_id) Hashtbl.t;
  symbol_ids_by_name : (name_id, symbol_id list) Hashtbl.t;
  symbol_ids_by_file : (file_id, symbol_id list) Hashtbl.t;
  symbol_ids_by_module : (module_id, symbol_id list) Hashtbl.t;
  symbol_ids_by_kind : (symbol_kind, symbol_id list) Hashtbl.t;
  types_by_id : (type_id, type_record) Hashtbl.t;
  type_id_by_display : (string, type_id) Hashtbl.t;
  formulas_by_node : (node_id, formula_summary) Hashtbl.t;
  exports : export_entry list ref;
  exports_by_name : (name_id, export_entry list) Hashtbl.t;
  exports_by_owner_module : (module_id, export_entry list) Hashtbl.t;
  exports_by_owner_file : (file_id, export_entry list) Hashtbl.t;
  imports : import_edge list ref;
  external_bindings : (name_id, external_binding) Hashtbl.t;
  visibility_by_module : (module_id, visible_environment) Hashtbl.t;
  references_by_id : (reference_id, reference_record) Hashtbl.t;
  reference_ids_by_file : (file_id, reference_id list) Hashtbl.t;
  reference_ids_by_module : (module_id, reference_id list) Hashtbl.t;
  reference_ids_by_symbol : (symbol_id, reference_id list) Hashtbl.t;
  resolved_symbol_by_reference : (reference_id, symbol_id) Hashtbl.t;
  dependencies : dependency_graph;
  diagnostics_by_id : (diag_id, diagnostic_record) Hashtbl.t;
  diagnostic_ids_by_file : (file_id, diag_id list) Hashtbl.t;
  semantic_signatures_by_file : (file_id, semantic_signatures) Hashtbl.t;
  mutable persistent_cache_metadata : persistent_cache_metadata option;
}

let create_dependency_graph () =
  {
    module_imports = Hashtbl.create 512;
    module_imported_by = Hashtbl.create 512;
    symbol_used_by_modules = Hashtbl.create 2048;
    symbol_references = Hashtbl.create 2048;
    file_declares_symbols = Hashtbl.create 512;
    file_uses_symbols = Hashtbl.create 512;
  }

let create () =
  {
    name_table = NameTable.create ();
    files_by_id = Hashtbl.create 512;
    file_id_by_uri = Hashtbl.create 512;
    file_id_by_path = Hashtbl.create 512;
    modules_by_id = Hashtbl.create 512;
    module_id_by_file = Hashtbl.create 512;
    module_ids_by_name = Hashtbl.create 512;
    compool_module_ids_by_name = Hashtbl.create 256;
    scopes_by_id = Hashtbl.create 1024;
    symbols_by_id = Hashtbl.create 4096;
    symbol_ids_by_stable_key = Hashtbl.create 4096;
    symbol_ids_by_name = Hashtbl.create 2048;
    symbol_ids_by_file = Hashtbl.create 512;
    symbol_ids_by_module = Hashtbl.create 512;
    symbol_ids_by_kind = Hashtbl.create 64;
    types_by_id = Hashtbl.create 1024;
    type_id_by_display = Hashtbl.create 1024;
    formulas_by_node = Hashtbl.create 1024;
    exports = ref [];
    exports_by_name = Hashtbl.create 1024;
    exports_by_owner_module = Hashtbl.create 512;
    exports_by_owner_file = Hashtbl.create 512;
    imports = ref [];
    external_bindings = Hashtbl.create 512;
    visibility_by_module = Hashtbl.create 512;
    references_by_id = Hashtbl.create 8192;
    reference_ids_by_file = Hashtbl.create 512;
    reference_ids_by_module = Hashtbl.create 512;
    reference_ids_by_symbol = Hashtbl.create 4096;
    resolved_symbol_by_reference = Hashtbl.create 8192;
    dependencies = create_dependency_graph ();
    diagnostics_by_id = Hashtbl.create 2048;
    diagnostic_ids_by_file = Hashtbl.create 512;
    semantic_signatures_by_file = Hashtbl.create 512;
    persistent_cache_metadata = None;
  }

let clear_dependency_graph deps =
  Hashtbl.clear deps.module_imports;
  Hashtbl.clear deps.module_imported_by;
  Hashtbl.clear deps.symbol_used_by_modules;
  Hashtbl.clear deps.symbol_references;
  Hashtbl.clear deps.file_declares_symbols;
  Hashtbl.clear deps.file_uses_symbols

let reset t =
  NameTable.clear t.name_table;
  Hashtbl.clear t.files_by_id;
  Hashtbl.clear t.file_id_by_uri;
  Hashtbl.clear t.file_id_by_path;
  Hashtbl.clear t.modules_by_id;
  Hashtbl.clear t.module_id_by_file;
  Hashtbl.clear t.module_ids_by_name;
  Hashtbl.clear t.compool_module_ids_by_name;
  Hashtbl.clear t.scopes_by_id;
  Hashtbl.clear t.symbols_by_id;
  Hashtbl.clear t.symbol_ids_by_stable_key;
  Hashtbl.clear t.symbol_ids_by_name;
  Hashtbl.clear t.symbol_ids_by_file;
  Hashtbl.clear t.symbol_ids_by_module;
  Hashtbl.clear t.symbol_ids_by_kind;
  Hashtbl.clear t.types_by_id;
  Hashtbl.clear t.type_id_by_display;
  Hashtbl.clear t.formulas_by_node;
  Hashtbl.clear t.exports_by_name;
  Hashtbl.clear t.exports_by_owner_module;
  Hashtbl.clear t.exports_by_owner_file;
  Hashtbl.clear t.external_bindings;
  Hashtbl.clear t.visibility_by_module;
  Hashtbl.clear t.references_by_id;
  Hashtbl.clear t.reference_ids_by_file;
  Hashtbl.clear t.reference_ids_by_module;
  Hashtbl.clear t.reference_ids_by_symbol;
  Hashtbl.clear t.resolved_symbol_by_reference;
  Hashtbl.clear t.diagnostics_by_id;
  Hashtbl.clear t.diagnostic_ids_by_file;
  Hashtbl.clear t.semantic_signatures_by_file;
  t.exports := [];
  t.imports := [];
  clear_dependency_graph t.dependencies;
  t.persistent_cache_metadata <- None

let name_table t = t.name_table
let uri_key (uri : T.DocumentUri.t) = Uri_path.docuri_to_string uri

let path_key_of_doc (doc : Document.t) =
  Option.map Uri_path.normalize_path_key doc.Document.file

let add_to_list_tbl tbl key value =
  let prev = Option.value (Hashtbl.find_opt tbl key) ~default:[] in
  if List.mem value prev then ()
  else Hashtbl.replace tbl key (value :: prev)

let sorted_unique_ints xs = List.sort_uniq Int.compare xs

let loc_range (loc : Ast.Loc.t) =
  PackedRange.make ~start_offset:loc.Ast.Loc.start_pos.offset
    ~end_offset:loc.Ast.Loc.end_pos.offset

let loc_equal (a : Ast.Loc.t) (b : Ast.Loc.t) =
  a.Ast.Loc.start_pos.offset = b.Ast.Loc.start_pos.offset
  && a.Ast.Loc.end_pos.offset = b.Ast.Loc.end_pos.offset

let source_line_at_offset (text : string) (offset : int) : string option =
  if offset < 0 || offset >= String.length text then None
  else
    let start = ref offset in
    while !start > 0 && text.[!start - 1] <> '\n' do
      decr start
    done;
    let finish = ref offset in
    while !finish < String.length text && text.[!finish] <> '\n' do
      incr finish
    done;
    let line = String.sub text !start (!finish - !start) |> String.trim in
    if line = "" then None else Some line

let name_range_for_declaration (token_index : TokenIndex.t) (name_id : name_id)
    (decl_range : PackedRange.t) : PackedRange.t =
  let candidates =
    TokenIndex.all token_index
    |> List.filter (fun (tok : TokenIndex.token) -> tok.name_id = Some name_id)
  in
  candidates
  |> List.find_opt (fun (tok : TokenIndex.token) ->
         decl_range.start_offset <= tok.range.start_offset
         && tok.range.end_offset <= decl_range.end_offset)
  |> (function
       | Some _ as hit -> hit
       | None -> List.find_opt (fun _ -> true) candidates)
  |> Option.map (fun (tok : TokenIndex.token) -> tok.range)
  |> Option.value ~default:decl_range

let symbol_kind_of_metadata (m : Metadata.jovial_symbol_metadata) fallback =
  let base =
    match m.Metadata.jovial_kind with
    | Metadata.JovialProgram -> Compool
    | Metadata.JovialModule -> Compool
    | Metadata.JovialCompool | Metadata.JovialCompoolImport -> Compool
    | Metadata.JovialItem -> Item
    | Metadata.JovialTable -> Table
    | Metadata.JovialBlock -> Block
    | Metadata.JovialOverlay -> Item
    | Metadata.JovialType | Metadata.JovialBuiltinType -> Type
    | Metadata.JovialProcedure -> Proc
    | Metadata.JovialFunction -> Function
    | Metadata.JovialParameter -> FormalParam
    | Metadata.JovialField -> Item
    | Metadata.JovialLabel -> Label
    | Metadata.JovialDefine -> Define
    | Metadata.JovialConstantItem -> ConstantItem
    | Metadata.JovialConstantTable -> Table
    | Metadata.JovialStatusConstant -> StatusValue
    | Metadata.JovialUnknownSymbol -> fallback
  in
  match m.Metadata.external_kind with
  | Metadata.ExternalDef -> ExternalDef
  | Metadata.ExternalRef -> ExternalRef
  | Metadata.ExternalSystem -> Builtin
  | Metadata.ExternalLocal -> base

let symbol_kind_of_lsp_int kind =
  if kind = 5 then Type
  else if kind = 12 then Proc
  else if kind = 14 then ConstantItem
  else if kind = 2 then Compool
  else Item

let export_kind_of_symbol (sym : symbol_record) (modul : module_record) :
    export_kind option =
  match sym.metadata.Metadata.external_kind with
  | Metadata.ExternalSystem -> Some ExportBuiltin
  | Metadata.ExternalDef -> (
      match sym.kind with
      | Proc -> Some ExportProcedure
      | Function -> Some ExportFunction
      | Type -> Some ExportType
      | _ -> Some ExportDef)
  | Metadata.ExternalLocal ->
      if modul.module_kind = CompoolModule then
        match sym.kind with
        | Type -> Some ExportType
        | Proc -> Some ExportProcedure
        | Function -> Some ExportFunction
        | _ -> Some ExportCompoolMember
      else if sym.metadata.Metadata.is_exported then Some ExportDef
      else None
  | Metadata.ExternalRef -> None

let diagnostic_message_text (diag : T.Diagnostic.t) : string =
  match diag.T.Diagnostic.message with
  | `String s -> s
  | `MarkupContent mc -> mc.value

let diagnostic_range ~(idx : LineIndex.t) (diag : T.Diagnostic.t) =
  let start_offset =
    Option.value
      (LineIndex.offset_of_position idx diag.T.Diagnostic.range.start)
      ~default:0
  in
  let end_offset =
    Option.value
      (LineIndex.offset_of_position idx diag.T.Diagnostic.range.end_)
      ~default:start_offset
  in
  PackedRange.make ~start_offset ~end_offset

let diagnostics_from_doc ~(file_id : file_id) ~(idx : LineIndex.t) (doc : Document.t)
    : diagnostic_record list =
  Document.diagnostics doc
  |> List.mapi (fun ordinal diag ->
         {
           diag_id =
             id_of_stable_string
               (Printf.sprintf "diag|%s|%d|%s" (uri_key doc.Document.uri)
                  ordinal (diagnostic_message_text diag));
           file_id;
           range = diagnostic_range ~idx diag;
           severity = diag.T.Diagnostic.severity;
           source = diag.T.Diagnostic.source;
           message = diagnostic_message_text diag;
           hint = None;
         })

let classify_reference ~(is_decl : bool) (d : Semantic_store.Snapshot.nav_def option)
    : reference_kind =
  if is_decl then
    match d with
    | Some d when Metadata.is_external_ref d.metadata -> ImportRef
    | _ -> DefRef
  else
    match d with
    | Some d when d.kind = 12 -> CallRef
    | Some d when d.kind = 5 -> TypeRef
    | _ -> ReadRef

let type_id_for_metadata t (m : Metadata.jovial_symbol_metadata) : type_id option
    =
  match m.Metadata.type_info with
  | None -> None
  | Some info ->
      let display =
        match info.Metadata.resolved_display with
        | Some resolved when String.trim resolved <> "" ->
            info.Metadata.display ^ " => " ^ resolved
        | _ -> info.Metadata.display
      in
      let id =
        match Hashtbl.find_opt t.type_id_by_display display with
        | Some id -> id
        | None ->
            let id = id_of_stable_string ("type|" ^ display) in
            Hashtbl.replace t.type_id_by_display display id;
            Hashtbl.replace t.types_by_id id
              { type_id = id; descriptor = DisplayType display; display };
            id
      in
      Some id

let storage_info_of_metadata (m : Metadata.jovial_symbol_metadata) =
  Option.map
    (fun storage -> { storage_label = Some (Metadata.storage_label storage) })
    m.Metadata.storage

let external_info_of_metadata names name_id (m : Metadata.jovial_symbol_metadata)
    =
  match m.Metadata.external_kind with
  | Metadata.ExternalDef -> Some { external_name_id = name_id; is_def = true; origin = None }
  | Metadata.ExternalRef -> Some { external_name_id = name_id; is_def = false; origin = None }
  | Metadata.ExternalSystem ->
      Some { external_name_id = name_id; is_def = true; origin = Some "system" }
  | Metadata.ExternalLocal ->
      ignore names;
      None

let tokens_for_doc (doc : Document.t) : Preprocess.lex_tok array =
  match Document.current_parse doc with
  | Some { Document.parsed_syntax = Some syntax; _ } -> (
      match syntax.Syntax_cache.raw_tokens with
      | Some toks -> toks
      | None ->
          Preprocess.lex_all_tokens_with_lexemes ~file:doc.Document.file
            ~text:doc.Document.text)
  | _ ->
      Preprocess.lex_all_tokens_with_lexemes ~file:doc.Document.file
        ~text:doc.Document.text

let basename_stem path =
  let base = Filename.basename path in
  try Filename.chop_extension base with Invalid_argument _ -> base

let module_reference_name (s : string) : string =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 then
    match (s.[0], s.[len - 1]) with
    | '\'', '\'' | '"', '"' -> String.sub s 1 (len - 2)
    | _ -> s
  else s

let module_name_for_doc (doc : Document.t) (snap : Semantic_store.Snapshot.t) :
    string =
  match doc.Document.compool_def with
  | Some name when String.trim name <> "" -> module_reference_name name
  | _ -> (
      match
        snap.Semantic_store.Snapshot.nav_defs
        |> List.find_map (fun (_, d) ->
               if d.Semantic_store.Snapshot.kind = 2 then Some d.name else None)
      with
      | Some name -> module_reference_name name
      | None -> (
          match doc.Document.file with
          | Some path -> basename_stem path
          | None -> uri_key doc.Document.uri))

let module_kind_for_doc (doc : Document.t) (snap : Semantic_store.Snapshot.t) =
  match doc.Document.compool_def with
  | Some _ -> CompoolModule
  | None ->
      if
        List.exists
          (fun (_, d) -> d.Semantic_store.Snapshot.kind = 2)
          snap.Semantic_store.Snapshot.nav_defs
      then CompoolModule
      else
      if
        List.exists
          (fun (_, d) -> d.Semantic_store.Snapshot.kind = 12)
          snap.Semantic_store.Snapshot.nav_defs
      then ProcedureModule
      else UnknownModule

let remove_file_id t (file_id : file_id) : unit =
  (match Hashtbl.find_opt t.files_by_id file_id with
  | None -> ()
  | Some file ->
      Hashtbl.remove t.file_id_by_uri file.uri_key;
      Option.iter (fun path_key -> Hashtbl.remove t.file_id_by_path path_key) file.path_key);
  Hashtbl.remove t.files_by_id file_id;
  (match Hashtbl.find_opt t.module_id_by_file file_id with
  | None -> ()
  | Some module_id ->
      Hashtbl.remove t.modules_by_id module_id;
      Hashtbl.remove t.module_id_by_file file_id);
  let remove_symbols =
    Option.value (Hashtbl.find_opt t.symbol_ids_by_file file_id) ~default:[]
  in
  List.iter
    (fun sym_id ->
      Hashtbl.remove t.symbols_by_id sym_id;
      Hashtbl.remove t.resolved_symbol_by_reference sym_id)
    remove_symbols;
  Hashtbl.remove t.symbol_ids_by_file file_id;
  let remove_refs =
    Option.value (Hashtbl.find_opt t.reference_ids_by_file file_id) ~default:[]
  in
  List.iter
    (fun ref_id ->
      Hashtbl.remove t.references_by_id ref_id;
      Hashtbl.remove t.resolved_symbol_by_reference ref_id)
    remove_refs;
  Hashtbl.remove t.reference_ids_by_file file_id;
  let remove_diags =
    Option.value (Hashtbl.find_opt t.diagnostic_ids_by_file file_id) ~default:[]
  in
  List.iter (Hashtbl.remove t.diagnostics_by_id) remove_diags;
  Hashtbl.remove t.diagnostic_ids_by_file file_id;
  Hashtbl.remove t.semantic_signatures_by_file file_id

let rebuild_secondary_indexes t =
  Hashtbl.clear t.module_ids_by_name;
  Hashtbl.clear t.compool_module_ids_by_name;
  Hashtbl.clear t.symbol_ids_by_stable_key;
  Hashtbl.clear t.symbol_ids_by_name;
  Hashtbl.clear t.symbol_ids_by_file;
  Hashtbl.clear t.symbol_ids_by_module;
  Hashtbl.clear t.symbol_ids_by_kind;
  Hashtbl.clear t.exports_by_name;
  Hashtbl.clear t.exports_by_owner_module;
  Hashtbl.clear t.exports_by_owner_file;
  Hashtbl.clear t.external_bindings;
  Hashtbl.clear t.visibility_by_module;
  Hashtbl.clear t.reference_ids_by_module;
  Hashtbl.clear t.reference_ids_by_symbol;
  Hashtbl.clear t.resolved_symbol_by_reference;
  t.exports := [];
  t.imports := [];
  clear_dependency_graph t.dependencies;
  Hashtbl.iter
    (fun module_id (m : module_record) ->
      add_to_list_tbl t.module_ids_by_name m.module_name_id module_id;
      if m.module_kind = CompoolModule then
        add_to_list_tbl t.compool_module_ids_by_name m.module_name_id module_id;
      m.declared_symbols <- [];
      m.def_symbols <- [];
      m.ref_symbols <- [])
    t.modules_by_id;
  Hashtbl.iter
    (fun symbol_id (sym : symbol_record) ->
      Hashtbl.replace t.symbol_ids_by_stable_key sym.stable_key symbol_id;
      add_to_list_tbl t.symbol_ids_by_name sym.name_id symbol_id;
      add_to_list_tbl t.symbol_ids_by_file sym.file_id symbol_id;
      add_to_list_tbl t.symbol_ids_by_kind sym.kind symbol_id;
      Option.iter
        (fun module_id ->
          add_to_list_tbl t.symbol_ids_by_module module_id symbol_id;
          match Hashtbl.find_opt t.modules_by_id module_id with
          | None -> ()
          | Some m ->
              m.declared_symbols <- sorted_unique_ints (symbol_id :: m.declared_symbols);
              (match sym.metadata.Metadata.external_kind with
              | Metadata.ExternalDef ->
                  m.def_symbols <- sorted_unique_ints (symbol_id :: m.def_symbols)
              | Metadata.ExternalRef ->
                  m.ref_symbols <- sorted_unique_ints (symbol_id :: m.ref_symbols)
              | Metadata.ExternalLocal | Metadata.ExternalSystem -> ()))
        sym.module_id;
      Option.iter
        (fun module_id ->
          match Hashtbl.find_opt t.modules_by_id module_id with
          | None -> ()
          | Some modul -> (
              match export_kind_of_symbol sym modul with
              | None -> ()
              | Some export_kind ->
                  let entry =
                    {
                      symbol_id;
                      name_id = sym.name_id;
                      owner_module = module_id;
                      owner_file = sym.file_id;
                      export_kind;
                      type_id = sym.type_id;
                      signature_id = sym.signature_id;
                    }
                  in
                  t.exports := entry :: !(t.exports);
                  add_to_list_tbl t.exports_by_name entry.name_id entry;
                  add_to_list_tbl t.exports_by_owner_module module_id entry;
                  add_to_list_tbl t.exports_by_owner_file sym.file_id entry))
        sym.module_id)
    t.symbols_by_id;
  Hashtbl.iter
    (fun _ (m : module_record) ->
      List.iter
        (fun compool_name_id ->
          let edge =
            {
              importing_module = m.module_id;
              imported_name_id = compool_name_id;
              import_kind = ImportCompool;
              source_range = PackedRange.make ~start_offset:0 ~end_offset:0;
            }
          in
          t.imports := edge :: !(t.imports))
        m.imported_compools)
    t.modules_by_id;
  List.iter
    (fun (edge : import_edge) ->
      match Hashtbl.find_opt t.compool_module_ids_by_name edge.imported_name_id with
      | None -> ()
      | Some module_ids ->
          Hashtbl.replace t.dependencies.module_imports edge.importing_module
            (sorted_unique_ints
               (module_ids
               @ Option.value
                   (Hashtbl.find_opt t.dependencies.module_imports
                      edge.importing_module)
                   ~default:[]));
          List.iter
            (fun imported_module ->
              add_to_list_tbl t.dependencies.module_imported_by imported_module
                edge.importing_module)
            module_ids)
    !(t.imports);
  Hashtbl.iter
    (fun ref_id (refn : reference_record) ->
      add_to_list_tbl t.reference_ids_by_module refn.module_id ref_id;
      Option.iter
        (fun sym_id ->
          Hashtbl.replace t.resolved_symbol_by_reference ref_id sym_id;
          add_to_list_tbl t.reference_ids_by_symbol sym_id ref_id;
          add_to_list_tbl t.dependencies.symbol_references sym_id ref_id;
          add_to_list_tbl t.dependencies.symbol_used_by_modules sym_id
            refn.module_id)
        refn.resolved_symbol_id)
    t.references_by_id;
  Hashtbl.iter
    (fun file_id ids ->
      Hashtbl.replace t.dependencies.file_declares_symbols file_id
        (sorted_unique_ints ids))
    t.symbol_ids_by_file;
  Hashtbl.iter
    (fun file_id ref_ids ->
      let symbols =
        ref_ids
        |> List.filter_map (fun ref_id ->
               match Hashtbl.find_opt t.references_by_id ref_id with
               | Some { resolved_symbol_id = Some sym_id; _ } -> Some sym_id
               | _ -> None)
        |> sorted_unique_ints
      in
      Hashtbl.replace t.dependencies.file_uses_symbols file_id symbols)
    t.reference_ids_by_file;
  let by_external = Hashtbl.create 512 in
  Hashtbl.iter
    (fun _ (sym : symbol_record) ->
      match sym.external_info with
      | None -> ()
      | Some info ->
          let defs, refs =
            Option.value
              (Hashtbl.find_opt by_external info.external_name_id)
              ~default:([], [])
          in
          if info.is_def then
            Hashtbl.replace by_external info.external_name_id
              (sym.symbol_id :: defs, refs)
          else
            Hashtbl.replace by_external info.external_name_id
              (defs, sym.symbol_id :: refs))
    t.symbols_by_id;
  Hashtbl.iter
    (fun name_id (defs, refs) ->
      let defs = sorted_unique_ints defs in
      let refs = sorted_unique_ints refs in
      let matching_def =
        match defs with def :: _ -> Some def | [] -> None
      in
      let duplicate_defs = match defs with _ :: _ :: _ -> defs | _ -> [] in
      let unresolved_refs = if matching_def = None then refs else [] in
      Hashtbl.replace t.external_bindings name_id
        {
          external_name_id = name_id;
          matching_def;
          ref_symbols = refs;
          duplicate_defs;
          unresolved_refs;
        })
    by_external;
  Hashtbl.iter
    (fun module_id (m : module_record) ->
      let local_symbols =
        Option.value (Hashtbl.find_opt t.symbol_ids_by_module module_id) ~default:[]
        |> sorted_unique_ints
      in
      let imported_symbols =
        m.imported_compools
        |> List.concat_map (fun name_id ->
               match Hashtbl.find_opt t.compool_module_ids_by_name name_id with
               | None -> []
               | Some module_ids ->
                   module_ids
                   |> List.concat_map (fun imported_module ->
                          Option.value
                            (Hashtbl.find_opt t.exports_by_owner_module
                               imported_module)
                            ~default:[]
                          |> List.map (fun e -> e.symbol_id)))
        |> sorted_unique_ints
      in
      let system_symbols =
        Option.value (Hashtbl.find_opt t.symbol_ids_by_kind Builtin) ~default:[]
        |> sorted_unique_ints
      in
      let by_name = Hashtbl.create 256 in
      let add_symbol_id sym_id =
        match Hashtbl.find_opt t.symbols_by_id sym_id with
        | None -> ()
        | Some sym -> add_to_list_tbl by_name sym.name_id sym_id
      in
      List.iter add_symbol_id (local_symbols @ imported_symbols @ system_symbols);
      Hashtbl.replace t.visibility_by_module module_id
        { module_id; local_symbols; imported_symbols; system_symbols; by_name })
    t.modules_by_id;
  let same_range (a : PackedRange.t) (b : PackedRange.t) =
    a.start_offset = b.start_offset && a.end_offset = b.end_offset
  in
  let same_symbol_reference_location (sym : symbol_record)
      (refn : reference_record) =
    sym.file_id = refn.file_id && same_range sym.name_range refn.range
  in
  let matching_external_def_id (refn : reference_record) : symbol_id option =
    let name_id =
      match refn.reference_kind with
      | ImportRef -> Some refn.name_id
      | _ -> (
          match refn.resolved_symbol_id with
          | None -> None
          | Some sym_id -> (
              match Hashtbl.find_opt t.symbols_by_id sym_id with
              | Some sym when sym.kind = ExternalRef -> Some sym.name_id
              | _ -> None))
    in
    match name_id with
    | None -> None
    | Some name_id -> (
        match Hashtbl.find_opt t.external_bindings name_id with
        | Some { matching_def = Some sym_id; _ }
          when Hashtbl.mem t.symbols_by_id sym_id ->
            Some sym_id
        | _ -> None)
  in
  let should_refresh_resolution (refn : reference_record) =
    match refn.reference_kind with
    | DefRef -> false
    | ImportRef -> true
    | _ -> (
        match refn.resolved_symbol_id with
        | None -> true
        | Some sym_id -> (
            match Hashtbl.find_opt t.symbols_by_id sym_id with
            | None -> true
            | Some sym when sym.kind = ExternalRef ->
                matching_external_def_id refn <> None
            | Some sym -> same_symbol_reference_location sym refn))
  in
  let priority_for_reference (refn : reference_record) (sym : symbol_record) =
    let self_hit = same_symbol_reference_location sym refn in
    let base =
      match sym.kind with
      | Item | ConstantItem | Table | Type | Proc | Function | FormalParam
      | Label | Define | StatusValue | Builtin | AsmExternal ->
          0
      | ExternalDef -> 1
      | ExternalRef -> 2
      | Block | Compool -> 3
      | UnknownSymbol -> 4
    in
    if self_hit then base + 100 else base
  in
  let choose_visible_symbol (refn : reference_record) =
    match matching_external_def_id refn with
    | Some sym_id -> Some sym_id
    | None -> (
        match Hashtbl.find_opt t.visibility_by_module refn.module_id with
        | None -> refn.resolved_symbol_id
        | Some env -> (
            match Hashtbl.find_opt env.by_name refn.name_id with
            | None | Some [] -> refn.resolved_symbol_id
            | Some candidates ->
                candidates
                |> List.filter_map (fun sym_id ->
                       Hashtbl.find_opt t.symbols_by_id sym_id)
                |> List.sort (fun (a : symbol_record) (b : symbol_record) ->
                       match
                         compare (priority_for_reference refn a)
                           (priority_for_reference refn b)
                       with
                       | 0 ->
                           compare a.name_range.start_offset
                             b.name_range.start_offset
                       | n -> n)
                |> function
                | sym :: _ -> Some sym.symbol_id
                | [] -> refn.resolved_symbol_id))
  in
  Hashtbl.iter
    (fun ref_id (refn : reference_record) ->
      if should_refresh_resolution refn then
        let resolved_symbol_id = choose_visible_symbol refn in
        Hashtbl.replace t.references_by_id ref_id
          { refn with resolved_symbol_id })
    t.references_by_id;
  Hashtbl.clear t.resolved_symbol_by_reference;
  Hashtbl.clear t.reference_ids_by_symbol;
  Hashtbl.clear t.dependencies.symbol_references;
  Hashtbl.clear t.dependencies.symbol_used_by_modules;
  Hashtbl.clear t.dependencies.file_uses_symbols;
  Hashtbl.iter
    (fun ref_id (refn : reference_record) ->
      Option.iter
        (fun sym_id ->
          Hashtbl.replace t.resolved_symbol_by_reference ref_id sym_id;
          add_to_list_tbl t.reference_ids_by_symbol sym_id ref_id;
          add_to_list_tbl t.dependencies.symbol_references sym_id ref_id;
          add_to_list_tbl t.dependencies.symbol_used_by_modules sym_id
            refn.module_id)
        refn.resolved_symbol_id)
    t.references_by_id;
  Hashtbl.iter
    (fun file_id ref_ids ->
      let symbols =
        ref_ids
        |> List.filter_map (fun ref_id ->
               match Hashtbl.find_opt t.references_by_id ref_id with
               | Some { resolved_symbol_id = Some sym_id; _ } -> Some sym_id
               | _ -> None)
        |> sorted_unique_ints
      in
      Hashtbl.replace t.dependencies.file_uses_symbols file_id symbols)
    t.reference_ids_by_file

let semantic_hash_of_strings xs =
  xs |> List.sort String.compare |> String.concat "\n" |> Digest.string |> Digest.to_hex

let update_semantic_signatures t file_id snap =
  let exports =
    snap.Semantic_store.Snapshot.nav_defs
    |> List.filter_map (fun (_, d) ->
           if d.Semantic_store.Snapshot.metadata.Metadata.is_exported then
             Some d.key
           else None)
  in
  let imports =
    snap.imports
    |> List.map (fun (imp : Preprocess.import) -> imp.name)
  in
  let decls =
    snap.nav_defs
    |> List.map (fun (_, d) ->
           Printf.sprintf "%s:%d:%s" d.Semantic_store.Snapshot.key d.kind
             (Metadata.symbol_kind_label d.metadata.Metadata.jovial_kind))
  in
  let types =
    snap.nav_defs
    |> List.filter_map (fun (_, d) ->
           Option.map
             (fun (ti : Metadata.jovial_type_info) -> ti.display)
             d.Semantic_store.Snapshot.metadata.Metadata.type_info)
  in
  Hashtbl.replace t.semantic_signatures_by_file file_id
    {
      exports_hash = semantic_hash_of_strings exports;
      imports_hash = semantic_hash_of_strings imports;
      local_decls_hash = semantic_hash_of_strings decls;
      types_hash = semantic_hash_of_strings types;
    }

let upsert_document_snapshot t (doc : Document.t)
    (snap : Semantic_store.Snapshot.t) : unit =
  let uri_s = uri_key doc.Document.uri in
  let file_id = id_of_stable_string ("file|" ^ uri_s) in
  remove_file_id t file_id;
  let line_index = LineIndex.of_string doc.Document.text in
  let token_index = TokenIndex.of_lex_tokens t.name_table (tokens_for_doc doc) in
  let detected_module_header =
    Option.map (NameTable.intern_identifier t.name_table) doc.Document.compool_def
  in
  let file_record =
    {
      file_id;
      uri = doc.Document.uri;
      uri_key = uri_s;
      path_key = path_key_of_doc doc;
      lsp_version = doc.Document.lsp_version;
      rev = doc.Document.rev;
      parse_rev = doc.Document.parse_rev;
      content_hash = Digest.to_hex (Digest.string doc.Document.text);
      line_index;
      token_index;
      ast_available = doc.Document.ast <> None;
      parse_error_count = List.length doc.Document.parse_diags;
      detected_module_header;
    }
  in
  Hashtbl.replace t.files_by_id file_id file_record;
  Hashtbl.replace t.file_id_by_uri uri_s file_id;
  Option.iter
    (fun path_key -> if path_key <> "" then Hashtbl.replace t.file_id_by_path path_key file_id)
    file_record.path_key;
  let module_name = module_name_for_doc doc snap in
  let module_name_id = NameTable.intern_identifier t.name_table module_name in
  let module_id = id_of_stable_string ("module|" ^ uri_s) in
  let root_scope = id_of_stable_string ("scope|module|" ^ uri_s) in
  let module_record =
    {
      module_id;
      file_id;
      module_name_id;
      module_kind = module_kind_for_doc doc snap;
      root_scope;
      declared_symbols = [];
      def_symbols = [];
      ref_symbols = [];
      imported_compools =
        snap.imports
        |> List.filter_map (fun (imp : Preprocess.import) ->
                match imp.kind with
                | Preprocess.Compool ->
                    Some
                      (NameTable.intern_identifier t.name_table
                         (module_reference_name imp.name)))
        |> sorted_unique_ints;
    }
  in
  Hashtbl.replace t.modules_by_id module_id module_record;
  Hashtbl.replace t.module_id_by_file file_id module_id;
  Hashtbl.replace t.scopes_by_id root_scope
    {
      scope_id = root_scope;
      scope_kind =
        (match module_record.module_kind with
        | CompoolModule -> CompoolScope
        | ProcedureModule | MainProgram -> ModuleScope
        | UnknownModule -> UnknownScope);
      file_id = Some file_id;
      module_id = Some module_id;
      parent_scope = None;
      children_scopes = [];
      range =
        Some
          (PackedRange.make ~start_offset:0
             ~end_offset:(String.length doc.Document.text));
      local_symbols = [];
    };
  List.iter
    (fun (stable_sym_id, d) ->
      let name_text =
        let spelling = String.trim d.Semantic_store.Snapshot.name in
        if spelling = "" then d.Semantic_store.Snapshot.key else spelling
      in
      let name_id = NameTable.intern_identifier t.name_table name_text in
      let fallback = symbol_kind_of_lsp_int d.kind in
      let type_id = type_id_for_metadata t d.metadata in
      let sym_id = id_of_stable_string ("symbol|" ^ stable_sym_id) in
      let declaration_range = loc_range d.loc in
      let name_range =
        name_range_for_declaration token_index name_id declaration_range
      in
      let sym =
        {
          symbol_id = sym_id;
          stable_key = stable_sym_id;
          name_id;
          spelling = d.name;
          kind = symbol_kind_of_metadata d.metadata fallback;
          file_id;
          module_id = Some module_id;
          scope_id = root_scope;
          declaration_range;
          name_range;
          declaration_text =
            source_line_at_offset doc.Document.text name_range.start_offset;
          type_id;
          signature_id = None;
          storage_info = storage_info_of_metadata d.metadata;
          external_info = external_info_of_metadata t.name_table name_id d.metadata;
          metadata = d.metadata;
        }
      in
      Hashtbl.replace t.symbols_by_id sym_id sym)
    snap.nav_defs;
  List.iter
    (fun (stable_sym_id, occs) ->
      let resolved_symbol_id =
        Hashtbl.find_opt t.symbols_by_id
          (id_of_stable_string ("symbol|" ^ stable_sym_id))
        |> Option.map (fun (sym : symbol_record) -> sym.symbol_id)
      in
      let def_for_symbol =
        snap.nav_defs
        |> List.find_map (fun (sid, d) ->
               if sid = stable_sym_id then Some d else None)
      in
      List.iter
        (fun ((occ_uri, loc) : Semantic_store.Snapshot.nav_occ) ->
          if uri_key occ_uri = uri_s then
            let name_id =
              match resolved_symbol_id with
              | Some sym_id -> (
                  match Hashtbl.find_opt t.symbols_by_id sym_id with
                  | Some sym -> sym.name_id
                  | None -> NameTable.intern_identifier t.name_table stable_sym_id)
              | None -> NameTable.intern_identifier t.name_table stable_sym_id
            in
            let is_decl =
              match def_for_symbol with Some d -> loc_equal loc d.loc | None -> false
            in
            let reference_id =
              id_of_stable_string
                (Printf.sprintf "ref|%s|%s|%d|%d" stable_sym_id uri_s
                   loc.Ast.Loc.start_pos.offset loc.Ast.Loc.end_pos.offset)
            in
            let refn =
              {
                reference_id;
                file_id;
                module_id;
                scope_id = root_scope;
                name_id;
                range = loc_range loc;
                reference_kind = classify_reference ~is_decl def_for_symbol;
                resolved_symbol_id;
              }
            in
            Hashtbl.replace t.references_by_id reference_id refn;
            add_to_list_tbl t.reference_ids_by_file file_id reference_id)
        occs)
    snap.nav_occs;
  let diags = diagnostics_from_doc ~file_id ~idx:line_index doc in
  List.iter
    (fun diag ->
      Hashtbl.replace t.diagnostics_by_id diag.diag_id diag;
      add_to_list_tbl t.diagnostic_ids_by_file file_id diag.diag_id)
    diags;
  update_semantic_signatures t file_id snap;
  rebuild_secondary_indexes t

let remove_uri t ~(uri : T.DocumentUri.t) : unit =
  match Hashtbl.find_opt t.file_id_by_uri (uri_key uri) with
  | None -> ()
  | Some file_id ->
      remove_file_id t file_id;
      rebuild_secondary_indexes t

let invalidate_path_and_dependents t ~(path_key : string) : T.DocumentUri.t list =
  let path_key = Uri_path.normalize_path_key path_key in
  let direct_file_ids =
    match Hashtbl.find_opt t.file_id_by_path path_key with
    | Some file_id -> [ file_id ]
    | None ->
        let prefix =
          let n = String.length path_key in
          if n > 0 && path_key.[n - 1] = '/' then path_key else path_key ^ "/"
        in
        Hashtbl.fold
          (fun pk file_id acc ->
            if String.starts_with ~prefix pk then file_id :: acc else acc)
          t.file_id_by_path []
  in
  let impacted = Hashtbl.create 16 in
  let add_file_id file_id = Hashtbl.replace impacted file_id true in
  List.iter add_file_id direct_file_ids;
  List.iter
    (fun file_id ->
      match Hashtbl.find_opt t.module_id_by_file file_id with
      | None -> ()
      | Some module_id -> (
          match Hashtbl.find_opt t.modules_by_id module_id with
          | Some m when m.module_kind = CompoolModule -> (
              match Hashtbl.find_opt t.dependencies.module_imported_by module_id with
              | None -> ()
              | Some importers ->
                  List.iter
                    (fun importer_module ->
                      match Hashtbl.find_opt t.modules_by_id importer_module with
                      | Some importer -> add_file_id importer.file_id
                      | None -> ())
                    importers)
          | _ -> ()))
    direct_file_ids;
  let removed_uris =
    Hashtbl.fold
      (fun file_id _ acc ->
        match Hashtbl.find_opt t.files_by_id file_id with
        | None -> acc
        | Some file -> file.uri :: acc)
      impacted []
  in
  Hashtbl.iter (fun file_id _ -> remove_file_id t file_id) impacted;
  if removed_uris <> [] then rebuild_secondary_indexes t;
  List.rev removed_uris

let file_current_for_doc t (doc : Document.t) : bool =
  match Hashtbl.find_opt t.file_id_by_uri (uri_key doc.Document.uri) with
  | None -> false
  | Some file_id -> (
      match Hashtbl.find_opt t.files_by_id file_id with
      | None -> false
      | Some file ->
          file.parse_rev = doc.Document.parse_rev
          && file.rev = doc.Document.rev
          && file.content_hash = Digest.to_hex (Digest.string doc.Document.text))

let file_id_for_uri t ~(uri : T.DocumentUri.t) =
  Hashtbl.find_opt t.file_id_by_uri (uri_key uri)

let module_id_for_file t file_id = Hashtbl.find_opt t.module_id_by_file file_id
let file_by_id t file_id = Hashtbl.find_opt t.files_by_id file_id
let symbol_by_id t symbol_id = Hashtbl.find_opt t.symbols_by_id symbol_id

let symbols_by_name t name_id =
  Option.value (Hashtbl.find_opt t.symbol_ids_by_name name_id) ~default:[]
  |> List.filter_map (fun id -> Hashtbl.find_opt t.symbols_by_id id)

let visible_environment t module_id =
  Hashtbl.find_opt t.visibility_by_module module_id

let reference_at_offset t (file : file_record) (offset : int) :
    reference_record option =
  let ids =
    Option.value (Hashtbl.find_opt t.reference_ids_by_file file.file_id)
      ~default:[]
  in
  ids
  |> List.filter_map (fun id -> Hashtbl.find_opt t.references_by_id id)
  |> List.filter (fun (refn : reference_record) ->
         PackedRange.contains refn.range offset)
  |> List.sort (fun (a : reference_record) (b : reference_record) ->
         match compare (PackedRange.length a.range) (PackedRange.length b.range) with
         | 0 -> compare a.range.start_offset b.range.start_offset
         | n -> n)
  |> function
  | refn :: _ -> Some refn
  | [] -> None

let declaration_at_offset t (file : file_record) (offset : int) :
    symbol_record option =
  let ids =
    Option.value (Hashtbl.find_opt t.symbol_ids_by_file file.file_id)
      ~default:[]
  in
  ids
  |> List.filter_map (fun id -> Hashtbl.find_opt t.symbols_by_id id)
  |> List.filter (fun sym -> PackedRange.contains sym.name_range offset)
  |> List.sort (fun a b ->
         match
           compare (PackedRange.length a.name_range)
             (PackedRange.length b.name_range)
         with
         | 0 -> compare a.name_range.start_offset b.name_range.start_offset
         | n -> n)
  |> function
  | sym :: _ -> Some sym
  | [] -> None

let loc_for_file_range (file : file_record) (range : PackedRange.t) =
  LineIndex.loc_of_range
    ~file:
      (match file.path_key with
      | Some _ -> Uri_path.file_path_of_uri file.uri
      | None -> None)
    file.line_index range

let location_for_file_range (file : file_record) (range : PackedRange.t) :
    T.Location.t =
  T.Location.create ~uri:file.uri
    ~range:(LineIndex.lsp_range_of_range file.line_index range)

let same_packed_range (a : PackedRange.t) (b : PackedRange.t) : bool =
  a.start_offset = b.start_offset && a.end_offset = b.end_offset

let self_reference_hit (hit : symbol_hit) : bool =
  match hit.reference with
  | Some (refn : reference_record) ->
      hit.symbol.file_id = refn.file_id
      && same_packed_range hit.symbol.name_range refn.range
  | None -> false

let symbol_hit_of_symbol ?reference t (sym : symbol_record) : symbol_hit option =
  let reference : reference_record option = reference in
  match Hashtbl.find_opt t.files_by_id sym.file_id with
  | None -> None
  | Some file ->
      let range =
        match reference with
        | Some refn -> refn.range
        | None -> sym.name_range
      in
      Some { symbol = sym; reference; file; loc = loc_for_file_range file range }

let symbol_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    symbol_hit option =
  match file_id_for_uri t ~uri with
  | None -> None
  | Some file_id -> (
      match Hashtbl.find_opt t.files_by_id file_id with
      | None -> None
      | Some file -> (
          match LineIndex.offset_of_position file.line_index pos with
          | None -> None
          | Some offset -> (
              match reference_at_offset t file offset with
              | Some ({ resolved_symbol_id = Some sym_id; _ } as refn) -> (
                  match Hashtbl.find_opt t.symbols_by_id sym_id with
                  | Some sym -> symbol_hit_of_symbol ~reference:refn t sym
                  | None -> None)
              | Some refn -> (
                  match declaration_at_offset t file offset with
                  | Some sym -> symbol_hit_of_symbol ~reference:refn t sym
                  | None -> None)
              | None -> (
                  match declaration_at_offset t file offset with
                  | Some sym -> symbol_hit_of_symbol t sym
                  | None -> None))))

let definition_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.Location.t list option =
  let location_for_symbol (sym : symbol_record) =
    match Hashtbl.find_opt t.files_by_id sym.file_id with
    | None -> Some []
    | Some file -> Some [ location_for_file_range file sym.name_range ]
  in
  let matching_external_def (sym : symbol_record) =
    match Hashtbl.find_opt t.external_bindings sym.name_id with
    | Some { matching_def = Some sym_id; _ } -> (
        match Hashtbl.find_opt t.symbols_by_id sym_id with
        | Some def when def.symbol_id <> sym.symbol_id -> Some def
        | _ -> None)
    | _ -> None
  in
  match symbol_at_position t ~uri ~pos with
  | None -> None
  | Some hit when self_reference_hit hit -> None
  | Some { symbol = { kind = ExternalRef; _ } as symbol; _ } -> (
      match matching_external_def symbol with
      | Some def -> location_for_symbol def
      | None -> None)
  | Some { symbol; _ } -> (
      location_for_symbol symbol)

let implementation_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    : T.Location.t list option =
  let location_for_symbol (sym : symbol_record) =
    match Hashtbl.find_opt t.files_by_id sym.file_id with
    | None -> Some []
    | Some file -> Some [ location_for_file_range file sym.name_range ]
  in
  let matching_external_def (sym : symbol_record) =
    match Hashtbl.find_opt t.external_bindings sym.name_id with
    | Some { matching_def = Some sym_id; _ } -> (
        match Hashtbl.find_opt t.symbols_by_id sym_id with
        | Some def when def.symbol_id <> sym.symbol_id -> Some def
        | _ -> None)
    | _ -> None
  in
  match symbol_at_position t ~uri ~pos with
  | None -> None
  | Some hit when self_reference_hit hit -> None
  | Some { symbol = { kind = ExternalRef; _ } as symbol; _ } -> (
      match matching_external_def symbol with
      | Some def -> location_for_symbol def
      | None ->
          (* An unresolved REF is an import-side declaration, not an
             implementation. Let WorkspaceQuery fall through to the ASM-aware
             resolver instead of returning the REF declaration as authoritative. *)
          None)
  | Some { symbol; _ } -> (
      match symbol.kind with
      | Proc | Function | ExternalDef | AsmExternal -> location_for_symbol symbol
      | _ -> None)

let type_definition_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    : T.Location.t list option =
  match symbol_at_position t ~uri ~pos with
  | None -> None
  | Some { symbol; _ } -> (
      match symbol.metadata.Metadata.type_info with
      | Some { Metadata.type_decl_uri = Some type_uri; type_decl_loc = Some loc; _ }
        ->
          Some
            [
              T.Location.create ~uri:type_uri
                ~range:(Lsp_conv.range_of_loc loc);
            ]
      | _ -> (
          match symbol.kind with
          | Type -> (
              match Hashtbl.find_opt t.files_by_id symbol.file_id with
              | None -> Some []
              | Some file -> Some [ location_for_file_range file symbol.name_range ])
          | _ -> None))

let references_for_symbol t (symbol_id : symbol_id) : reference_record list =
  Option.value (Hashtbl.find_opt t.reference_ids_by_symbol symbol_id)
    ~default:[]
  |> List.filter_map (fun id -> Hashtbl.find_opt t.references_by_id id)
  |> List.sort (fun (a : reference_record) (b : reference_record) ->
         match compare a.file_id b.file_id with
         | 0 -> compare a.range.start_offset b.range.start_offset
         | n -> n)

let references_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    ~(include_declaration : bool) : T.Location.t list option =
  match symbol_at_position t ~uri ~pos with
  | None -> None
  | Some { symbol; _ } ->
      let refs =
        references_for_symbol t symbol.symbol_id
        |> List.filter (fun (refn : reference_record) ->
               include_declaration || refn.reference_kind <> DefRef)
      in
      let locations =
        refs
        |> List.filter_map (fun (refn : reference_record) ->
               match Hashtbl.find_opt t.files_by_id refn.file_id with
               | None -> None
               | Some file -> Some (location_for_file_range file refn.range))
      in
      Some locations

let hover_markdown ?range (value : string) : T.Hover.t =
  let contents =
    `MarkupContent (T.MarkupContent.create ~kind:T.MarkupKind.Markdown ~value)
  in
  T.Hover.create ~contents ?range ()

let type_display_for_symbol t (sym : symbol_record) : string option =
  match sym.type_id with
  | Some type_id -> (
      match Hashtbl.find_opt t.types_by_id type_id with
      | Some (ty : type_record) when String.trim ty.display <> "" ->
          Some ty.display
      | _ -> None)
  | None -> (
      match sym.metadata.Metadata.type_info with
      | Some info when String.trim info.Metadata.display <> "" ->
          Some info.Metadata.display
      | _ -> None)

let hover_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.Hover.t option =
  match symbol_at_position t ~uri ~pos with
  | None -> None
  | Some hit ->
      let sym = hit.symbol in
      let metadata = sym.metadata in
      if metadata.Metadata.jovial_kind = Metadata.JovialField then None
      else
      let range =
        match hit.reference with
        | Some (refn : reference_record) ->
            LineIndex.lsp_range_of_range hit.file.line_index refn.range
        | None -> LineIndex.lsp_range_of_range hit.file.line_index sym.name_range
      in
      let facts =
        [
          Some
            (Printf.sprintf "**Kind:** %s"
               (Metadata.metadata_summary metadata));
          Option.map
            (fun ty -> Printf.sprintf "**Type:** `%s`" ty)
            (type_display_for_symbol t sym);
          Some
            (Printf.sprintf "**Visibility:** %s"
               (Metadata.external_label metadata.Metadata.external_kind));
          Option.map
            (fun text -> Printf.sprintf "**Declaration:** `%s`" text)
            sym.declaration_text;
          Some
            (Printf.sprintf "**File:** `%s`"
               (Uri_path.docuri_to_string hit.file.uri));
          Option.map
            (fun storage -> Printf.sprintf "**Storage:** %s" storage)
            (Option.bind sym.storage_info (fun storage ->
                 storage.storage_label));
        ]
        |> List.filter_map (fun x -> x)
      in
      let body =
        Printf.sprintf "### `%s`\n\n%s" sym.spelling (String.concat "\n" facts)
      in
      Some (hover_markdown ~range body)

let lsp_symbol_kind_of_cross_symbol (sym : symbol_record) : T.SymbolKind.t =
  match sym.metadata.Metadata.jovial_kind with
  | Metadata.JovialProgram | Metadata.JovialModule | Metadata.JovialCompool
  | Metadata.JovialCompoolImport | Metadata.JovialBlock ->
      T.SymbolKind.Module
  | Metadata.JovialType | Metadata.JovialBuiltinType -> T.SymbolKind.Class
  | Metadata.JovialField | Metadata.JovialLabel -> T.SymbolKind.Field
  | Metadata.JovialProcedure | Metadata.JovialFunction -> T.SymbolKind.Function
  | Metadata.JovialDefine | Metadata.JovialConstantItem
  | Metadata.JovialConstantTable | Metadata.JovialStatusConstant ->
      T.SymbolKind.Constant
  | Metadata.JovialItem | Metadata.JovialTable | Metadata.JovialOverlay
  | Metadata.JovialParameter ->
      T.SymbolKind.Variable
  | Metadata.JovialUnknownSymbol -> (
      match sym.kind with
      | Compool | Block -> T.SymbolKind.Module
      | Type -> T.SymbolKind.Class
      | Proc | Function | AsmExternal -> T.SymbolKind.Function
      | Define | ConstantItem | StatusValue | Builtin -> T.SymbolKind.Constant
      | Label -> T.SymbolKind.Field
      | Item | Table | FormalParam | ExternalDef | ExternalRef | UnknownSymbol ->
          T.SymbolKind.Variable)

let symbol_information_for_symbol t (sym : symbol_record) :
    T.SymbolInformation.t option =
  match Hashtbl.find_opt t.files_by_id sym.file_id with
  | None -> None
  | Some file ->
      Some
        (T.SymbolInformation.create ~name:sym.spelling
           ~kind:(lsp_symbol_kind_of_cross_symbol sym)
           ~location:(location_for_file_range file sym.name_range)
           ())

let trim_ascii (s : string) : string = String.trim s

let overlay_detail_of_declaration (text : string) : string option =
  let len = String.length text in
  let groups = ref [] in
  let depth = ref 0 in
  let group_start = ref None in
  for i = 0 to len - 1 do
    match text.[i] with
    | '(' ->
        if !depth = 0 then group_start := Some (i + 1);
        incr depth
    | ')' ->
        if !depth > 0 then (
          decr depth;
          if !depth = 0 then (
            match !group_start with
            | Some start when start <= i ->
                groups := String.sub text start (i - start) :: !groups;
                group_start := None
            | _ -> group_start := None))
    | _ -> ()
  done;
  let rec choose = function
    | [] -> None
    | group :: rest ->
        if String.contains group ',' then Some group else choose rest
  in
  match choose !groups with
  | None -> Some "overlay"
  | Some body ->
          let names =
            body |> String.split_on_char ','
            |> List.filter_map (fun part ->
                   let part = trim_ascii part in
                   let upper = String.uppercase_ascii part in
                   if part = "" || String.starts_with ~prefix:"SPACER" upper
                   then None
                   else Some part)
          in
          Some
            (match names with
            | [] -> "overlay"
            | _ -> "overlay " ^ String.concat ", " names)

let document_symbol_detail_for_symbol (sym : symbol_record) : string option =
  match sym.metadata.Metadata.jovial_kind with
  | Metadata.JovialOverlay -> (
      match sym.declaration_text with
      | Some text -> overlay_detail_of_declaration text
      | None -> Some "overlay")
  | _ -> Some (Metadata.symbol_kind_label sym.metadata.Metadata.jovial_kind)

let document_symbol_for_symbol t (sym : symbol_record) :
    T.DocumentSymbol.t option =
  match Hashtbl.find_opt t.files_by_id sym.file_id with
  | None -> None
  | Some file ->
      Some
        (T.DocumentSymbol.create ~name:sym.spelling
           ~kind:(lsp_symbol_kind_of_cross_symbol sym)
           ~range:(LineIndex.lsp_range_of_range file.line_index sym.declaration_range)
           ~selectionRange:
             (LineIndex.lsp_range_of_range file.line_index sym.name_range)
           ?detail:(document_symbol_detail_for_symbol sym)
           ~children:[] ())

let document_symbols_for_file t ~(uri : T.DocumentUri.t) :
    [ `DocumentSymbol of T.DocumentSymbol.t
    | `SymbolInformation of T.SymbolInformation.t ]
    list
    option =
  match file_id_for_uri t ~uri with
  | None -> None
  | Some file_id ->
      let ids =
        Option.value (Hashtbl.find_opt t.symbol_ids_by_file file_id) ~default:[]
      in
      let symbols =
        ids
        |> List.filter_map (fun id -> Hashtbl.find_opt t.symbols_by_id id)
        |> List.sort (fun (a : symbol_record) (b : symbol_record) ->
               compare a.name_range.start_offset b.name_range.start_offset)
      in
      Some
        (symbols
        |> List.filter_map (fun sym ->
               document_symbol_for_symbol t sym
               |> Option.map (fun info -> `DocumentSymbol info)))

let starts_with_ci ~(prefix : string) (s : string) : bool =
  let prefix = NameTable.normalize_identifier prefix in
  let key = NameTable.normalize_identifier s in
  let m = String.length prefix in
  String.length key >= m && String.sub key 0 m = prefix

let workspace_symbols t ~(query : string) : T.SymbolInformation.t list option =
  if Hashtbl.length t.symbols_by_id = 0 then None
  else
    let prefix = String.trim query in
    let symbols =
      Hashtbl.fold (fun _ (sym : symbol_record) acc -> sym :: acc) t.symbols_by_id []
      |> List.filter (fun sym ->
             prefix = "" || starts_with_ci ~prefix sym.spelling
             ||
             match NameTable.text t.name_table sym.name_id with
             | Some key -> starts_with_ci ~prefix key
             | None -> false)
      |> List.sort (fun (a : symbol_record) (b : symbol_record) ->
             match String.compare a.spelling b.spelling with
             | 0 -> compare a.name_range.start_offset b.name_range.start_offset
             | n -> n)
      |> List.filter_map (symbol_information_for_symbol t)
    in
    Some symbols

let valid_rename_name (s : string) : bool =
  let s = String.trim s in
  s <> ""
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '$' -> true
         | _ -> false)
       s

let prepare_rename_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    :
    [ `Range of T.Range.t | `RangeWithPlaceholder of T.Range.t * string ] option
    =
  match symbol_at_position t ~uri ~pos with
  | None -> None
  | Some hit ->
      let range =
        match hit.reference with
        | Some (refn : reference_record) ->
            LineIndex.lsp_range_of_range hit.file.line_index refn.range
        | None ->
            LineIndex.lsp_range_of_range hit.file.line_index
              hit.symbol.name_range
      in
      Some (`RangeWithPlaceholder (range, hit.symbol.spelling))

let rename_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    ~(new_name : string) : T.WorkspaceEdit.t option =
  if not (valid_rename_name new_name) then None
  else
    match symbol_at_position t ~uri ~pos with
    | None -> None
    | Some hit ->
        let seen = Hashtbl.create 64 in
        let edits_by_uri : (T.DocumentUri.t, T.TextEdit.t list) Hashtbl.t =
          Hashtbl.create 16
        in
        let add_edit (file : file_record) (range : PackedRange.t) =
          let key =
            Printf.sprintf "%s|%d|%d" file.uri_key range.start_offset
              range.end_offset
          in
          if not (Hashtbl.mem seen key) then (
            Hashtbl.replace seen key true;
            let edit =
              T.TextEdit.create
                ~range:(LineIndex.lsp_range_of_range file.line_index range)
                ~newText:new_name
            in
            let prev =
              Option.value (Hashtbl.find_opt edits_by_uri file.uri) ~default:[]
            in
            Hashtbl.replace edits_by_uri file.uri (edit :: prev))
        in
        (match Hashtbl.find_opt t.files_by_id hit.symbol.file_id with
        | Some file -> add_edit file hit.symbol.name_range
        | None -> ());
        references_for_symbol t hit.symbol.symbol_id
        |> List.iter (fun (refn : reference_record) ->
               match Hashtbl.find_opt t.files_by_id refn.file_id with
               | Some file -> add_edit file refn.range
               | None -> ());
        let changes =
          Hashtbl.fold
            (fun uri edits acc -> (uri, List.rev edits) :: acc)
            edits_by_uri []
        in
        (match changes with
        | [] -> None
        | _ -> Some (T.WorkspaceEdit.create ~changes ()))

let completion_kind_of_symbol (sym : symbol_record) : T.CompletionItemKind.t =
  match sym.kind with
  | Proc | Function | AsmExternal -> T.CompletionItemKind.Function
  | Type -> T.CompletionItemKind.Class
  | Table | Item | ExternalRef | ExternalDef -> T.CompletionItemKind.Variable
  | ConstantItem | Define | StatusValue | Builtin -> T.CompletionItemKind.Constant
  | Compool | Block -> T.CompletionItemKind.Module
  | FormalParam -> T.CompletionItemKind.Variable
  | Label -> T.CompletionItemKind.Variable
  | UnknownSymbol -> T.CompletionItemKind.Variable

let completions_at_position t ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.CompletionItem.t list option =
  match file_id_for_uri t ~uri with
  | None -> None
  | Some file_id -> (
      match (Hashtbl.find_opt t.files_by_id file_id, module_id_for_file t file_id) with
      | Some file, Some module_id -> (
          let prefix =
            match LineIndex.offset_of_position file.line_index pos with
            | None -> ""
            | Some offset -> (
                match TokenIndex.find_at_offset file.token_index offset with
                | Some { raw_text = Some raw; kind = IdentifierToken; _ } -> raw
                | _ -> "")
          in
          match visible_environment t module_id with
          | None -> None
          | Some env ->
              let seen = Hashtbl.create 256 in
              let out = ref [] in
              let add ~rank sym_id =
                match Hashtbl.find_opt t.symbols_by_id sym_id with
                | None -> ()
                | Some sym ->
                    if starts_with_ci ~prefix sym.spelling then
                      let key =
                        Printf.sprintf "%d|%s" sym.name_id
                          (Metadata.symbol_kind_label
                             sym.metadata.Metadata.jovial_kind)
                      in
                      if not (Hashtbl.mem seen key) then (
                        Hashtbl.replace seen key true;
                        out :=
                          T.CompletionItem.create ~label:sym.spelling
                            ~kind:(completion_kind_of_symbol sym)
                            ~detail:
                              (Metadata.symbol_kind_label
                                 sym.metadata.Metadata.jovial_kind)
                            ~sortText:
                              (Printf.sprintf "%d_%s" rank
                                 (NameTable.normalize_identifier sym.spelling))
                            ()
                          :: !out)
              in
              List.iter (add ~rank:0) env.local_symbols;
              List.iter (add ~rank:1) env.imported_symbols;
              List.iter (add ~rank:2) env.system_symbols;
              Some (List.rev !out))
      | _ -> None)

let diagnostic_to_lsp t (diag : diagnostic_record) : T.Diagnostic.t option =
  match Hashtbl.find_opt t.files_by_id diag.file_id with
  | None -> None
  | Some file ->
      Some
        (T.Diagnostic.create
           ~range:(LineIndex.lsp_range_of_range file.line_index diag.range)
           ?severity:diag.severity ?source:diag.source
           ~message:(`String diag.message) ())

let diagnostics_for_file t ~(uri : T.DocumentUri.t) :
    T.Diagnostic.t list option =
  match file_id_for_uri t ~uri with
  | None -> None
  | Some file_id ->
      let ids =
        Option.value (Hashtbl.find_opt t.diagnostic_ids_by_file file_id)
          ~default:[]
      in
      Some
        (ids
         |> List.filter_map (fun id ->
                Option.bind (Hashtbl.find_opt t.diagnostics_by_id id)
                  (diagnostic_to_lsp t)))

let stats_json t =
  `Assoc
    [
      ("nameCount", `Int (NameTable.count t.name_table));
      ("fileCount", `Int (Hashtbl.length t.files_by_id));
      ("moduleCount", `Int (Hashtbl.length t.modules_by_id));
      ("scopeCount", `Int (Hashtbl.length t.scopes_by_id));
      ("symbolCount", `Int (Hashtbl.length t.symbols_by_id));
      ("referenceCount", `Int (Hashtbl.length t.references_by_id));
      ("typeCount", `Int (Hashtbl.length t.types_by_id));
      ("formulaCount", `Int (Hashtbl.length t.formulas_by_node));
      ("exportCount", `Int (List.length !(t.exports)));
      ("importCount", `Int (List.length !(t.imports)));
      ("visibilityCount", `Int (Hashtbl.length t.visibility_by_module));
      ("externalBindingCount", `Int (Hashtbl.length t.external_bindings));
      ("diagnosticCount", `Int (Hashtbl.length t.diagnostics_by_id));
      ( "persistentCacheMetadataPresent",
        `Bool (Option.is_some t.persistent_cache_metadata) );
    ]
