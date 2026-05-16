(* Module overview: Indexes syntax skeletons before full semantic analysis is available. *)

module T = Lsp.Types

type module_kind =
  | MainProgram
  | ProcedureModule
  | CompoolModule
  | UnknownModule

type symbol_kind =
  | Program
  | Module
  | Compool
  | Procedure
  | Function
  | Item
  | Table
  | Block
  | Type
  | Label
  | Define
  | ExternalDef
  | ExternalRef

type symbol_decl = {
  name : string;
  normalized_name : string;
  kind : symbol_kind;
  loc : Ast.Loc.t;
  scope_id : int;
  exported : bool;
  imported : bool;
  metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
}

type import_kind = Icopy | Icompool | DirectExternal

type import = {
  kind : import_kind;
  name : string;
  loc : Ast.Loc.t;
}

type skeleton_file = {
  uri : string;
  rev : int;
  size_bytes : int;
  module_name : string option;
  module_kind : module_kind;
  imports : import list;
  exports : symbol_decl list;
  locals : symbol_decl list;
  defines : symbol_decl list;
  labels : symbol_decl list;
  diagnostics : T.Diagnostic.t list;
}

let normalize_name (s : string) = String.uppercase_ascii (String.trim s)

let empty ~(uri : string) ~(rev : int) ~(size_bytes : int) : skeleton_file =
  {
    uri;
    rev;
    size_bytes;
    module_name = None;
    module_kind = UnknownModule;
    imports = [];
    exports = [];
    locals = [];
    defines = [];
    labels = [];
    diagnostics = [];
  }

let symbol_kind_of_syntax = function
  | Syntax_cache.SkModule -> Module
  | Syntax_cache.SkCompool -> Compool
  | Syntax_cache.SkProcedure -> Procedure
  | Syntax_cache.SkFunction -> Function
  | Syntax_cache.SkItem -> Item
  | Syntax_cache.SkTable -> Table
  | Syntax_cache.SkBlock -> Block
  | Syntax_cache.SkType -> Type
  | Syntax_cache.SkLabel -> Label
  | Syntax_cache.SkDefineMacro -> Define

let metadata_of_decl ~(kind : symbol_kind) ~(exported : bool) ~(imported : bool)
    =
  let external_kind =
    if exported then Workspace_symbol_metadata.ExternalDef
    else if imported then Workspace_symbol_metadata.ExternalRef
    else Workspace_symbol_metadata.ExternalLocal
  in
  let jovial_kind =
    match kind with
    | Program -> Workspace_symbol_metadata.JovialProgram
    | Module -> Workspace_symbol_metadata.JovialModule
    | Compool -> Workspace_symbol_metadata.JovialCompool
    | Procedure -> Workspace_symbol_metadata.JovialProcedure
    | Function -> Workspace_symbol_metadata.JovialFunction
    | Item -> Workspace_symbol_metadata.JovialItem
    | Table -> Workspace_symbol_metadata.JovialTable
    | Block -> Workspace_symbol_metadata.JovialBlock
    | Type -> Workspace_symbol_metadata.JovialType
    | Label -> Workspace_symbol_metadata.JovialLabel
    | Define -> Workspace_symbol_metadata.JovialDefine
    | ExternalDef | ExternalRef -> Workspace_symbol_metadata.JovialUnknownSymbol
  in
  {
    Workspace_symbol_metadata.default_metadata with
    jovial_kind;
    external_kind;
    decl_role = Workspace_symbol_metadata.decl_role_of_external_kind external_kind;
    is_imported = imported;
    is_exported = exported;
  }

let decl_of_syntax (sym : Syntax_cache.skeleton_symbol) : symbol_decl =
  let base_kind = symbol_kind_of_syntax sym.sk_kind in
  let kind =
    match (sym.sk_exported, sym.sk_imported) with
    | true, _ -> ExternalDef
    | _, true -> ExternalRef
    | _ -> base_kind
  in
  {
    name = sym.sk_name;
    normalized_name = normalize_name sym.sk_name;
    kind;
    loc = sym.sk_loc;
    scope_id = 0;
    exported = sym.sk_exported;
    imported = sym.sk_imported;
    metadata =
      metadata_of_decl ~kind:base_kind ~exported:sym.sk_exported
        ~imported:sym.sk_imported;
  }

let loc_of_token ~(file : string option) (tok : Preprocess.lex_tok) : Ast.Loc.t =
  Ast.Loc.of_lexing_positions
    (Parser.token_span_start_p ~file tok)
    (Parser.token_span_end_p ~file tok)
    ~file

let import_of_preprocess (imp : Preprocess.import) : import =
  {
    kind = Icompool;
    name = imp.name;
    loc = imp.loc;
  }

let icopy_imports ~(file : string option) (tokens : Preprocess.lex_tok array) :
    import list =
  let len = Array.length tokens in
  let name_at i =
    if i < 0 || i >= len then None
    else
      match tokens.(i).Parser.tok with
      | Parser.ID s | Parser.STRINGLIT s -> Some (s, tokens.(i))
      | _ -> None
  in
  let rec find_target i steps =
    if i >= len || steps > 16 then None
    else
      match tokens.(i).Parser.tok with
      | Parser.LPAREN | Parser.RPAREN | Parser.COMMA ->
          find_target (i + 1) (steps + 1)
      | Parser.SEMI | Parser.TERM | Parser.EOF -> None
      | _ -> (
          match name_at i with
          | Some _ as hit -> hit
          | None -> find_target (i + 1) (steps + 1))
  in
  let out = ref [] in
  for i = 0 to len - 1 do
    match tokens.(i).Parser.tok with
    | Parser.ID raw when normalize_name raw = "ICOPY" -> (
        match find_target (i + 1) 0 with
        | None -> ()
        | Some (name, tok) ->
            out := { kind = Icopy; name; loc = loc_of_token ~file tok } :: !out)
    | _ -> ()
  done;
  List.rev !out

let module_kind_of_symbols (symbols : symbol_decl list) : module_kind =
  if
    List.exists
      (fun (s : symbol_decl) ->
        s.kind = Compool || (s.kind = Module && s.name = "COMPOOL"))
      symbols
  then
    CompoolModule
  else if List.exists (fun (s : symbol_decl) -> s.kind = Procedure) symbols
  then ProcedureModule
  else if
    List.exists
      (fun (s : symbol_decl) -> s.kind = Program || s.kind = Module)
      symbols
  then
    MainProgram
  else UnknownModule

let of_syntax_cache ~(uri : string) ~(rev : int) ~(size_bytes : int)
    (syntax : Syntax_cache.t) : skeleton_file =
  let raw_symbols = List.map decl_of_syntax syntax.skeleton.symbols in
  let defines, non_defines =
    List.partition (fun (s : symbol_decl) -> s.kind = Define) raw_symbols
  in
  let labels, non_labels =
    List.partition (fun (s : symbol_decl) -> s.kind = Label) non_defines
  in
  let exports, locals =
    List.partition (fun (s : symbol_decl) -> s.exported || s.imported) non_labels
  in
  let module_name =
    match
      List.find_opt
        (fun (s : symbol_decl) ->
          s.kind = Module || s.kind = Compool || s.kind = Program)
        raw_symbols
    with
    | Some s -> Some s.name
    | None -> syntax.skeleton.compool_def
  in
  let module_kind =
    match syntax.skeleton.compool_def with
    | Some _ -> CompoolModule
    | None -> module_kind_of_symbols raw_symbols
  in
  let token_imports =
    match syntax.raw_tokens with
    | None -> []
    | Some tokens -> icopy_imports ~file:None tokens
  in
  {
    uri;
    rev;
    size_bytes;
    module_name;
    module_kind;
    imports = List.map import_of_preprocess syntax.skeleton.imports @ token_imports;
    exports;
    locals;
    defines;
    labels;
    diagnostics = syntax.preprocess.diags @ syntax.parse.diags;
  }

let of_document (doc : Document.t) : skeleton_file option =
  match Document.current_parse doc with
  | Some { Document.parsed_syntax = Some syntax; _ } ->
      Some
        (of_syntax_cache
           ~uri:(Uri_path.docuri_to_string doc.Document.uri)
           ~rev:doc.Document.rev
           ~size_bytes:(String.length doc.Document.text)
           syntax)
  | _ -> None

let build_from_text ~(uri : string) ~(rev : int) ~(file : string option)
    ~(text : string) : skeleton_file =
  let syntax = Syntax_cache.build_with_profile ~profile:Parser.Background ~file ~text () in
  of_syntax_cache ~uri ~rev ~size_bytes:(String.length text) syntax

let symbols (s : skeleton_file) : symbol_decl list =
  s.exports @ s.locals @ s.defines @ s.labels

let position_in_loc (pos : T.Position.t) (loc : Ast.Loc.t) : bool =
  let line = pos.T.Position.line + 1 in
  let col = pos.T.Position.character in
  let sp = loc.Ast.Loc.start_pos in
  let ep = loc.Ast.Loc.end_pos in
  (line > sp.line || (line = sp.line && col >= sp.col))
  && (line < ep.line || (line = ep.line && col <= ep.col))

let symbol_at_position (sk : skeleton_file) (pos : T.Position.t) :
    symbol_decl option =
  symbols sk
  |> List.find_opt (fun (sym : symbol_decl) -> position_in_loc pos sym.loc)
