(** Module overview: Central cross-file semantic index for workspace-wide lookup. *)

module T = Lsp.Types

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

module PackedRange : sig
  type t = { start_offset : int; end_offset : int }

  val make : start_offset:int -> end_offset:int -> t
  val contains : t -> int -> bool
  val length : t -> int
end

module NameTable : sig
  type t

  val create : unit -> t
  val clear : t -> unit

  (** JOVIAL identifier normalization: case-insensitive, apostrophe and dollar
      preserved, and normal names compared by their first 31 significant
      characters. Callers should use this only for identifier tokens, not for
      character/string literal payloads. *)
  val normalize_identifier : string -> string

  val intern_identifier : t -> string -> name_id
  val intern_raw : t -> string -> name_id
  val find_identifier : t -> string -> name_id option
  val text : t -> name_id -> string option
  val count : t -> int
end

module LineIndex : sig
  type t = Text_index.t

  val of_string : string -> t
  val offset_of_position : t -> T.Position.t -> int option
  val position_of_offset : t -> int -> T.Position.t
  val loc_of_range : file:string option -> t -> PackedRange.t -> Ast.Loc.t
  val lsp_range_of_range : t -> PackedRange.t -> T.Range.t
end

module TokenIndex : sig
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

  type t

  val empty : unit -> t
  val of_lex_tokens : NameTable.t -> Preprocess.lex_tok array -> t
  val find_at_offset : t -> int -> token option
  val all : t -> token list
  val count : t -> int
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
  metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
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

type t

val create : unit -> t
val reset : t -> unit
val name_table : t -> NameTable.t

val upsert_document_snapshot :
  t -> Document.t -> Semantic_store.Snapshot.t -> unit

val remove_uri : t -> uri:T.DocumentUri.t -> unit
val invalidate_path_and_dependents : t -> path_key:string -> T.DocumentUri.t list

val file_current_for_doc : t -> Document.t -> bool
val file_id_for_uri : t -> uri:T.DocumentUri.t -> file_id option
val module_id_for_file : t -> file_id -> module_id option
val file_by_id : t -> file_id -> file_record option
val symbol_by_id : t -> symbol_id -> symbol_record option
val symbols_by_name : t -> name_id -> symbol_record list
val visible_environment : t -> module_id -> visible_environment option

val symbol_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> symbol_hit option

val definition_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list option

val implementation_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list option

val type_definition_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list option

val references_at_position :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  include_declaration:bool ->
  T.Location.t list option

val hover_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Hover.t option

val completions_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.CompletionItem.t list option

val document_symbols_for_file :
  t ->
  uri:T.DocumentUri.t ->
  [ `DocumentSymbol of T.DocumentSymbol.t
  | `SymbolInformation of T.SymbolInformation.t ]
  list
  option

val workspace_symbols :
  t -> query:string -> T.SymbolInformation.t list option

val prepare_rename_at_position :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  [ `Range of T.Range.t | `RangeWithPlaceholder of T.Range.t * string ] option

val rename_at_position :
  t ->
  uri:T.DocumentUri.t ->
  pos:T.Position.t ->
  new_name:string ->
  T.WorkspaceEdit.t option

val diagnostics_for_file : t -> uri:T.DocumentUri.t -> T.Diagnostic.t list option
val stats_json : t -> Yojson.Safe.t
