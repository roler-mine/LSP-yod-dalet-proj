(** Module overview: Semantic graph representation of symbols, scopes, definitions, and references. *)

module T = Lsp.Types

type symbol = {
  id : Symbol_id.t;
  name : string;
  key : string;
  kind : Workspace_symbol_metadata.jovial_symbol_kind;
  metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
  decl_uri : T.DocumentUri.t;
  decl_loc : Ast.Loc.t;
  scope_id : Scope_id.t option;
  type_id : Type_id.t option;
  body_range : Ast.Loc.t option;
}

type reference = {
  symbol_id : Symbol_id.t;
  uri : T.DocumentUri.t;
  loc : Ast.Loc.t;
  role : Workspace_symbol_metadata.jovial_decl_role;
}

type scope_kind =
  | SystemScope
  | CompoolScope
  | ModuleScope
  | ProcedureScope
  | BlockScope
  | TableScope
  | DefineScope

type scope = {
  id : Scope_id.t;
  kind : scope_kind;
  uri : T.DocumentUri.t;
  parent : Scope_id.t option;
  owner_symbol : Symbol_id.t option;
  range : Ast.Loc.t;
  mutable declarations : Symbol_id.t list;
  mutable imports : string list;
}

type duplicate_declaration = {
  scope_id : Scope_id.t;
  key : string;
  declarations : Symbol_id.t list;
}

type t = {
  symbols_by_id : (Symbol_id.t, symbol) Hashtbl.t;
  symbol_ids_by_decl_key : (string, Symbol_id.t) Hashtbl.t;
  references_by_symbol : (Symbol_id.t, reference list) Hashtbl.t;
  scopes_by_id : (Scope_id.t, scope) Hashtbl.t;
  duplicate_declarations_by_key : (string, duplicate_declaration) Hashtbl.t;
}

val create : unit -> t
val scope_kind_label : scope_kind -> string

val stable_decl_key :
  uri:T.DocumentUri.t ->
  loc:Ast.Loc.t ->
  key:string ->
  kind:Workspace_symbol_metadata.jovial_symbol_kind ->
  string

val symbol_id_for_decl :
  uri:T.DocumentUri.t ->
  loc:Ast.Loc.t ->
  key:string ->
  kind:Workspace_symbol_metadata.jovial_symbol_kind ->
  Symbol_id.t

val symbol_of_def : Workspace_nav_model.def -> symbol
val symbol_id_of_def : Workspace_nav_model.def -> Symbol_id.t
val add_symbol : t -> symbol -> unit
val add_reference : t -> reference -> unit
val add_def_symbol : t -> Workspace_nav_model.def -> symbol
val find_symbol : t -> Symbol_id.t -> symbol option
val find_symbol_by_def : t -> Workspace_nav_model.def -> symbol option
val references_for_symbol : t -> Symbol_id.t -> reference list
val symbols : t -> symbol list
val references : t -> reference list
val find_scope : t -> Scope_id.t -> scope option
val scopes : t -> scope list
val duplicate_declarations : t -> duplicate_declaration list
val scope_at_loc :
  t -> uri:T.DocumentUri.t -> Ast.Loc.t -> Scope_id.t option
val visible_symbols : t -> Scope_id.t -> Symbol_id.t list
val resolve_name : t -> Scope_id.t -> string -> Symbol_id.t list
val debug_json : t -> Yojson.Safe.t
val of_defs : Workspace_nav_model.def list -> t
val of_doc_defs : Document.t -> t
