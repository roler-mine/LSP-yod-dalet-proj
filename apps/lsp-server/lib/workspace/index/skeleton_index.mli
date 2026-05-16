(** Module overview: Indexes syntax skeletons before full semantic analysis is available. *)

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

val empty : uri:string -> rev:int -> size_bytes:int -> skeleton_file
val of_syntax_cache : uri:string -> rev:int -> size_bytes:int -> Syntax_cache.t -> skeleton_file
val of_document : Document.t -> skeleton_file option
val build_from_text : uri:string -> rev:int -> file:string option -> text:string -> skeleton_file
val symbols : skeleton_file -> symbol_decl list
val symbol_at_position : skeleton_file -> T.Position.t -> symbol_decl option
