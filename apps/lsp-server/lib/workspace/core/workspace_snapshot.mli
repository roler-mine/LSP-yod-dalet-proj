(** Module overview: Immutable workspace snapshot model used by persistent indexes and LSIF export. *)

module UriMap : Map.S with type key = string

type file_state = {
  uri : string;
  rev : int;
  lsp_version : int option;
  mode : Workspace_tuning.file_mode;
  text_size : int;
  tokens : Token_cache.t option;
  skeleton : Skeleton_index.skeleton_file option;
  ast : Ast.program option;
  semantic : Semantic_overlay.t option;
  diagnostics : Lsp.Types.Diagnostic.t list;
}

type snapshot = {
  generation : int;
  files : file_state UriMap.t;
  symbols : Symbol_index.t;
  scopes : Scope_graph.t;
  refs : Reference_index.t;
  deps : Dependency_graph.t;
}

val empty : unit -> snapshot
val of_workspace : Workspace_foundation.t -> snapshot
val current : unit -> snapshot
val publish : snapshot -> unit
val cached_for_workspace : Workspace_foundation.t -> snapshot option
val publish_for_workspace : Workspace_foundation.t -> snapshot -> unit
val file : snapshot -> uri:string -> file_state option
