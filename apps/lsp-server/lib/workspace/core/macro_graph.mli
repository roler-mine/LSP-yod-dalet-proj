(** Module overview: Tracks DEFINE macro relationships for dependency and change-impact analysis. *)

module T = Lsp.Types

type macro_actual = {
  formal : string option;
  loc : Ast.Loc.t;
  text : string;
}

type source_map = { generated_to_source : (Ast.Loc.t * Ast.Loc.t) list }

type expansion = {
  define_symbol_id : Symbol_id.t option;
  define : Preprocess.define;
  define_def : Workspace_nav_model.def;
  call_site_uri : T.DocumentUri.t;
  call_site_loc : Ast.Loc.t;
  call_name_loc : Ast.Loc.t;
  expanded_loc : Ast.Loc.t option;
  actuals : macro_actual list;
  source_map : source_map;
  provisional : bool;
  reason : Workspace_readiness.reason option;
}

type t

val empty : t
val of_document : Document.t -> t
val expansions : t -> expansion list
val source_map : t -> source_map

val macro_use_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> expansion option

val definition_of_macro_use :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> Workspace_nav_model.def option

val uses_of_define : t -> Workspace_nav_model.def -> expansion list

val expansion_at_position :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> expansion option

val source_loc_for_generated_loc : t -> Ast.Loc.t -> Ast.Loc.t option
