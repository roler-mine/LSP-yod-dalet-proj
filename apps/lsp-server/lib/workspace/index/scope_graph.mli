(** Module overview: Models lexical and module scopes for symbol lookup across Jovial files. *)

type symbol_id = string
type import_id = int

type scope_kind =
  | SystemScope
  | CompoolScope
  | ModuleScope
  | ModuleBodyScope
  | ProcedureScope
  | FunctionScope
  | BlockScope
  | TableScope
  | TypeScope
  | LoopScope
  | MacroScope

type scope = {
  id : int;
  parent : int option;
  kind : scope_kind;
  name : string option;
  loc : Ast.Loc.t;
  symbols : symbol_id list;
  imports : import_id list;
}

type t

val empty : unit -> t
val add_scope : t -> scope -> t
val of_skeleton : Skeleton_index.skeleton_file -> t
val scopes : t -> scope list
val innermost_scope_at : t -> Ast.Loc.pos -> scope option
val resolve_order : t -> scope_id:int -> int list
val by_id : t -> int -> scope option
val lookup_symbol_id : t -> scope_id:int -> normalized_name:string -> symbol_id option
