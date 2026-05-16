(** Module overview: Status and diagnostic helpers for Jovial semantic analysis results. *)

type value = {
  name : string;
  key : string;
  loc : Ast.Loc.t;
  ordinal : int;
  representation : Ast.expr Ast.node option;
}

type owner = {
  owner_name : string option;
  owner_key : string option;
  owner_loc : Ast.Loc.t option;
  values : value list;
}

val normalize : string -> string

val owner_of_type_expr :
  ?owner_name:string ->
  ?owner_loc:Ast.Loc.t ->
  Ast.type_expr Ast.node ->
  owner option

val owners_of_type_expr :
  ?owner_name:string ->
  ?owner_loc:Ast.Loc.t ->
  Ast.type_expr Ast.node ->
  owner list

val owners_of_program : Ast.program -> owner list
val duplicate_values : owner -> (value * value) list
val value_is_member : owner -> string -> bool
val owner_display : owner -> string
val status_constructor_arg : Ast.expr Ast.node -> Ast.ident option
