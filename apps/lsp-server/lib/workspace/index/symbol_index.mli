(** Module overview: Maintains searchable symbol definitions and names across the workspace. *)

type symbol_id = Symbol_id.t

type type_info = { display : string }

type symbol_record = {
  id : symbol_id;
  name : string;
  normalized_name : string;
  kind : Skeleton_index.symbol_kind;
  declaration : Ast.Loc.t;
  definition : Ast.Loc.t option;
  container_scope : int;
  exported : bool;
  external_kind : [ `Def | `Ref | `Local | `System ];
  metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
  type_info : type_info option;
  docs : string option;
}

type t

val empty : unit -> t
val symbol_id_of_skeleton : uri:string -> Skeleton_index.symbol_decl -> symbol_id
val add : t -> symbol_record -> unit
val of_skeleton : Skeleton_index.skeleton_file -> t
val by_id : t -> symbol_id -> symbol_record option
val by_name : t -> string -> symbol_record list
val by_prefix : t -> string -> symbol_record list
val by_file : t -> string -> symbol_record list
val by_scope : t -> int -> symbol_record list
val exports_by_module : t -> string -> symbol_record list
val all : t -> symbol_record list
