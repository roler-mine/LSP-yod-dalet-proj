(** Module overview: Tracks imports/includes between source files for workspace graph traversal. *)

type edge_kind =
  | ICompoolImport
  | ICopyInclude
  | DefExport
  | RefImport
  | DefineUse
  | TypeUse
  | ProcedureCall
  | TableFieldUse

type edge = {
  source_uri : string;
  target : string;
  target_uri : string option;
  kind : edge_kind;
}

type t

val empty : unit -> t
val add_edge : t -> edge -> unit
val of_skeleton : Skeleton_index.skeleton_file -> t
val edges : t -> edge list
