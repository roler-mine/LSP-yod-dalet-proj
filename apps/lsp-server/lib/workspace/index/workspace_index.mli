(** Module overview: Filesystem-backed source locator.

    This is intentionally not the semantic authority for LSP requests. Hover,
    goto, references, rename, completion, and diagnostics should go through
    Workspace_query/Cross_file_index. This legacy index remains as a compact
    source-file discovery layer for startup scheduling, source-set changes,
    and coarse compool path hints while the centralized cross-file index owns
    symbols, scopes, references, types, and visibility. *)

type t
type file_change_kind = Created | Changed | Deleted

val of_source_files :
  source_extensions:string list -> root:string -> paths:string list -> t
val compool_count : t -> int
val source_count : t -> int
val source_total_bytes : t -> int
val is_complete : t -> bool
val source_import_hints : t -> path:string -> string list
val source_entry_hint : t -> path:string -> bool
val source_paths_for_proc_hint : t -> name:string -> string list
val replace_source_files : t -> paths:string list -> bool
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> t option

(* apply an external file-system change incrementally; returns true when COMPOOL mapping changed *)
val apply_file_change : t -> path:string -> kind:file_change_kind -> bool

(* name -> file path if found *)
val find_compool : t -> name:string -> string option

(* sample (name, path) pairs *)
val sample : t -> int -> (string * string) list

(* all indexed compool file paths *)
val all_paths : t -> string list

(* all indexed source file paths *)
val all_source_paths : t -> string list
