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
