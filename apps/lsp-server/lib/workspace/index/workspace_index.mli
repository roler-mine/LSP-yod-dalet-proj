type t

type file_change_kind =
  | Created
  | Changed
  | Deleted

val start : root:string -> t
val build : root:string -> t

val root : t -> string
val compool_count : t -> int
val is_complete : t -> bool

(* process part of the pending directory tree; returns (dirs_scanned, files_scanned) *)
val scan_step : t -> max_dirs:int -> max_files:int -> int * int

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
