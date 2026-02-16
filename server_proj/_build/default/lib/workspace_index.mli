type t

val build : root:string -> t

val root : t -> string
val compool_count : t -> int

(* name -> file path if found *)
val find_compool : t -> name:string -> string option

(* sample (name, path) pairs *)
val sample : t -> int -> (string * string) list

(* all indexed compool file paths *)
val all_paths : t -> string list
