(** Module overview: Normalizes Jovial source-file extensions and filename matching rules. *)

val default_extensions : string list
val normalize_extension : string -> string option
val normalize_extensions : string list -> string list
val with_defaults : string list -> string list
val has_extension : extensions:string list -> string -> bool
val strip_known_extension : extensions:string list -> string -> string
