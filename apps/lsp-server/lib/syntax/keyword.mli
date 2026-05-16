(** Module overview: Case-insensitive Jovial keyword classification shared by the lexer and parser. *)

type class_ = Hard | Soft

val equal_upper_ascii : string -> string -> bool
val classify : string -> Parser.token option
val class_of_token : Parser.token -> class_ option
val is_soft_token : Parser.token -> bool
val is_builtin_type_name : string -> bool
