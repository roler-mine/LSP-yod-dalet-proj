type t

val of_string : string -> t

val line_count : t -> int
val line_start_offset : t -> line:int -> int option

val offset_of_line_col : t -> line:int -> col:int -> int option
val line_col_of_offset : t -> int -> int * int
