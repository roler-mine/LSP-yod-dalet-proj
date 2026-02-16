type entry = Ast.Loc.t * string
val clear : unit -> unit
val add : Ast.Loc.t -> string -> unit
val take : unit -> entry list
