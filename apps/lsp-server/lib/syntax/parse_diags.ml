(* Module overview: Converts lexer and parser failures into user-facing syntax diagnostics. *)

(* lib/parse_diags.ml *)

type entry = Ast.Loc.t * string

let buf : entry list ref = ref []
let clear () : unit = buf := []
let add (loc : Ast.Loc.t) (msg : string) : unit = buf := (loc, msg) :: !buf

let take () : entry list =
  let xs = List.rev !buf in
  buf := [];
  xs
