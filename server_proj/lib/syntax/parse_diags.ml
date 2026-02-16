(* lib/parse_diags.ml *)

type entry = Ast.Loc.t * string

let buf : entry list ref = ref []

let clear () : unit =
  buf := []

let add (loc : Ast.Loc.t) (msg : string) : unit =
  buf := (loc, msg) :: !buf

let take () : entry list =
  let xs = List.rev !buf in
  buf := [];
  xs
