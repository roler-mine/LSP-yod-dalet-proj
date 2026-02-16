module T = Lsp.Types

val position_of_ast_pos : Ast.Loc.pos -> T.Position.t
val range_of_loc : Ast.Loc.t -> T.Range.t

val location_of_loc : uri:T.DocumentUri.t -> Ast.Loc.t -> T.Location.t

val diagnostic :
  ?severity:T.DiagnosticSeverity.t ->
  ?source:string ->
  message:string ->
  Ast.Loc.t ->
  T.Diagnostic.t

val offset_of_lsp_position : Text_index.t -> T.Position.t -> int option
val lsp_position_of_offset : Text_index.t -> int -> T.Position.t
