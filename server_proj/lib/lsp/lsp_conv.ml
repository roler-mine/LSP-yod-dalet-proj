module T = Lsp.Types

let position_of_ast_pos (p : Ast.Loc.pos) : T.Position.t =
  { T.Position.line = max 0 (p.line - 1); character = max 0 p.col }

let range_of_loc (l : Ast.Loc.t) : T.Range.t =
  let start = position_of_ast_pos l.start_pos in
  let end_  = position_of_ast_pos l.end_pos in
  { T.Range.start; end_ }

let location_of_loc ~(uri:T.DocumentUri.t) (l : Ast.Loc.t) : T.Location.t =
  { T.Location.uri; range = range_of_loc l }

let diagnostic ?severity ?source ~(message:string) (l : Ast.Loc.t) : T.Diagnostic.t =
  {
    T.Diagnostic.range = range_of_loc l;
    severity;
    code = None;
    codeDescription = None;
    source;
    message = `String message;
    tags = None;
    relatedInformation = None;
    data = None;
  }

let offset_of_lsp_position (idx : Text_index.t) (p : T.Position.t) : int option =
  Text_index.offset_of_line_col idx ~line:p.line ~col:p.character

let lsp_position_of_offset (idx : Text_index.t) (off : int) : T.Position.t =
  let (line, col) = Text_index.line_col_of_offset idx off in
  { T.Position.line; character = col }
