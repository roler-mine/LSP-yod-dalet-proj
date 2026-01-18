(* server/lib/lsp_convert.ml *)

module T = Lsp.Types

(* NOTE:
   LSP "character" is defined in UTF-16 code units.
   This implementation uses byte offsets (pos_cnum - pos_bol),
   which is correct for ASCII and “good enough” for a first pass.
   We can upgrade later if you want full Unicode correctness. *)

let position_of_lex (p : Lexing.position) : T.Position.t =
  let line = max 0 (p.pos_lnum - 1) in
  let character = max 0 (p.pos_cnum - p.pos_bol) in
  T.Position.create ~line ~character

let range_of_lex (start_p : Lexing.position) (end_p : Lexing.position) : T.Range.t =
  T.Range.create ~start:(position_of_lex start_p) ~end_:(position_of_lex end_p)

let range_of_lexbuf (lexbuf : Lexing.lexbuf) : T.Range.t =
  range_of_lex lexbuf.lex_start_p lexbuf.lex_curr_p

let new_line (lexbuf : Lexing.lexbuf) : unit =
  (* Call this in the lexer when you consume '\n' so pos_lnum/pos_bol stay accurate. *)
  Lexing.new_line lexbuf
