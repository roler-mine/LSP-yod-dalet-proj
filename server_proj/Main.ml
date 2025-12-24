let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "usage: jovial_parse <file.jov>";
    exit 2
  );

  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  try
    let ast_loc = parser.compilation_unit lexer.token lexbuf in
    let (sp, ep) = ast_loc.Ast.span in
    Printf.printf "OK: parsed. span=%d:%d -> %d:%d\n"
      sp.pos_lnum (sp.pos_cnum - sp.pos_bol + 1)
      ep.pos_lnum (ep.pos_cnum - ep.pos_bol + 1)
  with
  | lexer.Internal_lexer_bug msg ->
      prerr_endline ("internal lexer bug: " ^ msg);
      exit 1
  | e ->
      prerr_endline ("error: " ^ Printexc.to_string e);
      exit 1
