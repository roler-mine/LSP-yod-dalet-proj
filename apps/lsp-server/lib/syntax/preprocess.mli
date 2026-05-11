module T = Lsp.Types

type import_kind = Compool

type import = {
  kind : import_kind;
  name : string; (* normalized uppercase name *)
  loc : Ast.Loc.t; (* location for diagnostics *)
}

type define = {
  name : string; (* source spelling *)
  key : string; (* normalized uppercase name *)
  formals : string list; (* normalized uppercase formal names *)
  requires_call : bool; (* true for DEFINE NAME(...) *)
  body : string; (* define replacement body *)
  loc : Ast.Loc.t; (* location of define name *)
  decl_start_off : int; (* offset of DEFINE token in source *)
}

type source_span = {
  source_start_off : int;
  source_end_off : int;
  source_loc : Ast.Loc.t;
}

type expansion_origin =
  | Original of source_span
  | MacroExpansion of {
      macro_name : string;
      macro_decl : Ast.Loc.t;
      call_site : Ast.Loc.t;
      original_tokens : source_span list;
    }

type expansion_segment = {
  generated_start_off : int;
  generated_end_off : int;
  origin : expansion_origin;
}

type result = {
  text : string; (* preprocessed text after DEFINE expansion *)
  imports : import list; (* COMPOOL/ICOMPOOL/!COMPOOL extracted *)
  compool_def : string option; (* START COMPOOL NAME; found in file *)
  defines : define list; (* DEFINE declarations in source order *)
  source_map : expansion_segment list;
  diags : T.Diagnostic.t list; (* preprocess diagnostics only *)
}

type lex_tok = Parser.token_span

val lex_all_tokens : file:string option -> text:string -> lex_tok array
val lex_all_tokens_with_lexemes : file:string option -> text:string -> lex_tok array
val lex_all_tokens_from_offset :
  file:string option ->
  text:string ->
  start_off:int ->
  start_line:int ->
  start_col:int ->
  lex_tok array

val run_from_tokens :
  file:string option -> text:string -> tokens:lex_tok array -> result * bool

val run : file:string option -> text:string -> result

val loc_through_source_map : expansion_segment list -> Ast.Loc.t -> Ast.Loc.t

val diagnostic_through_source_map :
  generated_text:string -> expansion_segment list -> T.Diagnostic.t -> T.Diagnostic.t

val diagnostics_through_source_map :
  generated_text:string ->
  expansion_segment list ->
  T.Diagnostic.t list ->
  T.Diagnostic.t list

(* used by workspace scanning without needing full run() *)
val scan_compool_def : text:string -> string option
