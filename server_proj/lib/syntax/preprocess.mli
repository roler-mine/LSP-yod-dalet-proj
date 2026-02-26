module T = Lsp.Types

type import_kind = Compool

type import = {
  kind : import_kind;
  name : string;     (* normalized uppercase name *)
  loc  : Ast.Loc.t;  (* location for diagnostics *)
}

type define = {
  name : string;           (* source spelling *)
  key : string;            (* normalized uppercase name *)
  formals : string list;   (* normalized uppercase formal names *)
  requires_call : bool;    (* true for DEFINE NAME(...) *)
  body : string;           (* define replacement body *)
  loc : Ast.Loc.t;         (* location of define name *)
  decl_start_off : int;    (* offset of DEFINE token in source *)
}

type result = {
  text : string;                 (* preprocessed text after DEFINE expansion *)
  imports : import list;         (* COMPOOL/ICOMPOOL/!COMPOOL extracted *)
  compool_def : string option;   (* START COMPOOL NAME; found in file *)
  defines : define list;         (* DEFINE declarations in source order *)
  diags : T.Diagnostic.t list;   (* preprocess diagnostics only *)
}

val run : file:string option -> text:string -> result

(* used by workspace scanning without needing full run() *)
val scan_compool_def : text:string -> string option
