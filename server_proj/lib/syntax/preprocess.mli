module T = Lsp.Types

type import_kind = Compool

type import = {
  kind : import_kind;
  name : string;     (* normalized uppercase name *)
  loc  : Ast.Loc.t;  (* location for diagnostics *)
}

type result = {
  text : string;                 (* for now: identical to input *)
  imports : import list;         (* COMPOOL/ICOMPOOL/!COMPOOL extracted *)
  compool_def : string option;   (* START COMPOOL NAME; found in file *)
  diags : T.Diagnostic.t list;   (* preprocess diagnostics only *)
}

val run : file:string option -> text:string -> result

(* used by workspace scanning without needing full run() *)
val scan_compool_def : text:string -> string option
