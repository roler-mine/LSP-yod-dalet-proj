module T = Lsp.Types

type nav_def = {
  sym_id : string;
  uri : T.DocumentUri.t;
  name : string;
  key : string;
  loc : Ast.Loc.t;
  kind : int;
  container : string option;
}

type nav_occ = T.DocumentUri.t * Ast.Loc.t
type token = { line : int; start : int; len : int; typ : int; mods : int }

type t = {
  uri : T.DocumentUri.t;
  path_key : string option;
  doc_rev : int;
  text_hash : string;
  imports : Preprocess.import list;
  defines : Preprocess.define list;
  compool_def : string option;
  nav_defs : (string * nav_def) list;
  nav_occs : (string * nav_occ list) list;
  proc_param_map : (string * string list) list;
  mutable doc_symbols : Yojson.Safe.t list option;
  mutable semantic_tokens_full : token list option;
  symbol_keys_touched : string list;
}

val build :
  uri:T.DocumentUri.t ->
  path_key:string option ->
  doc_rev:int ->
  text:string ->
  imports:Preprocess.import list ->
  defines:Preprocess.define list ->
  compool_def:string option ->
  nav_defs:(string * nav_def) list ->
  nav_occs:(string * nav_occ list) list ->
  proc_param_map:(string * string list) list ->
  symbol_keys_touched:string list ->
  t

val set_doc_symbols : t -> Yojson.Safe.t list -> unit
val set_semantic_tokens_full : t -> token list -> unit
