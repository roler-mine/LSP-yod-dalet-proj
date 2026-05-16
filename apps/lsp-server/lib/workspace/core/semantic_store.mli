(** Module overview: Mutable semantic store backing hover, navigation, and LSIF symbol metadata. *)

module T = Lsp.Types

type t

module Snapshot : sig
  type nav_def = {
    sym_id : string;
    uri : T.DocumentUri.t;
    name : string;
    key : string;
    loc : Ast.Loc.t;
    kind : int;
    container : string option;
    metadata : Workspace_symbol_metadata.jovial_symbol_metadata;
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
    mutable semantic_lex_tokens : token list option;
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

  val set_semantic_lex_tokens : t -> token list -> unit
  val set_semantic_tokens_full : t -> token list -> unit
end

val create : unit -> t
val reset : t -> unit
val global_rev : t -> int
val upsert_snapshot : t -> Snapshot.t -> unit
val remove_uri : t -> uri:T.DocumentUri.t -> unit
val snapshot_for_uri : t -> uri:T.DocumentUri.t -> Snapshot.t option
val iter_snapshots : t -> (Snapshot.t -> unit) -> unit

val resolve_symbol_at :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> string option

val defs_for_sym_id : t -> string -> Snapshot.nav_def list
val defs_for_key_kind : t -> key:string -> kind:int -> Snapshot.nav_def list
val defs_for_key_in_uri :
  t -> uri:T.DocumentUri.t -> key:string -> Snapshot.nav_def list
val refs_for_sym_id : t -> string -> Snapshot.nav_occ list
val sym_ids_for_key : t -> key:string -> string list
val symbols_for_prefix : t -> prefix:string -> string list
val uris_for_path_key : t -> path_key:string -> T.DocumentUri.t list
val uris_importing_compool : t -> compool_key:string -> T.DocumentUri.t list

val invalidate_path_and_dependents :
  t -> path_key:string -> T.DocumentUri.t list
