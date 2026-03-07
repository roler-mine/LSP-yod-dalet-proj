module T = Lsp.Types

type t

val create : unit -> t
val reset : t -> unit
val global_rev : t -> int
val upsert_snapshot : t -> Doc_snapshot.t -> unit
val remove_uri : t -> uri:T.DocumentUri.t -> unit
val snapshot_for_uri : t -> uri:T.DocumentUri.t -> Doc_snapshot.t option
val iter_snapshots : t -> (Doc_snapshot.t -> unit) -> unit

val resolve_symbol_at :
  t -> uri:T.DocumentUri.t -> pos:T.Position.t -> string option

val defs_for_sym_id : t -> string -> Doc_snapshot.nav_def list
val refs_for_sym_id : t -> string -> Doc_snapshot.nav_occ list
val sym_ids_for_key : t -> key:string -> string list
val symbols_for_prefix : t -> prefix:string -> string list
val uris_for_path_key : t -> path_key:string -> T.DocumentUri.t list
val uris_importing_compool : t -> compool_key:string -> T.DocumentUri.t list

val invalidate_path_and_dependents :
  t -> path_key:string -> T.DocumentUri.t list
