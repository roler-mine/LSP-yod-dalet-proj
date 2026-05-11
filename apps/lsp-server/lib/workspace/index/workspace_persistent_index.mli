type file_fingerprint = {
  uri : string;
  size : int;
  mtime_ns : int64;
  content_hash : string option;
  parser_version : string;
  indexer_version : string;
}

val parser_version : string
val indexer_version : string
val cache_dir : root:string -> string
val files_json_path : root:string -> string
val symbols_json_path : root:string -> string
val refs_json_path : root:string -> string
val scopes_json_path : root:string -> string
val deps_json_path : root:string -> string
val macros_json_path : root:string -> string
val diagnostics_json_path : root:string -> string
val fingerprint_path : string -> file_fingerprint option

val load_workspace_index :
  source_extensions:string list -> root:string -> Workspace_index.t option

val save_workspace_index : root:string -> Workspace_index.t -> unit
val save_snapshot_index : root:string -> Workspace_snapshot.snapshot -> unit
