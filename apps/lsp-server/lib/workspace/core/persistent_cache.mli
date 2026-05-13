type file_metadata = {
  path : string;
  path_key : string;
  uri : string;
  size : int;
  mtime_ns : int64;
  content_hash : string option;
  source_extensions : string list;
  parser_version : string;
  indexer_version : string;
  schema_version : int;
}

type source_index_load = {
  index : Workspace_index.t;
  loaded_from_cache : bool;
  changed_paths : string list;
  pruned_paths : string list;
}

type skeleton_cache
type module_summary_cache

val schema_version : int
val parser_version : string
val indexer_version : string
val cache_dir : root:string -> string
val cache_version_json_path : root:string -> string
val source_index_json_path : root:string -> string
val skeleton_index_json_path : root:string -> string
val module_summary_json_path : root:string -> string
val fingerprint_path : string -> file_metadata option

val load_or_build_source_index :
  root:string ->
  source_extensions:string list ->
  paths:string list ->
  source_index_load

val save_source_index :
  root:string ->
  source_extensions:string list ->
  Workspace_index.t ->
  unit

val load_skeleton_cache :
  root:string ->
  source_extensions:string list ->
  max_bytes:int ->
  paths:string list ->
  skeleton_cache

val skeleton_entries :
  skeleton_cache -> path:string -> Workspace_foundation.quick_nav_entry list option

val save_skeleton_entry :
  root:string ->
  source_extensions:string list ->
  max_bytes:int ->
  path:string ->
  entries:Workspace_foundation.quick_nav_entry list ->
  unit

val load_module_summary_cache :
  root:string ->
  source_extensions:string list ->
  paths:string list ->
  module_summary_cache

val module_summary_entries :
  module_summary_cache -> Workspace_foundation.module_summary_cache_entry list

val save_module_summary_entry :
  root:string ->
  source_extensions:string list ->
  path:string ->
  summary:Module_summary.t ->
  unit
