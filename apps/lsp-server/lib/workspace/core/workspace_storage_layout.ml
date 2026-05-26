(* Module overview: Centralizes on-disk workspace storage paths for LSP indexes. *)

let workspace_dir_name = ".jovial_ls"

let workspace_dir ~(root : string) : string =
  Filename.concat root workspace_dir_name

let cache_dir ~(root : string) : string =
  Filename.concat (workspace_dir ~root) "cache"

let index_dir ~(root : string) : string =
  Filename.concat (workspace_dir ~root) "index"
