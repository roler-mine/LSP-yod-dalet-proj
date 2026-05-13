type t =
  | ParseAuthoritative
  | LocalSemanticAuthoritative
  | CrossModuleProvisional
  | CrossModuleAuthoritative
  | SkeletonOnly

type diagnostic = {
  lsp : Lsp.Types.Diagnostic.t;
  authority : t;
  readiness : Workspace_readiness.t;
}

val label : t -> string
val is_authoritative : t -> bool
val make : authority:t -> readiness:Workspace_readiness.t -> Lsp.Types.Diagnostic.t -> diagnostic
val parse : Lsp.Types.Diagnostic.t -> diagnostic
val local_semantic : Lsp.Types.Diagnostic.t -> diagnostic
val cross_module_authoritative : Lsp.Types.Diagnostic.t -> diagnostic
val cross_module_provisional : Lsp.Types.Diagnostic.t -> diagnostic
val skeleton_only : Lsp.Types.Diagnostic.t -> diagnostic
val provisional_warmup_message : string
val with_message_suffix : suffix:string -> Lsp.Types.Diagnostic.t -> Lsp.Types.Diagnostic.t
val soften_for_warmup : Lsp.Types.Diagnostic.t -> Lsp.Types.Diagnostic.t
val to_lsp : diagnostic -> Lsp.Types.Diagnostic.t option
val to_lsp_list : diagnostic list -> Lsp.Types.Diagnostic.t list
val to_debug_json : diagnostic -> Yojson.Safe.t
