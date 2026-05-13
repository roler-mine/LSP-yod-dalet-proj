(* Compatibility facade for historical callers.

   New workspace or LSP logic should call Workspace_query when it needs the
   readiness-aware definition/references/hover surface, or the focused
   Workspace_* feature module that owns the requested capability. The
   workspace_core_suite_test architecture check keeps internal modules from
   accumulating new dependencies on this facade.
*)

let definition_locations_for = Workspace_definition.definition_locations_for
let declaration_locations_for = Workspace_definition.declaration_locations_for

let type_definition_locations_for =
  Workspace_type_definition.type_definition_locations_for

let implementation_locations_for =
  Workspace_implementation.implementation_locations_for

let references_locations_for = Workspace_references.references_locations_for
let references_locations_stream = Workspace_references.references_locations_stream
let hover_for = Workspace_hover.hover_for
let prepare_rename_for = Workspace_rename.prepare_rename_for
let rename_for = Workspace_rename.rename_for
let completion_items_for = Workspace_completion.completion_items_for
let workspace_symbols_for = Workspace_symbols.workspace_symbols_for
let workspace_symbols_stream = Workspace_symbols.workspace_symbols_stream
let signature_help_for = Workspace_signature_help.signature_help_for
let code_actions_for = Workspace_code_actions.code_actions_for
