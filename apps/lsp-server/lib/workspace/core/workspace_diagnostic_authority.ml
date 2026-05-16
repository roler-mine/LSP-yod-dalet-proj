(* Module overview: Tracks whether diagnostics are provisional or authoritative for each document. *)

module T = Lsp.Types

type t =
  | ParseAuthoritative
  | LocalSemanticAuthoritative
  | CrossModuleProvisional
  | CrossModuleAuthoritative
  | SkeletonOnly

type diagnostic = {
  lsp : T.Diagnostic.t;
  authority : t;
  readiness : Workspace_readiness.t;
}

let label = function
  | ParseAuthoritative -> "parseAuthoritative"
  | LocalSemanticAuthoritative -> "localSemanticAuthoritative"
  | CrossModuleProvisional -> "crossModuleProvisional"
  | CrossModuleAuthoritative -> "crossModuleAuthoritative"
  | SkeletonOnly -> "skeletonOnly"

let is_authoritative = function
  | ParseAuthoritative | LocalSemanticAuthoritative | CrossModuleAuthoritative ->
      true
  | CrossModuleProvisional | SkeletonOnly -> false

let make ~authority ~readiness lsp = { lsp; authority; readiness }

let parse lsp =
  make ~authority:ParseAuthoritative ~readiness:Workspace_readiness.LocalAstReady
    lsp

let local_semantic lsp =
  make ~authority:LocalSemanticAuthoritative
    ~readiness:Workspace_readiness.LocalAstReady lsp

let cross_module_authoritative lsp =
  make ~authority:CrossModuleAuthoritative
    ~readiness:Workspace_readiness.CrossModuleSemanticReady lsp

let cross_module_provisional lsp =
  make ~authority:CrossModuleProvisional
    ~readiness:Workspace_readiness.WorkspaceSemanticReady lsp

let skeleton_only lsp =
  make ~authority:SkeletonOnly ~readiness:Workspace_readiness.SkeletonReady lsp

let provisional_warmup_message =
  "Cross-module semantic index is still warming up."

let message_to_string
    (message : [ `String of string | `MarkupContent of T.MarkupContent.t ]) =
  match message with
  | `String s -> s
  | `MarkupContent mc -> mc.value

let with_message_suffix ~(suffix : string) (lsp : T.Diagnostic.t) :
    T.Diagnostic.t =
  let text = message_to_string lsp.message in
  let message =
    if suffix = "" || String.ends_with ~suffix text then text
    else text ^ " " ^ suffix
  in
  { lsp with T.Diagnostic.message = `String message }

let soften_for_warmup (lsp : T.Diagnostic.t) : T.Diagnostic.t =
  let lsp = with_message_suffix ~suffix:provisional_warmup_message lsp in
  { lsp with T.Diagnostic.severity = Some T.DiagnosticSeverity.Information }

let to_lsp (diagnostic : diagnostic) : T.Diagnostic.t option =
  match diagnostic.authority with
  | CrossModuleProvisional | SkeletonOnly -> None
  | ParseAuthoritative | LocalSemanticAuthoritative | CrossModuleAuthoritative ->
      Some diagnostic.lsp

let to_lsp_list diagnostics = List.filter_map to_lsp diagnostics

let severity_json (diagnostic : T.Diagnostic.t) =
  match diagnostic.severity with
  | None -> `Null
  | Some T.DiagnosticSeverity.Error -> `String "error"
  | Some T.DiagnosticSeverity.Warning -> `String "warning"
  | Some T.DiagnosticSeverity.Information -> `String "information"
  | Some T.DiagnosticSeverity.Hint -> `String "hint"

let to_debug_json (diagnostic : diagnostic) : Yojson.Safe.t =
  `Assoc
    [
      ("authority", `String (label diagnostic.authority));
      ("authoritative", `Bool (is_authoritative diagnostic.authority));
      ("readiness", `String (Workspace_readiness.label diagnostic.readiness));
      ("severity", severity_json diagnostic.lsp);
      ("message", `String (message_to_string diagnostic.lsp.message));
    ]
