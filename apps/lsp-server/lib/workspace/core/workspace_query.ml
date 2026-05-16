(* Module overview: High-level query API for hover, definition, references, AST/CST dumps, and debug output. *)

module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_nav_model
open Workspace_nav_lookup

module R = Workspace_readiness
module Perf_stats = Workspace_foundation.Perf_stats
module Metadata = Workspace_symbol_metadata

type query_context = {
  ws : t;
  doc : Document.t;
  pos : T.Position.t;
}

type symbol_ref = {
  symbol_id : string option;
  name : string;
  key : string;
  loc : Ast.Loc.t;
  def : def option;
  readiness : R.t;
  authority : R.authority;
}

type 'a query_result = 'a R.result
type definition_result = T.Location.t list query_result
type references_result = T.Location.t list query_result
type hover_result = T.Hover.t option query_result

let context (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    query_context option =
  match Hashtbl.find_opt ws.docs uri with
  | None -> None
  | Some doc -> Some { ws; doc; pos }

let startup_source_shape_looks_large (ws : t) : bool =
  List.length ws.source_file_paths >= 320
  || ws.source_bytes_estimate_count >= 320
  || Option.value ws.source_bytes_estimate ~default:0 >= (64 * 1024 * 1024)

let has_parse_skipped_diag (doc : Document.t) : bool =
  List.exists
    (fun (diag : T.Diagnostic.t) ->
      match diag.message with
      | `String s -> String.starts_with ~prefix:"File parse skipped" s
      | `MarkupContent mc ->
          String.starts_with ~prefix:"File parse skipped" mc.value)
    doc.Document.parse_diags

let readiness_for_doc (ws : t) (doc : Document.t) : R.t * R.reason list =
  let reasons = ref [] in
  if request_cancelled ws then reasons := R.Cancelled :: !reasons;
  if doc.Document.parse_rev <> doc.Document.rev then (
    reasons := R.ParseStale :: !reasons;
    (R.LexicalOnly, List.rev !reasons))
  else if has_parse_skipped_diag doc then (
    reasons := R.ParseSkippedLargeFile :: !reasons;
    (R.LexicalOnly, List.rev !reasons))
  else
    match (doc.Document.ast, doc.Document.syntax) with
    | Some _, _ ->
        let readiness =
          if ws.startup_fully_nav_ready_ms <> None then R.WorkspaceSemanticReady
          else R.LocalAstReady
        in
        if ws.startup_fully_nav_ready_ms = None then
          reasons := R.WorkspaceIndexWarming :: !reasons;
        (readiness, List.rev !reasons)
    | None, Some _ ->
        reasons := R.WorkspaceIndexWarming :: !reasons;
        (R.SkeletonReady, List.rev !reasons)
    | None, None ->
        reasons := R.SemanticStoreMiss :: !reasons;
        (R.LexicalOnly, List.rev !reasons)

let doc_has_current_local_ast (doc : Document.t) : bool =
  doc.Document.parse_rev = doc.Document.rev && doc.Document.ast <> None
  && not (has_parse_skipped_diag doc)

let doc_has_cached_nav_snapshot (ws : t) (doc : Document.t) : bool =
  ws.sem_store_enabled
  &&
  match Semantic_store.snapshot_for_uri ws.semantic_store ~uri:doc.Document.uri with
  | Some snap -> snap.Semantic_store.Snapshot.doc_rev = doc.Document.parse_rev
  | None -> false

let startup_large_workspace_skeletal (ws : t) (doc : Document.t) : bool =
  (not (quick_nav_index_ready_for_startup ws))
  && (not (doc_has_current_local_ast doc || doc_has_cached_nav_snapshot ws doc))
  && (startup_source_shape_looks_large ws
     ||
     match workspace_profile_for_budget ws with
     | ProfileLarge -> true
     | ProfileSmall | ProfileMedium -> false)

let startup_large_doc_semantic_deferred (ws : t) (doc : Document.t) : bool =
  (* Large open files can have a current AST before workspace navigation is
     ready. During startup, keep editor requests on skeleton/summary data and
     leave macro/nav construction to the background path. *)
  ws.startup_fully_nav_ready_ms = None
  && String.length doc.Document.text >= ws.full_semantic_tokens_max_bytes
  && (not (quick_nav_index_complete ws))
  &&
  match workspace_profile_for_budget ws with
  | ProfileLarge -> true
  | ProfileSmall | ProfileMedium -> startup_source_shape_looks_large ws

let fallback_readiness_for_doc (doc : Document.t) : R.t =
  if doc.Document.syntax <> None || doc.Document.ast <> None then R.SkeletonReady
  else R.LexicalOnly

let append_reason reason reasons =
  if List.exists (( = ) reason) reasons then reasons else reasons @ [ reason ]

let is_blocking_reason = function
  | R.ParseStale | R.ParseSkippedLargeFile | R.Cancelled
  | R.SoftBudgetExceeded | R.MemoryPressure ->
      true
  | R.WorkspaceIndexWarming | R.CrossModuleIndexWarming | R.SemanticStoreMiss
  | R.MacroExpansionUnavailable | R.UnknownReason _ ->
      false

let doc_blocked_metadata ctx =
  let readiness, reasons = readiness_for_doc ctx.ws ctx.doc in
  if List.exists is_blocking_reason reasons then
    Some (readiness, R.Provisional, reasons)
  else None

let authority_for readiness reasons =
  if R.compare readiness R.LocalAstReady >= 0 && reasons = [] then
    R.Authoritative
  else R.Provisional

let make_result ?readiness ?authority ?(reasons = []) ctx value =
  let readiness, base_reasons =
    match readiness with
    | Some readiness -> (readiness, reasons)
    | None -> readiness_for_doc ctx.ws ctx.doc
  in
  let reasons =
    if request_cancelled ctx.ws then append_reason R.Cancelled base_reasons
    else base_reasons
  in
  let authority =
    match authority with
    | Some authority -> authority
    | None -> authority_for readiness reasons
  in
  { R.value = value; readiness; authority; reasons }

let result_for_fallback_word ctx value =
  let readiness = fallback_readiness_for_doc ctx.doc in
  let _, doc_reasons = readiness_for_doc ctx.ws ctx.doc in
  let reasons = append_reason R.SemanticStoreMiss doc_reasons in
  make_result ctx ~readiness ~authority:R.Provisional ~reasons value

let result_for_unresolved_ast ctx value =
  let readiness =
    if doc_has_current_local_ast ctx.doc then R.LocalAstReady
    else fallback_readiness_for_doc ctx.doc
  in
  let _, doc_reasons = readiness_for_doc ctx.ws ctx.doc in
  let reasons = append_reason R.SemanticStoreMiss doc_reasons in
  make_result ctx ~readiness ~authority:R.Provisional ~reasons value

let semantic_snapshot_current_for_uri (ws : t) ~(uri : T.DocumentUri.t) : bool =
  match Semantic_store.snapshot_for_uri ws.semantic_store ~uri with
  | None -> false
  | Some snap -> (
      match Hashtbl.find_opt ws.docs uri with
      | Some doc -> snap.Semantic_store.Snapshot.doc_rev = doc.Document.parse_rev
      | None -> true)

let doc_for_location_uri (ws : t) (uri : T.DocumentUri.t) : Document.t option =
  match Hashtbl.find_opt ws.docs uri with
  | Some _ as hit -> hit
  | None -> (
      match Uri_path.file_path_of_uri uri with
      | None -> None
      | Some path ->
          Hashtbl.find_opt ws.files (normalize_path_key path))

let cross_module_location_authoritative (ws : t) (loc : T.Location.t) : bool =
  if semantic_snapshot_current_for_uri ws ~uri:loc.uri then true
  else
    match doc_for_location_uri ws loc.uri with
    | Some doc -> doc_has_current_local_ast doc
    | None -> false

let record_cross_module_authority (authority : R.authority) : unit =
  match authority with
  | R.Authoritative -> Perf_stats.tick "query.cross_module.authoritative_result"
  | R.Provisional -> Perf_stats.tick "query.cross_module.provisional_result"

let metadata_for_locations ctx (locations : T.Location.t list) =
  let doc_readiness, doc_reasons = readiness_for_doc ctx.ws ctx.doc in
  if List.exists is_blocking_reason doc_reasons then
    (doc_readiness, R.Provisional, doc_reasons)
  else
    let imported_lookup = import_under_cursor ctx.doc ctx.pos <> None in
    let cross_file =
      imported_lookup
      || List.exists
           (fun (loc : T.Location.t) ->
             not (same_uri loc.uri ctx.doc.Document.uri))
           locations
    in
    if cross_file then
      let targets_authoritative =
        locations <> []
        && List.for_all
             (fun (loc : T.Location.t) ->
               same_uri loc.uri ctx.doc.Document.uri
               || cross_module_location_authoritative ctx.ws loc)
             locations
      in
      if
        ctx.ws.startup_fully_nav_ready_ms <> None && targets_authoritative
      then (
        record_cross_module_authority R.Authoritative;
        (R.CrossModuleSemanticReady, R.Authoritative, []))
      else (
        record_cross_module_authority R.Provisional;
        ( R.WorkspaceSemanticReady,
          R.Provisional,
          [ R.CrossModuleIndexWarming ] ))
    else if locations = [] then
      let reasons =
        if doc_reasons = [] then [ R.SemanticStoreMiss ] else doc_reasons
      in
      let readiness =
        if doc_has_current_local_ast ctx.doc then R.LocalAstReady
        else fallback_readiness_for_doc ctx.doc
      in
      (readiness, R.Provisional, reasons)
    else if doc_has_current_local_ast ctx.doc then
      (R.LocalAstReady, R.Authoritative, [])
    else (fallback_readiness_for_doc ctx.doc, R.Provisional, doc_reasons)

let metric_fragment s =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'A' .. 'Z' as c ->
        Bytes.set b i (Char.lowercase_ascii c)
    | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> ()
    | _ -> Bytes.set b i '_'
  done;
  Bytes.unsafe_to_string b

let record_result name (r : 'a query_result) : 'a query_result =
  Perf_stats.tick ("query." ^ name);
  Perf_stats.tick ("query.readiness." ^ metric_fragment (R.label r.readiness));
  Perf_stats.tick
    ("query.authority." ^ metric_fragment (R.authority_label r.authority));
  List.iter
    (fun reason ->
      Perf_stats.tick
        ("query.reason." ^ metric_fragment (R.reason_label reason)))
    r.reasons;
  if List.exists (( = ) R.Cancelled) r.reasons then
    Perf_stats.tick "query.cancelled_ready";
  r

let perf_metric_calls (name : string) : int =
  match Perf_stats.snapshot_json () with
  | `Assoc fields -> (
      match List.assoc_opt "metrics" fields with
      | Some (`List metrics) ->
          metrics
          |> List.find_map (function
               | `Assoc metric_fields -> (
                   match
                     ( List.assoc_opt "name" metric_fields,
                       List.assoc_opt "calls" metric_fields )
                   with
                   | Some (`String n), Some (`Int calls) when n = name ->
                       Some calls
                   | _ -> None)
               | _ -> None)
          |> Option.value ~default:0
      | _ -> 0)
  | _ -> 0

let lookup_symbol_ref ctx : symbol_ref option query_result =
  let cache = Hashtbl.create 8 in
  let nav = nav_for_doc_cached ctx.ws cache ctx.doc in
  match symbol_at_position_in_nav nav ~uri:ctx.doc.Document.uri ~pos:ctx.pos with
  | Some (sym_id, loc) ->
      let def =
        match Hashtbl.find_opt nav.defs_by_id sym_id with
        | Some _ as hit -> hit
        | None -> (
            match Semantic_store.defs_for_sym_id ctx.ws.semantic_store sym_id with
            | snap :: _ -> Some (def_of_snapshot_def snap)
            | [] -> None)
      in
      let name, key =
        match def with
        | Some d -> (d.name, d.key)
        | None -> (
            match nav_word_at_position ctx.doc ctx.pos with
            | Some (name, _) -> (name, normalize_name name)
            | None -> ("", ""))
      in
      let readiness, authority, reasons =
        match doc_blocked_metadata ctx with
        | Some metadata -> metadata
        | None -> (
            match def with
            | Some d when same_uri d.uri ctx.doc.Document.uri ->
                (R.LocalAstReady, R.Authoritative, [])
            | Some _ when ctx.ws.startup_fully_nav_ready_ms <> None ->
                (R.CrossModuleSemanticReady, R.Authoritative, [])
            | Some _ ->
                ( R.WorkspaceSemanticReady,
                  R.Provisional,
                  [ R.CrossModuleIndexWarming ] )
            | None when doc_has_current_local_ast ctx.doc ->
                (R.LocalAstReady, R.Provisional, [ R.SemanticStoreMiss ])
            | None ->
                ( fallback_readiness_for_doc ctx.doc,
                  R.Provisional,
                  [ R.SemanticStoreMiss ] ))
      in
      let value =
        Some
          { symbol_id = Some sym_id; name; key; loc; def; readiness; authority }
      in
      make_result ctx ~readiness ~authority ~reasons value
  | None -> (
      match nav_word_at_position ctx.doc ctx.pos with
      | None -> result_for_unresolved_ast ctx None
      | Some (name, loc) ->
          let key = normalize_name name in
          let readiness = fallback_readiness_for_doc ctx.doc in
          let authority = R.Provisional in
          let value =
            Some
              { symbol_id = None; name; key; loc; def = None; readiness; authority }
          in
          result_for_fallback_word ctx value)

let symbol_at_position ctx =
  lookup_symbol_ref ctx |> record_result "symbol_at_position"

let hover_target_at_position ctx =
  symbol_at_position ctx |> record_result "hover_target_at_position"

let definition_at_position ctx =
  let summary_hits_before = perf_metric_calls "query.cross_module.summary_hit" in
  let value =
    Perf_stats.time "query.definition_core_ms" (fun () ->
        Workspace_definition.definition_locations_for ctx.ws
          ~uri:ctx.doc.Document.uri ~pos:ctx.pos)
  in
  let summary_backed =
    perf_metric_calls "query.cross_module.summary_hit" > summary_hits_before
  in
  let readiness, authority, reasons =
    Perf_stats.time "query.definition_metadata_ms" (fun () ->
        metadata_for_locations ctx value)
  in
  let cross_file =
    List.exists
      (fun (loc : T.Location.t) -> not (same_uri loc.uri ctx.doc.Document.uri))
      value
  in
  let readiness, authority, reasons =
    if summary_backed && cross_file then
      let reasons =
        if List.mem R.CrossModuleIndexWarming reasons then reasons
        else R.CrossModuleIndexWarming :: reasons
      in
      (R.WorkspaceSemanticReady, R.Provisional, reasons)
    else (readiness, authority, reasons)
  in
  make_result ctx ~readiness ~authority ~reasons value |> record_result "definition"

let references_at_position ~(include_declaration : bool) ctx =
  let value =
    Workspace_references.references_locations_for ctx.ws
      ~uri:ctx.doc.Document.uri ~pos:ctx.pos
      ~include_decl:include_declaration
  in
  let readiness, authority, reasons = metadata_for_locations ctx value in
  make_result ctx ~readiness ~authority ~reasons value |> record_result "references"

let hover_at_position ctx =
  let value =
    Workspace_hover.hover_for ctx.ws ~uri:ctx.doc.Document.uri ~pos:ctx.pos
  in
  let readiness, authority, reasons =
    match value with
    | Some _ ->
        let readiness, reasons = readiness_for_doc ctx.ws ctx.doc in
        let readiness = R.max readiness R.LocalAstReady in
        (readiness, authority_for readiness reasons, reasons)
    | None -> (
        match hover_target_at_position ctx with
        | { R.value = Some _; readiness; authority; reasons } ->
            (readiness, authority, reasons)
        | { readiness; authority; reasons; _ } -> (readiness, authority, reasons))
  in
  make_result ctx ~readiness ~authority ~reasons value |> record_result "hover"

let none_result (ws : t) value =
  let readiness = R.LexicalOnly in
  let reasons =
    if request_cancelled ws then [ R.Cancelled ] else [ R.SemanticStoreMiss ]
  in
  { R.value = value; readiness; authority = R.Provisional; reasons }

let startup_skeletal_result ctx value =
  let readiness = fallback_readiness_for_doc ctx.doc in
  make_result ctx ~readiness ~authority:R.Provisional
    ~reasons:[ R.WorkspaceIndexWarming ] value

let definition_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  match context ws ~uri ~pos with
  | None -> (none_result ws [] |> record_result "definition").value
  | Some ctx when startup_large_doc_semantic_deferred ws ctx.doc ->
      (startup_skeletal_result ctx [] |> record_result "definition").value
  | Some ctx when startup_large_workspace_skeletal ws ctx.doc ->
      (startup_skeletal_result ctx [] |> record_result "definition").value
  | Some ctx -> (definition_at_position ctx).value

let references_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) ~(include_decl : bool) : T.Location.t list =
  match context ws ~uri ~pos with
  | None -> (none_result ws [] |> record_result "references").value
  | Some ctx when startup_large_doc_semantic_deferred ws ctx.doc ->
      ignore include_decl;
      (startup_skeletal_result ctx [] |> record_result "references").value
  | Some ctx when startup_large_workspace_skeletal ws ctx.doc ->
      ignore include_decl;
      (startup_skeletal_result ctx [] |> record_result "references").value
  | Some ctx ->
      (references_at_position ~include_declaration:include_decl ctx).value

let hover_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.Hover.t option =
  match context ws ~uri ~pos with
  | None -> (none_result ws None |> record_result "hover").value
  | Some ctx when startup_large_doc_semantic_deferred ws ctx.doc ->
      let value =
        Workspace_hover.hover_skeleton_fallback_for ws ctx.doc ~pos:ctx.pos
      in
      (startup_skeletal_result ctx value |> record_result "hover").value
  | Some ctx when startup_large_workspace_skeletal ws ctx.doc ->
      (startup_skeletal_result ctx None |> record_result "hover").value
  | Some ctx -> (hover_at_position ctx).value

let position_json (pos : T.Position.t) : Yojson.Safe.t =
  `Assoc [ ("line", `Int pos.line); ("character", `Int pos.character) ]

let readiness_json (result : 'a query_result) : (string * Yojson.Safe.t) list =
  [
    ("readiness", `String (R.label result.readiness));
    ("authority", `String (R.authority_label result.authority));
    ( "reasons",
      `List (List.map (fun reason -> `String (R.reason_label reason)) result.reasons)
    );
  ]

let def_json (d : def) : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String d.name);
      ("key", `String d.key);
      ("kind", `String (Metadata.symbol_kind_label d.metadata.jovial_kind));
      ("uri", `String (Uri_path.docuri_to_string d.uri));
      ("location", location_json ~uri:d.uri d.loc);
    ]

let symbol_ref_json ~(uri : T.DocumentUri.t) (sym : symbol_ref option) :
    Yojson.Safe.t =
  match sym with
  | None -> `Null
  | Some sym ->
      `Assoc
        [
          ("name", `String sym.name);
          ("key", `String sym.key);
          ( "symbolId",
            match sym.symbol_id with Some id -> `String id | None -> `Null );
          ("location", location_json ~uri sym.loc);
          ("readiness", `String (R.label sym.readiness));
          ("authority", `String (R.authority_label sym.authority));
          ( "definition",
            match sym.def with Some d -> def_json d | None -> `Null );
        ]

let target_definition_json (sym : symbol_ref option)
    (definitions : T.Location.t list) : Yojson.Safe.t =
  match sym with
  | Some { def = Some d; _ } -> def_json d
  | _ -> (
      match definitions with
      | loc :: _ -> T.Location.yojson_of_t loc
      | [] -> `Null)

let cache_source_for ctx (sym : symbol_ref option) definitions fallback_path_used =
  match sym with
  | Some { symbol_id = Some _; def = Some d; _ }
    when same_uri d.uri ctx.doc.Document.uri ->
      "document-navigation"
  | Some { symbol_id = Some _; _ } when ctx.ws.sem_store_enabled ->
      "semantic-store"
  | Some { symbol_id = None; _ } when fallback_path_used -> "fallback-word"
  | _ when definitions <> [] ->
      if List.exists
           (fun (loc : T.Location.t) -> not (same_uri loc.uri ctx.doc.Document.uri))
           definitions
      then "cross-module-index"
      else "document-navigation"
  | _ -> "none"

let explain_symbol_resolution_json (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : Yojson.Safe.t =
  match context ws ~uri ~pos with
  | None ->
      `Assoc
        [
          ("symbol", `Null);
          ("position", position_json pos);
          ("readiness", `String (R.label R.LexicalOnly));
          ("authority", `String (R.authority_label R.Provisional));
          ("reasons", `List [ `String (R.reason_label R.SemanticStoreMiss) ]);
          ("resolutionPath", `List [ `String "document: not open or not indexed" ]);
          ("fallbackScanUsed", `Bool false);
          ("fallbackPathUsed", `Bool false);
          ("cacheSource", `String "none");
          ("targetDefinition", `Null);
        ]
  | Some ctx ->
      let fallback_before =
        perf_metric_calls "query.cross_module.fallback_scan"
      in
      let symbol_result = symbol_at_position ctx in
      let definition_result = definition_at_position ctx in
      let fallback_after = perf_metric_calls "query.cross_module.fallback_scan" in
      let fallback_scan_used = fallback_after > fallback_before in
      let fallback_path_used =
        fallback_scan_used
        ||
        match symbol_result.value with
        | Some { symbol_id = None; _ } -> true
        | _ -> false
      in
      let result_for_metadata =
        if definition_result.value <> [] then definition_result
        else
          {
            R.value = [];
            readiness = symbol_result.readiness;
            authority = symbol_result.authority;
            reasons = symbol_result.reasons;
          }
      in
      let symbol_step =
        match symbol_result.value with
        | None -> "symbolAtPosition: unresolved"
        | Some sym ->
            Printf.sprintf "symbolAtPosition: %s (%s)" sym.name sym.key
      in
      let target_step =
        Printf.sprintf "definition: %d target%s"
          (List.length definition_result.value)
          (if List.length definition_result.value = 1 then "" else "s")
      in
      let fallback_step =
        if fallback_scan_used then "fallbackScan: used"
        else if fallback_path_used then "fallbackWord: used"
        else "fallbackScan: not used"
      in
      let cache_source =
        cache_source_for ctx symbol_result.value definition_result.value
          fallback_path_used
      in
      `Assoc
        ([
           ("symbol", symbol_ref_json ~uri:ctx.doc.Document.uri symbol_result.value);
           ("symbolName", (
             match symbol_result.value with
             | Some sym -> `String sym.name
             | None -> `Null));
           ("symbolKey", (
             match symbol_result.value with
             | Some sym -> `String sym.key
             | None -> `Null));
           ("position", position_json pos);
         ]
        @ readiness_json result_for_metadata
        @ [
            ( "resolutionPath",
              `List
                [
                  `String
                    ("documentReadiness: " ^ R.label symbol_result.readiness);
                  `String symbol_step;
                  `String target_step;
                  `String fallback_step;
                ] );
            ("fallbackScanUsed", `Bool fallback_scan_used);
            ("fallbackPathUsed", `Bool fallback_path_used);
            ("fallbackScanCountBefore", `Int fallback_before);
            ("fallbackScanCountAfter", `Int fallback_after);
            ("cacheSource", `String cache_source);
            ( "targetDefinition",
              target_definition_json symbol_result.value definition_result.value );
          ])

let debug_report_json (ws : t) (doc : Document.t) : Yojson.Safe.t =
  let readiness, reasons = readiness_for_doc ws doc in
  let authority = authority_for readiness reasons in
  let summary_total, summary_provisional, summary_validated =
    module_summary_cache_counts ws
  in
  let doc_summary_authority =
    match module_summary_entry_for_uri ws ~uri:doc.Document.uri with
    | None -> `Null
    | Some entry -> `String (module_summary_authority_label entry.msc_authority)
  in
  `Assoc
    [
      ("documentReadiness", `String (R.label readiness));
      ("authority", `String (R.authority_label authority));
      ("reasons", `List (List.map (fun r -> `String (R.reason_label r)) reasons));
      ("semanticStoreEnabled", `Bool ws.sem_store_enabled);
      ("semanticStoreRevision", `Int (Semantic_store.global_rev ws.semantic_store));
      ( "semanticSnapshotCached",
        `Bool
          (match
             Semantic_store.snapshot_for_uri ws.semantic_store
               ~uri:doc.Document.uri
           with
          | Some snap
            when snap.Semantic_store.Snapshot.doc_rev = doc.Document.parse_rev ->
              true
          | _ -> false) );
      ( "navigationStartupReady",
        `Bool (ws.startup_fully_nav_ready_ms <> None) );
      ( "moduleSummaryCache",
        `Assoc
          [
            ("loaded", `Bool ws.module_summary_cache_loaded);
            ("entryCount", `Int summary_total);
            ("provisionalCount", `Int summary_provisional);
            ("metadataValidatedCount", `Int summary_validated);
            ( "reverseImporterCompoolCount",
              `Int (Hashtbl.length ws.module_summary_reverse_importers) );
            ("documentAuthority", doc_summary_authority);
          ] );
      ("cancelRequested", `Bool (request_cancelled ws));
      ( "crossModule",
        `Assoc
          [
            ( "semanticHitCount",
              `Int (perf_metric_calls "query.cross_module.semantic_hit") );
            ( "summaryHitCount",
              `Int (perf_metric_calls "query.cross_module.summary_hit") );
            ( "fallbackScanCount",
              `Int (perf_metric_calls "query.cross_module.fallback_scan") );
            ( "provisionalResultCount",
              `Int
                (perf_metric_calls
                   "query.cross_module.provisional_result") );
            ( "authoritativeResultCount",
              `Int
                (perf_metric_calls
                   "query.cross_module.authoritative_result") );
          ] );
      ("counters", perf_stats_json ws);
    ]
