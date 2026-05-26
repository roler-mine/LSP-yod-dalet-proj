(* Module overview: Computes and resolves CodeLens entries for references and change impact. *)

module T = Lsp.Types
module Metadata = Workspace_symbol_metadata
module R = Workspace_readiness

open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

type count_context = {
  ws : t;
  doc : Document.t;
  budget : Workspace_budget.t;
  nav_cache : (string, doc_nav) Hashtbl.t;
  mutable lookup_docs : Document.t list option;
}

let code_lens_budget_ms = 40

type count_confidence =
  | ExactAuthoritative
  | Provisional
  | LowerBound
  | Pending

type count_result = {
  count : int option;
  confidence : count_confidence;
  readiness : R.t;
  authority : R.authority;
  reasons : R.reason list;
}

let same_def (a : def) (b : def) : bool =
  a.key = b.key && same_uri a.uri b.uri
  && loc_key ~uri:a.uri a.loc = loc_key ~uri:b.uri b.loc

let lookup_docs (ctx : count_context) : Document.t list =
  match ctx.lookup_docs with
  | Some docs -> docs
  | None ->
      let docs =
        if Workspace_budget.should_stop ~phase:"codelens.lookup_docs" ctx.budget
        then [ ctx.doc ]
        else docs_for_lookup ctx.ws ctx.doc
      in
      ctx.lookup_docs <- Some docs;
      docs

let sym_id_for_def (ctx : count_context) (d : def) : string option =
  let doc =
    if same_uri d.uri ctx.doc.Document.uri then Some ctx.doc
    else List.find_opt (fun doc -> same_uri doc.Document.uri d.uri) (lookup_docs ctx)
  in
  match doc with
  | None -> None
  | Some doc ->
      let nav = nav_for_doc_cached ctx.ws ctx.nav_cache doc in
      let hit = ref None in
      Hashtbl.iter
        (fun sym_id def ->
          if !hit = None && same_def def d then hit := Some sym_id)
        nav.defs_by_id;
      !hit

let unique_occ_count (occs : (T.DocumentUri.t * Ast.Loc.t) list) : int =
  let seen = Hashtbl.create 64 in
  let count = ref 0 in
  List.iter
    (fun (uri, loc) ->
      let key = loc_key ~uri loc in
      if not (Hashtbl.mem seen key) then (
        Hashtbl.replace seen key true;
        incr count))
    occs;
  !count

let declaration_keys_for_key (ctx : count_context) ~(key : string) :
    (string, bool) Hashtbl.t =
  let decls = Hashtbl.create 16 in
  lookup_docs ctx
  |> List.iter (fun doc ->
         if not (Workspace_budget.should_stop ~phase:"codelens.decls" ctx.budget)
         then
           collect_doc_defs doc
           |> List.iter (fun d ->
                  if d.key = key then
                    Hashtbl.replace decls (loc_key ~uri:d.uri d.loc) true));
  decls

let non_declaration_occs ~(decls : (string, bool) Hashtbl.t)
    (occs : (T.DocumentUri.t * Ast.Loc.t) list) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  List.filter
    (fun (uri, loc) -> not (Hashtbl.mem decls (loc_key ~uri loc)))
    occs

let fallback_reference_count (ctx : count_context) ~(key : string) : int =
  if key = "" then 0
  else
    let decls = declaration_keys_for_key ctx ~key in
    lookup_docs ctx
    |> occurrences_for_docs_with_budget ctx.budget ~key
    |> non_declaration_occs ~decls |> unique_occ_count

let reference_count_for_def (ctx : count_context) (d : def) : int =
  match sym_id_for_def ctx d with
  | Some sym_id when ctx.ws.sem_store_enabled -> (
      let defs =
        Semantic_store.defs_for_sym_id ctx.ws.semantic_store sym_id
        |> List.map def_of_snapshot_def
      in
      let decls = def_keys_for_defs defs in
      let refs =
        Semantic_store.refs_for_sym_id ctx.ws.semantic_store sym_id
        |> non_declaration_occs ~decls
      in
      match refs with
      | [] -> fallback_reference_count ctx ~key:d.key
      | _ -> unique_occ_count refs)
  | _ -> fallback_reference_count ctx ~key:d.key

let defs_for_key (ctx : count_context) ~(key : string) : def list =
  if key = "" then []
  else
    let from_semantic_store =
      if not ctx.ws.sem_store_enabled then []
      else
        Semantic_store.sym_ids_for_key ctx.ws.semantic_store ~key
        |> List.concat_map (fun sym_id ->
               Semantic_store.defs_for_sym_id ctx.ws.semantic_store sym_id)
        |> List.map def_of_snapshot_def
    in
    let from_docs =
      lookup_docs ctx
      |> List.concat_map (fun doc ->
             if Workspace_budget.should_stop ~phase:"codelens.defs" ctx.budget
             then []
             else collect_doc_defs doc)
    in
    from_semantic_store @ from_docs
    |> List.filter (fun d -> d.key = key)
    |> uniq_defs

let proc_ref_count (ctx : count_context) ~(key : string) : int =
  defs_for_key ctx ~key
  |> List.filter (fun d -> d.metadata.Metadata.external_kind = Metadata.ExternalRef)
  |> List.length

let proc_impl_count (ctx : count_context) ~(key : string) : int =
  defs_for_key ctx ~key
  |> List.filter (is_likely_proc_implementation ctx.ws)
  |> List.length

let importer_count (ctx : count_context) ~(key : string) : int =
  importer_uris_for_compool_key ctx.ws ~compool_key:key |> List.length

let plural count singular plural =
  if count = 1 then singular else plural

let readiness_for_ctx ctx =
  if ctx.ws.startup_fully_nav_ready_ms <> None then R.WorkspaceSemanticReady
  else R.SkeletonReady

let append_reason reason reasons =
  if List.exists (( = ) reason) reasons then reasons else reasons @ [ reason ]

let reasons_for_ctx ctx =
  let reasons =
    if ctx.ws.startup_fully_nav_ready_ms = None then
      [ R.WorkspaceIndexWarming ]
    else []
  in
  match Workspace_budget.reason_if_stopped ctx.budget with
  | None -> reasons
  | Some reason -> append_reason reason reasons

let confidence_label = function
  | ExactAuthoritative -> "exact-authoritative"
  | Provisional -> "provisional"
  | LowerBound -> "lower-bound"
  | Pending -> "pending"

let count_result_for ctx count : count_result =
  match Workspace_budget.reason_if_stopped ctx.budget with
  | Some reason ->
      {
        count = Some count;
        confidence = LowerBound;
        readiness = readiness_for_ctx ctx;
        authority = R.Provisional;
        reasons = append_reason reason (reasons_for_ctx ctx);
      }
  | None when ctx.ws.startup_fully_nav_ready_ms <> None ->
      {
        count = Some count;
        confidence = ExactAuthoritative;
        readiness = R.WorkspaceSemanticReady;
        authority = R.Authoritative;
        reasons = [];
      }
  | None ->
      {
        count = Some count;
        confidence = Provisional;
        readiness = R.SkeletonReady;
        authority = R.Provisional;
        reasons = reasons_for_ctx ctx;
      }

let pending_count ctx : count_result =
  {
    count = None;
    confidence = Pending;
    readiness = readiness_for_ctx ctx;
    authority = R.Provisional;
    reasons = reasons_for_ctx ctx;
  }

let counted ctx f : count_result =
  match Workspace_budget.reason_if_stopped ctx.budget with
  | Some _ -> pending_count ctx
  | None ->
      let count = f () in
      count_result_for ctx count

let count_phrase ?pending_label result singular plural_label =
  match (result.confidence, result.count) with
  | Pending, _ | _, None -> (
      match pending_label with
      | Some label -> label
      | None -> plural_label ^ " pending")
  | ExactAuthoritative, Some count ->
      Printf.sprintf "%d %s" count (plural count singular plural_label)
  | Provisional, Some count ->
      Printf.sprintf "~%d %s" count (plural count singular plural_label)
  | LowerBound, Some count ->
      Printf.sprintf "%d+ %s" count (plural count singular plural_label)

let provisional_from_count = function
  | Some result -> result.authority <> R.Authoritative
  | None -> true

let command_data ctx (d : def) ~(action : string) ~(count : count_result option)
    : Yojson.Safe.t =
  let readiness =
    match count with Some c -> c.readiness | None -> readiness_for_ctx ctx
  in
  let authority =
    match count with Some c -> c.authority | None -> R.Provisional
  in
  let confidence =
    match count with
    | Some c -> c.confidence
    | None -> if authority = R.Authoritative then ExactAuthoritative else Pending
  in
  let reasons =
    match count with Some c -> c.reasons | None -> reasons_for_ctx ctx
  in
  let base =
    [
      ("kind", `String "jovial.codelens");
      ("action", `String action);
      ("uri", `String (Uri_path.docuri_to_string d.uri));
      ("symbol", `String d.name);
      ("key", `String d.key);
      ("declarationKind", `String (Metadata.symbol_kind_label d.metadata.jovial_kind));
      ("provisional", `Bool (provisional_from_count count));
      ("confidence", `String (confidence_label confidence));
      ("authority", `String (R.authority_label authority));
      ("authoritative", `Bool (authority = R.Authoritative));
      ("readiness", `String (R.label readiness));
      ("reasons", `List (List.map (fun r -> `String (R.reason_label r)) reasons));
      ("impact", `String (Workspace_change_impact.change_impact_for_def ctx.ws d));
    ]
  in
  let fields =
    match count with
    | Some { count = Some count; confidence; _ } ->
        ("referenceCount", `Int count)
        :: ("countConfidence", `String (confidence_label confidence))
        :: base
    | Some { count = None; confidence; _ } ->
        ("countConfidence", `String (confidence_label confidence)) :: base
    | None -> ("countConfidence", `String (confidence_label confidence)) :: base
  in
  let fields =
    match reasons with
    | reason :: _ -> ("reason", `String (R.reason_label reason)) :: fields
    | [] -> fields
  in
  `Assoc fields

let lens_command ~(title : string) (d : def) : T.Command.t =
  T.Command.create ~title ~command:"jovial.debugReport"
    ~arguments:[ `String (Uri_path.docuri_to_string d.uri); `Int 200 ]
    ()

let make_lens ctx d ~(title : string) ~(action : string)
    ?count () : T.CodeLens.t =
  T.CodeLens.create ~range:(Lsp_conv.range_of_loc d.loc)
    ~command:(lens_command ~title d)
    ~data:(command_data ctx d ~action ~count)
    ()

let lens_for_def ctx (d : def) : T.CodeLens.t option =
  match d.metadata.Metadata.jovial_kind with
    | Metadata.JovialProcedure | Metadata.JovialFunction ->
        let refs = counted ctx (fun () -> reference_count_for_def ctx d) in
        let ref_imports = counted ctx (fun () -> proc_ref_count ctx ~key:d.key) in
        let impls = counted ctx (fun () -> proc_impl_count ctx ~key:d.key) in
        let title =
          Printf.sprintf "%s | %s | %s | show impact"
            (count_phrase ~pending_label:"references pending" refs "reference"
               "references")
            (count_phrase ~pending_label:"REFs pending" ref_imports "REF"
               "REFs")
            (count_phrase ~pending_label:"implementations pending" impls
               "implementation" "implementations")
        in
        Some
          (make_lens ctx d ~title ~action:"procedureImpact"
             ~count:refs ())
    | Metadata.JovialCompool ->
        let importers = counted ctx (fun () -> importer_count ctx ~key:d.key) in
        let title =
          Printf.sprintf "%s | show import graph"
            (count_phrase ~pending_label:"importers pending" importers
               "importer" "importers")
        in
        Some
          (make_lens ctx d ~title ~action:"compoolImportGraph"
             ~count:importers ())
    | Metadata.JovialDefine ->
        let uses = counted ctx (fun () -> reference_count_for_def ctx d) in
        let title =
          Printf.sprintf "%s | expansion impact"
            (count_phrase ~pending_label:"macro uses pending" uses
               "macro use" "macro uses")
        in
        Some
          (make_lens ctx d ~title ~action:"defineExpansionImpact"
             ~count:uses ())
    | Metadata.JovialTable | Metadata.JovialBlock | Metadata.JovialOverlay
    | Metadata.JovialConstantTable ->
        let refs = counted ctx (fun () -> reference_count_for_def ctx d) in
        let title =
          Printf.sprintf "%s | layout impact"
            (count_phrase ~pending_label:"references pending" refs
               "reference" "references")
        in
        Some (make_lens ctx d ~title ~action:"layoutImpact" ~count:refs ())
    | Metadata.JovialItem | Metadata.JovialConstantItem
    | Metadata.JovialStatusConstant ->
        let refs = counted ctx (fun () -> reference_count_for_def ctx d) in
        let title =
          count_phrase ~pending_label:"references pending" refs
            "reference" "references"
        in
        Some
          (make_lens ctx d ~title ~action:"referenceCount"
             ~count:refs ())
    | _ -> None

let code_lenses_for (ws : t) ~(uri : T.DocumentUri.t) : T.CodeLens.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = Workspace_budget.start ~ws ~soft_budget_ms:code_lens_budget_ms in
      let ctx =
        {
          ws;
          doc;
          budget;
          nav_cache = Hashtbl.create 16;
          lookup_docs = None;
        }
      in
      let defs = collect_doc_defs doc |> uniq_defs in
      let rec loop acc = function
        | [] -> List.rev acc
        | d :: rest ->
            let stopped =
              Workspace_budget.should_stop ~phase:"codelens.loop" budget
            in
            let acc =
              match lens_for_def ctx d with
              | None -> acc
              | Some lens -> lens :: acc
            in
            if stopped then List.rev acc else loop acc rest
      in
      loop [] defs

let resolve_code_lens (_ws : t) (lens : T.CodeLens.t) : T.CodeLens.t = lens
