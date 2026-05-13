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

let code_lens_budget_ms = 20

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

let provisional ctx =
  ctx.ws.startup_fully_nav_ready_ms = None
  || Workspace_budget.reason_if_stopped ctx.budget <> None

let title_suffix ctx = if provisional ctx then " (provisional)" else ""

let command_data ctx (d : def) ~(action : string) ~(reference_count : int option)
    : Yojson.Safe.t =
  let reason =
    match Workspace_budget.reason_if_stopped ctx.budget with
    | None -> None
    | Some reason -> Some (R.reason_label reason)
  in
  let base =
    [
      ("kind", `String "jovial.codelens");
      ("action", `String action);
      ("uri", `String (Uri_path.docuri_to_string d.uri));
      ("symbol", `String d.name);
      ("key", `String d.key);
      ("declarationKind", `String (Metadata.symbol_kind_label d.metadata.jovial_kind));
      ("provisional", `Bool (provisional ctx));
      ( "readiness",
        `String
          (if ctx.ws.startup_fully_nav_ready_ms <> None then
             R.label R.WorkspaceSemanticReady
           else R.label R.SkeletonReady) );
      ("impact", `String (Workspace_change_impact.change_impact_for_def ctx.ws d));
    ]
  in
  let fields =
    match reference_count with
    | Some count -> ("referenceCount", `Int count) :: base
    | None -> base
  in
  let fields =
    match reason with
    | Some reason -> ("reason", `String reason) :: fields
    | None -> fields
  in
  `Assoc fields

let lens_command ~(title : string) (d : def) : T.Command.t =
  T.Command.create ~title ~command:"jovial.debugReport"
    ~arguments:[ `String (Uri_path.docuri_to_string d.uri); `Int 200 ]
    ()

let make_lens ctx d ~(title : string) ~(action : string)
    ?reference_count () : T.CodeLens.t =
  let title = title ^ title_suffix ctx in
  T.CodeLens.create ~range:(Lsp_conv.range_of_loc d.loc)
    ~command:(lens_command ~title d)
    ~data:(command_data ctx d ~action ~reference_count)
    ()

let lens_for_def ctx (d : def) : T.CodeLens.t option =
  if Workspace_budget.should_stop ~phase:"codelens.def" ctx.budget then None
  else
    match d.metadata.Metadata.jovial_kind with
    | Metadata.JovialProcedure | Metadata.JovialFunction ->
        let refs = reference_count_for_def ctx d in
        let ref_imports = proc_ref_count ctx ~key:d.key in
        let impls = proc_impl_count ctx ~key:d.key in
        let title =
          Printf.sprintf "%d %s | %d REFs | %d %s | show impact" refs
            (plural refs "reference" "references")
            ref_imports impls
            (plural impls "implementation" "implementations")
        in
        Some
          (make_lens ctx d ~title ~action:"procedureImpact"
             ~reference_count:refs ())
    | Metadata.JovialCompool ->
        let importers = importer_count ctx ~key:d.key in
        let title =
          Printf.sprintf "%d %s | show import graph" importers
            (plural importers "importer" "importers")
        in
        Some
          (make_lens ctx d ~title ~action:"compoolImportGraph"
             ~reference_count:importers ())
    | Metadata.JovialDefine ->
        let uses = reference_count_for_def ctx d in
        let title =
          Printf.sprintf "%d macro %s | expansion impact" uses
            (plural uses "use" "uses")
        in
        Some
          (make_lens ctx d ~title ~action:"defineExpansionImpact"
             ~reference_count:uses ())
    | Metadata.JovialTable | Metadata.JovialBlock
    | Metadata.JovialConstantTable ->
        let refs = reference_count_for_def ctx d in
        let title =
          Printf.sprintf "%d %s | layout impact" refs
            (plural refs "reference" "references")
        in
        Some
          (make_lens ctx d ~title ~action:"layoutImpact" ~reference_count:refs ())
    | Metadata.JovialItem | Metadata.JovialConstantItem
    | Metadata.JovialStatusConstant ->
        let refs = reference_count_for_def ctx d in
        let title =
          Printf.sprintf "%d %s" refs (plural refs "reference" "references")
        in
        Some
          (make_lens ctx d ~title ~action:"referenceCount"
             ~reference_count:refs ())
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
            if Workspace_budget.should_stop ~phase:"codelens.loop" budget then
              List.rev acc
            else
              let acc =
                match lens_for_def ctx d with
                | None -> acc
                | Some lens -> lens :: acc
              in
              loop acc rest
      in
      loop [] defs

let resolve_code_lens (_ws : t) (lens : T.CodeLens.t) : T.CodeLens.t = lens
