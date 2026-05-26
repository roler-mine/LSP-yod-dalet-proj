(* Module overview: Reference lookup and streaming reference results for large workspaces. *)

module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let macro_target_def_at_position (doc : Document.t) (pos : T.Position.t) :
    Workspace_nav_model.def option =
  let graph = Macro_graph.of_document doc in
  match
    Macro_graph.macro_use_at_position graph ~uri:doc.Document.uri ~pos
  with
  | Some exp -> Some exp.Macro_graph.define_def
  | None -> (
      match define_under_cursor doc pos with
      | Some (dm, _) when position_in_loc pos dm.Preprocess.loc ->
          Some (def_of_preprocess_define doc dm)
      | Some _ | None -> None)

let macro_reference_occs_for_docs (budget : nav_budget)
    (docs : Document.t list) (target : Workspace_nav_model.def) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  let acc = ref [] in
  List.iter
    (fun doc ->
      if not (nav_budget_check budget) then
        let graph = Macro_graph.of_document doc in
        let uses =
          Macro_graph.uses_of_define graph target
          |> List.map (fun exp ->
                 (exp.Macro_graph.call_site_uri, exp.Macro_graph.call_name_loc))
        in
        acc := List.rev_append uses !acc)
    docs;
  List.rev !acc

let macro_reference_locations (budget : nav_budget) (ws : t) (doc : Document.t)
    (target : Workspace_nav_model.def) ~(include_decl : bool) :
    T.Location.t list =
  let docs = doc :: docs_for_rename ws doc in
  let occs = macro_reference_occs_for_docs budget docs target in
  let occs =
    if include_decl then (target.uri, target.loc) :: occs else occs
  in
  locations_with_budget budget occs

let macro_key_in_doc (doc : Document.t) (key : string) : bool =
  key <> ""
  && List.exists
       (fun (d : Preprocess.define) -> d.Preprocess.key = key)
       doc.Document.defines

let macro_target_def_at_position_if_candidate (doc : Document.t)
    (pos : T.Position.t) : Workspace_nav_model.def option =
  match define_under_cursor doc pos with
  | Some (dm, _) when position_in_loc pos dm.Preprocess.loc ->
      Some (def_of_preprocess_define doc dm)
  | Some _ | None -> (
      match nav_word_at_position doc pos with
      | None -> None
      | Some (nm, _) ->
          let key = normalize_name nm in
          if not (macro_key_in_doc doc key) then None
          else
            Workspace_foundation.Perf_stats.time "query.macro_graph_ms"
              (fun () -> macro_target_def_at_position doc pos))

let startup_fast_references_active (ws : t) (doc : Document.t) : bool =
  let text_len = String.length doc.Document.text in
  let large_for_interactive =
    text_len >= min ws.bg_large_file_bytes ws.full_semantic_tokens_max_bytes
  in
  (doc.Document.parse_rev <> doc.Document.rev && large_for_interactive)
  || (ws.startup_fully_nav_ready_ms = None
     &&
     (large_for_interactive
     ||
     match Workspace_runtime.workspace_profile_for_budget ws with
     | Workspace_foundation.ProfileLarge -> true
     | Workspace_foundation.ProfileSmall | Workspace_foundation.ProfileMedium ->
         false))

let startup_reference_defs_for_key (ws : t) (doc : Document.t) ~(key : string) :
    def list =
  if
    key = ""
    || String.length doc.Document.text
       > max 2_097_152 ws.full_semantic_tokens_max_bytes
  then
    []
  else
    fallback_line_defs doc
    |> List.filter (fun d -> d.key = key && same_uri d.uri doc.Document.uri)

let startup_fast_reference_locations (budget : nav_budget) (ws : t)
    (doc : Document.t) ~(pos : T.Position.t) ~(include_decl : bool) :
    T.Location.t list =
  let pos = adjust_nav_position doc pos in
  match nav_word_at_position doc pos with
  | None -> []
  | Some (nm, word_loc) ->
      let key = normalize_name nm in
      if key = "" then []
      else
        let defs =
          if include_decl then [] else startup_reference_defs_for_key ws doc ~key
        in
        let def_keys = def_keys_for_defs defs in
        let occs =
          Workspace_foundation.Perf_stats.time
            "query.references_startup_current_doc_ms" (fun () ->
              occurrences_in_doc_fallback ~budget doc ~key)
        in
        let occs =
          (doc.Document.uri, word_loc) :: occs
          |> filter_declarations ~include_decl ~def_keys
        in
        locations_with_budget budget occs

let references_locations_for ?budget (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) ~(include_decl : bool) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = match budget with Some b -> b | None -> nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
        else if startup_fast_references_active ws doc then
          startup_fast_reference_locations budget ws doc ~pos ~include_decl
        else
          match import_under_cursor doc pos with
          | Some imp ->
              let key = normalize_name imp.name in
              if key = "" then []
              else
                let docs = docs_for_lookup ws doc in
                let defs =
                  docs
                  |> List.concat_map collect_doc_defs
                  |> List.filter (fun d -> d.key = key)
                in
                let def_keys = def_keys_for_defs defs in
                let occs = occurrences_for_docs_with_budget budget docs ~key in
                let occs =
                  if include_decl then occs
                  else
                    List.filter
                      (fun (u, loc) ->
                        not (Hashtbl.mem def_keys (loc_key ~uri:u loc)))
                      occs
                in
                locations_with_budget budget occs
          | None -> (
              match macro_target_def_at_position_if_candidate doc pos with
              | Some target ->
                  macro_reference_locations budget ws doc target ~include_decl
              | None ->
              let word = nav_word_at_position doc pos in
              match word with
              | None -> []
              | Some _ -> (
                  let cache : (string, doc_nav) Hashtbl.t =
                    Hashtbl.create 64
                  in
                  let nav = nav_for_doc_cached ws cache doc in
                  let resolved =
                    symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos
                  in
                  match resolved with
                  | Some (sym_id, _) ->
                      let docs_cache : Document.t list option ref = ref None in
                      let docs_for_symbol () =
                        match !docs_cache with
                        | Some xs -> xs
                        | None ->
                            let xs = docs_for_rename ws doc in
                            docs_cache := Some xs;
                            xs
                      in
                      let sym_defs, base_occs =
                        if ws.sem_store_enabled then
                          ( Semantic_store.defs_for_sym_id ws.semantic_store
                              sym_id
                            |> List.map def_of_snapshot_def
                            |> uniq_defs,
                            Semantic_store.refs_for_sym_id ws.semantic_store
                              sym_id )
                        else
                          let docs = docs_for_symbol () in
                          ( docs
                            |> List.filter_map (fun d ->
                                   let dnav = nav_for_doc_cached ws cache d in
                                   Hashtbl.find_opt dnav.defs_by_id sym_id)
                            |> uniq_defs,
                            docs
                            |> List.concat_map (fun d ->
                                   let dnav = nav_for_doc_cached ws cache d in
                                   match
                                     Hashtbl.find_opt dnav.occs_by_id sym_id
                                   with
                                   | None -> []
                                   | Some xs -> xs) )
                      in
                      if
                        List.exists
                          (fun d -> not (same_uri d.uri doc.Document.uri))
                          sym_defs
                      then
                        Workspace_foundation.Perf_stats.tick
                          "query.cross_module.semantic_hit";
                      let decl_keys = Hashtbl.create 8 in
                      List.iter
                        (fun defn ->
                          Hashtbl.replace decl_keys
                            (loc_key ~uri:defn.uri defn.loc)
                            true)
                        sym_defs;
                      let occs =
                        if include_decl then base_occs
                        else
                          List.filter
                            (fun (u, loc) ->
                              not (Hashtbl.mem decl_keys (loc_key ~uri:u loc)))
                            base_occs
                      in
                      locations_with_budget budget occs
                  | None -> (
                      match word with
                      | None -> []
                      | Some (nm, _) ->
                          let key = normalize_name nm in
                          if key = "" then []
                          else
                            let imported_defs =
                              let sem =
                                semantic_defs_for_imported_compools ws doc ~key
                              in
                              if sem <> [] then sem
                              else summary_defs_for_imported_compools ws doc ~key
                            in
                            let docs =
                              if imported_defs <> [] then
                                scoped_reference_docs_for_imported_symbol ws doc
                                  ~key
                              else docs_for_rename ws doc
                            in
                            let defs =
                              if imported_defs <> [] then imported_defs
                              else
                                let proc_defs =
                                  if allow_fallback_for_ws ws doc then
                                    proc_defs_by_key ws doc ~key
                                  else []
                                in
                                if proc_defs <> [] then proc_defs
                                else if allow_fallback_for_ws ws doc then
                                  docs
                                  |> List.concat_map collect_doc_defs
                                  |> List.filter (fun d -> d.key = key)
                                else []
                            in
                            if defs = [] then []
                            else
                              let def_keys = def_keys_for_defs defs in
                              let occs =
                                occurrences_for_docs_with_budget budget docs
                                  ~key
                              in
                              let occs =
                                if imported_defs <> [] && include_decl then
                                  List.map (fun d -> (d.uri, d.loc)) defs @ occs
                                else occs
                              in
                              let occs =
                                if include_decl then occs
                                else
                                  List.filter
                                    (fun (u, loc) ->
                                      not
                                        (Hashtbl.mem def_keys
                                           (loc_key ~uri:u loc)))
                                    occs
                              in
                              locations_with_budget budget occs)))
      in
      let result = nav_compute_with_budget_value budget compute in
      schedule_nav_miss_for_result ws doc pos ~empty:(result = []);
      result

let references_locations_stream ?budget (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) ~(include_decl : bool)
    ~(emit : T.Location.t list -> unit) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = match budget with Some b -> b | None -> nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
        else if startup_fast_references_active ws doc then
          let locations =
            startup_fast_reference_locations budget ws doc ~pos ~include_decl
          in
          if locations <> [] then emit locations;
          locations
        else
          let current, imported, workspace = reference_doc_stages ws doc in
          match import_under_cursor doc pos with
          | Some imp ->
              let key = normalize_name imp.name in
              if key = "" then []
              else
                let all_docs = current @ imported @ workspace in
                let defs =
                  all_docs
                  |> List.concat_map collect_doc_defs
                  |> List.filter (fun d -> d.key = key)
                in
                emit_reference_doc_stages budget ~emit ~include_decl
                  ~def_keys:(def_keys_for_defs defs) ~key current imported
                  workspace
          | None -> (
              match macro_target_def_at_position_if_candidate doc pos with
              | Some target ->
                  let locations =
                    macro_reference_locations budget ws doc target ~include_decl
                  in
                  if locations <> [] then emit locations;
                  locations
              | None ->
              match nav_word_at_position doc pos with
              | None -> []
              | Some (nm, _) -> (
                  let cache : (string, doc_nav) Hashtbl.t =
                    Hashtbl.create 64
                  in
                  let nav = nav_for_doc_cached ws cache doc in
                  match
                    symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos
                  with
                  | Some (sym_id, _) ->
                      let def_keys, staged_occs =
                        if ws.sem_store_enabled then
                          let defs =
                            Semantic_store.defs_for_sym_id ws.semantic_store
                              sym_id
                            |> List.map def_of_snapshot_def
                            |> uniq_defs
                          in
                          if
                            List.exists
                              (fun d -> not (same_uri d.uri doc.Document.uri))
                              defs
                          then
                            Workspace_foundation.Perf_stats.tick
                              "query.cross_module.semantic_hit";
                          let imported_keys =
                            let keys = Hashtbl.create 32 in
                            List.iter
                              (fun d ->
                                Hashtbl.replace keys (uri_key d.Document.uri)
                                  true)
                              imported;
                            keys
                          in
                          let current_rev = ref [] in
                          let imported_rev = ref [] in
                          let workspace_rev = ref [] in
                          Semantic_store.refs_for_sym_id ws.semantic_store
                            sym_id
                          |> List.iter (fun ((u, _) as occ) ->
                                 if same_uri u doc.Document.uri then
                                   current_rev := occ :: !current_rev
                                 else if Hashtbl.mem imported_keys (uri_key u)
                                 then imported_rev := occ :: !imported_rev
                                 else workspace_rev := occ :: !workspace_rev);
                          ( def_keys_for_defs defs,
                            ( List.rev !current_rev,
                              List.rev !imported_rev,
                              List.rev !workspace_rev ) )
                        else
                          let defs_and_occs docs =
                            let defs_rev = ref [] in
                            let occs_rev = ref [] in
                            List.iter
                              (fun d ->
                                let dnav = nav_for_doc_cached ws cache d in
                                (match
                                   Hashtbl.find_opt dnav.defs_by_id sym_id
                                 with
                                | None -> ()
                                | Some def -> defs_rev := def :: !defs_rev);
                                match Hashtbl.find_opt dnav.occs_by_id sym_id with
                                | None -> ()
                                | Some xs ->
                                    occs_rev := List.rev_append xs !occs_rev)
                              docs;
                            (List.rev !defs_rev, List.rev !occs_rev)
                          in
                          let defs_current, occs_current =
                            defs_and_occs current
                          in
                          let defs_imported, occs_imported =
                            defs_and_occs imported
                          in
                          let defs_workspace, occs_workspace =
                            defs_and_occs workspace
                          in
                          ( def_keys_for_defs
                              (uniq_defs
                                 (defs_current @ defs_imported
                                @ defs_workspace)),
                            (occs_current, occs_imported, occs_workspace) )
                      in
                      let seen = Hashtbl.create 256 in
                      let acc = ref [] in
                      let emit_occs occs =
                        let batch =
                          occs
                          |> filter_declarations ~include_decl ~def_keys
                          |> emit_locations_stage budget seen ~emit
                        in
                        acc := !acc @ batch
                      in
                      let occs_current, occs_imported, occs_workspace =
                        staged_occs
                      in
                      emit_occs occs_current;
                      emit_occs occs_imported;
                      emit_occs occs_workspace;
                      !acc
                  | None ->
                      let key = normalize_name nm in
                      if key = "" then []
                      else
                        let imported_defs =
                          let sem =
                            semantic_defs_for_imported_compools ws doc ~key
                          in
                          if sem <> [] then sem
                          else summary_defs_for_imported_compools ws doc ~key
                        in
                        if imported_defs <> [] then (
                          let docs =
                            scoped_reference_docs_for_imported_symbol ws doc
                              ~key
                          in
                          let def_keys = def_keys_for_defs imported_defs in
                          let occs =
                            occurrences_for_docs_with_budget budget docs ~key
                          in
                          let occs =
                            if include_decl then
                              List.map
                                (fun d -> (d.uri, d.loc))
                                imported_defs
                              @ occs
                            else occs
                          in
                          let occs =
                            occs
                            |> filter_declarations ~include_decl ~def_keys
                          in
                          emit_locations_stage budget (Hashtbl.create 256)
                            ~emit occs)
                        else
                          let all_docs = current @ imported @ workspace in
                          let proc_defs =
                            if allow_fallback_for_ws ws doc then
                              proc_defs_by_key ws doc ~key
                            else []
                          in
                          let defs =
                            if proc_defs <> [] then proc_defs
                            else if allow_fallback_for_ws ws doc then
                              all_docs
                              |> List.concat_map collect_doc_defs
                              |> List.filter (fun d -> d.key = key)
                            else []
                          in
                          if defs = [] then []
                          else
                            emit_reference_doc_stages budget ~emit ~include_decl
                              ~def_keys:(def_keys_for_defs defs) ~key current
                              imported workspace))
      in
      let result = nav_compute_with_budget_value budget compute in
      schedule_nav_miss_for_result ws doc pos ~empty:(result = []);
      result
