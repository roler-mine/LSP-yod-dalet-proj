module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let definition_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let macro_graph = lazy (Macro_graph.of_document doc) in
      let definition_for_pos (pos : T.Position.t) : T.Location.t list =
        let pos = adjust_nav_position doc pos in
        let budget = nav_budget_start ws in
        let compute () =
          if nav_budget_check budget then []
          else
            match import_under_cursor doc pos with
            | Some imp ->
                defs_for_import_cursor ws imp |> List.map location_of_def
            | None -> (
                match
                  Macro_graph.definition_of_macro_use (Lazy.force macro_graph)
                    ~uri:doc.Document.uri ~pos
                with
                | Some d -> [ location_of_def d ]
                | None ->
                let word = nav_word_at_position doc pos in
                let imported_exported_proc_summary_hits =
                  match word with
                  | None -> []
                  | Some (nm, _) ->
                      let key = normalize_name nm in
                      if key = "" then []
                      else
                        summary_defs_for_imported_compools ws doc ~key
                        |> List.filter (fun d ->
                               d.kind = sym_kind_func
                               && not (is_ref_import_def d))
                in
                if imported_exported_proc_summary_hits <> [] then
                  List.map location_of_def imported_exported_proc_summary_hits
                else
                  let startup_quick_hits =
                    match word with
                    | None -> []
                    | Some (nm, _) ->
                        let key = normalize_name nm in
                        if key = "" then []
                        else proc_real_defs_by_key ws doc ~key
                  in
                  if startup_quick_hits <> [] then
                    List.map location_of_def startup_quick_hits
                  else
                  let allow_fallback = allow_fallback_for_ws ws doc in
                  match define_under_cursor doc pos with
                  | Some (dm, _) when position_in_loc pos dm.Preprocess.loc ->
                      [ location_of_def (def_of_preprocess_define doc dm) ]
                  | Some _ | None -> (
                      match word with
                      | None -> []
                      | Some _ ->
                          let cache : (string, doc_nav) Hashtbl.t =
                            Hashtbl.create 32
                          in
                          let nav = nav_for_doc_cached ws cache doc in
                          let docs_cache : Document.t list option ref = ref None in
                          let docs_for_symbol () =
                            match !docs_cache with
                            | Some xs -> xs
                            | None ->
                                let xs = docs_for_lookup ws doc in
                                docs_cache := Some xs;
                                xs
                          in
                          let hit =
                            match
                              symbol_at_position_in_nav nav
                                ~uri:doc.Document.uri ~pos
                            with
                            | None -> None
                            | Some (sym_id, _) -> (
                                match Hashtbl.find_opt nav.defs_by_id sym_id with
                                | Some _ as d0 -> d0
                                | None ->
                                    if ws.sem_store_enabled then
                                      match
                                        Semantic_store.defs_for_sym_id
                                          ws.semantic_store sym_id
                                      with
                                      | d0 :: _ -> Some (def_of_snapshot_def d0)
                                      | [] ->
                                          if nav_budget_check budget then None
                                          else
                                            find_def_for_sym_id ws cache
                                              ~docs:(docs_for_symbol ()) ~sym_id
                                    else if nav_budget_check budget then None
                                    else
                                      find_def_for_sym_id ws cache
                                        ~docs:(docs_for_symbol ()) ~sym_id)
                          in
                          match hit with
                          | Some d ->
                              if not (same_uri d.uri doc.Document.uri) then
                                Workspace_foundation.Perf_stats.tick
                                  "query.cross_module.semantic_hit";
                              let defs =
                                if
                                  d.kind = sym_kind_func
                                  && not (is_likely_proc_implementation ws d)
                                then
                                  let impls =
                                    proc_real_defs_by_key ws doc ~key:d.key
                                  in
                                  if impls = [] then [ d ] else impls
                                else [ d ]
                              in
                              List.map location_of_def defs
                          | None ->
                              let imported_defs =
                                match word with
                                | None -> []
                                | Some (nm, _) ->
                                    let key = normalize_name nm in
                                    if key = "" then []
                                    else
                                      let semantic_hits =
                                        semantic_defs_for_imported_compools ws
                                          doc ~key
                                      in
                                      if semantic_hits <> [] then semantic_hits
                                      else
                                        summary_defs_for_imported_compools ws
                                          doc ~key
                              in
                              if imported_defs <> [] then
                                List.map location_of_def imported_defs
                              else
                                let startup_quick_hits =
                                  match word with
                                  | None -> []
                                  | Some (nm, _) ->
                                      let key = normalize_name nm in
                                      if key = "" then []
                                      else proc_real_defs_by_key ws doc ~key
                                in
                                if startup_quick_hits <> [] then
                                  List.map location_of_def startup_quick_hits
                                else if nav_budget_check budget then []
                              else
                                let proc_by_name =
                                  if not allow_fallback then []
                                  else
                                    match word with
                                    | None -> []
                                    | Some (nm, _) ->
                                        let key = normalize_name nm in
                                        if key = "" then []
                                        else proc_real_defs_by_key ws doc ~key
                                in
                                if proc_by_name <> [] then
                                  List.map location_of_def proc_by_name
                                else if
                                  (not allow_fallback) || nav_budget_check budget
                                then []
                                else
                                  let by_name =
                                    match word with
                                    | None -> []
                                    | Some (nm, _) ->
                                        fallback_defs_by_name ws doc
                                          (normalize_name nm)
                                        |> prefer_local_defs_before_position doc
                                             pos
                                  in
                                  List.map location_of_def by_name))
        in
        let result = nav_compute_with_budget_value budget compute in
        schedule_nav_miss_for_result ws doc pos ~empty:(result = []);
        result
      in
      let primary = definition_for_pos pos in
      let original_was_filtered =
        match (word_at_position doc pos, nav_word_at_position doc pos) with
        | Some _, None -> true
        | _ -> false
      in
      if primary = [] && pos.character > 0 && not original_was_filtered then
        definition_for_pos { pos with T.Position.character = pos.character - 1 }
      else primary

let declaration_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
        else
          match nav_word_at_position doc pos with
          | Some (nm, _) ->
              let key = normalize_name nm in
              if key = "" then definition_locations_for ws ~uri ~pos
              else
                let proc_defs = proc_defs_by_key ws doc ~key |> uniq_defs in
                let decls =
                  proc_defs
                  |> List.filter (fun d ->
                         (not (is_likely_proc_implementation ws d))
                         && not (is_ref_import_def d))
                in
                if decls = [] then
                  let real_fallback =
                    proc_defs |> List.filter (fun d -> not (is_ref_import_def d))
                  in
                  if real_fallback <> [] then
                    List.map location_of_def real_fallback
                  else
                    let ref_fallback =
                      proc_defs |> List.filter is_ref_import_def
                    in
                    if ref_fallback = [] then definition_locations_for ws ~uri ~pos
                    else List.map location_of_def ref_fallback
                else List.map location_of_def decls
          | None -> definition_locations_for ws ~uri ~pos
      in
      nav_compute_with_budget_value budget compute
