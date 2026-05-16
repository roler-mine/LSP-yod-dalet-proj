(* Module overview: Implementation navigation for procedures, declarations, and external definitions. *)

module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let implementation_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
        else
          match import_under_cursor doc pos with
          | Some _ -> Workspace_definition.definition_locations_for ws ~uri ~pos
          | None -> (
              let word = nav_word_at_position doc pos in
              match word with
              | None -> []
              | Some _ ->
                  let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
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
                  let defs =
                    match
                      symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos
                    with
                    | None -> []
                    | Some (sym_id, _) ->
                        if ws.sem_store_enabled then
                          Semantic_store.defs_for_sym_id ws.semantic_store sym_id
                          |> List.map def_of_snapshot_def
                          |> uniq_defs
                        else
                          docs_for_symbol ()
                          |> List.filter_map (fun d ->
                                 let dnav = nav_for_doc_cached ws cache d in
                                 Hashtbl.find_opt dnav.defs_by_id sym_id)
                          |> uniq_defs
                  in
                  let defs =
                    if defs = [] then defs
                    else
                      let impls =
                        List.filter (is_likely_proc_implementation ws) defs
                      in
                      if impls = [] then [] else impls
                  in
                  let key_opt =
                    match defs with
                    | d :: _ when d.key <> "" -> Some d.key
                    | _ -> (
                        match word with
                        | Some (nm, _) ->
                            let key = normalize_name nm in
                            if key = "" then None else Some key
                        | None -> None)
                  in
                  let defs =
                    if defs <> [] then defs
                    else
                      match key_opt with
                      | None -> []
                      | Some key ->
                          if
                            allow_fallback_for_ws ws doc
                            && not (nav_budget_check budget)
                          then proc_impl_defs_by_key ws doc ~key
                          else []
                  in
                  if defs = [] then [] else List.map location_of_def defs)
      in
      let result = nav_compute_with_budget_value budget compute in
      schedule_nav_miss_for_result ws doc pos ~empty:(result = []);
      result
