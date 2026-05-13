module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let type_definition_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
        else
          let key_opt =
            match nav_word_at_position doc pos with
            | None -> None
            | Some (nm, _) ->
                let key = normalize_name nm in
                if key = "" then None else Some key
          in
          match key_opt with
          | None -> []
          | Some key ->
              let semantic_defs =
                semantic_defs_for_imported_compools ~type_only:true ws doc ~key
              in
              let defs =
                if semantic_defs <> [] then semantic_defs
                else
                  let summary_defs =
                    summary_defs_for_imported_compools ~type_only:true ws doc
                      ~key
                  in
                  if summary_defs <> [] then summary_defs
                  else
                    docs_for_lookup ws doc
                    |> List.concat_map collect_doc_defs
                    |> List.filter (fun d ->
                           d.kind = sym_kind_type && d.key = key)
                    |> uniq_defs
              in
              List.map location_of_def defs
      in
      nav_compute_with_budget_value budget compute
