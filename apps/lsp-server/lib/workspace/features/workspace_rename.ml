module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let prepare_rename_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    [ `Range of T.Range.t | `RangeWithPlaceholder of T.Range.t * string ] option
    =
  match doc_of_uri ws uri with
  | None -> None
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then None
        else
          let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
          let nav = nav_for_doc_cached ws cache doc in
          match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
          | Some (sym_id, sym_loc) ->
              let docs = docs_for_rename ws doc in
              let has_any =
                docs
                |> List.exists (fun d ->
                       if nav_budget_check budget then false
                       else
                         let dnav = nav_for_doc_cached ws cache d in
                         match Hashtbl.find_opt dnav.occs_by_id sym_id with
                         | None -> false
                         | Some xs -> xs <> [])
              in
              if nav_budget_check budget || not has_any then None
              else
                let placeholder =
                  match word_at_position doc pos with
                  | Some (nm, _) -> nm
                  | None -> (
                      match Hashtbl.find_opt nav.defs_by_id sym_id with
                      | Some d -> d.name
                      | None -> "name")
                in
                Some
                  (`RangeWithPlaceholder
                     (Lsp_conv.range_of_loc sym_loc, placeholder))
          | None -> (
              match nav_word_at_position doc pos with
              | None -> None
              | Some (nm, word_loc) ->
                  let key = normalize_name nm in
                  if key = "" || not (allow_fallback_for_ws ws doc) then None
                  else
                    let has_any =
                      docs_for_rename ws doc
                      |> List.exists (fun d ->
                             if nav_budget_check budget then false
                             else occurrences_in_doc d ~key <> [])
                    in
                    if nav_budget_check budget || not has_any then None
                    else
                      Some
                        (`RangeWithPlaceholder
                           (Lsp_conv.range_of_loc word_loc, nm)))
      in
      let computed = nav_compute_with_budget_value budget compute in
      if Workspace_budget.exceeded budget then None else computed

let rename_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    ~(new_name : string) : T.WorkspaceEdit.t option =
  if not (is_valid_rename_name new_name) then None
  else
    match doc_of_uri ws uri with
    | None -> None
    | Some doc ->
        let budget = nav_budget_start ws in
        let compute () =
          if nav_budget_check budget then None
          else
            let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
            let nav = nav_for_doc_cached ws cache doc in
            let docs = docs_for_rename ws doc in
            let seen = Hashtbl.create 1024 in
            let edits_by_uri : (T.DocumentUri.t, T.TextEdit.t list) Hashtbl.t =
              Hashtbl.create 128
            in
            let add_edit (u : T.DocumentUri.t) (loc : Ast.Loc.t) =
              if nav_budget_check budget then ()
              else
                let lk = loc_key ~uri:u loc in
                if not (Hashtbl.mem seen lk) then (
                  Hashtbl.add seen lk true;
                  let edit =
                    T.TextEdit.create
                      ~range:(Lsp_conv.range_of_loc loc)
                      ~newText:new_name
                  in
                  let prev =
                    match Hashtbl.find_opt edits_by_uri u with
                    | None -> []
                    | Some xs -> xs
                  in
                  Hashtbl.replace edits_by_uri u (edit :: prev))
            in
            let apply_changes () =
              let changes =
                Hashtbl.fold
                  (fun uri edits acc -> (uri, List.rev edits) :: acc)
                  edits_by_uri []
              in
              match changes with
              | [] -> None
              | _ -> Some (T.WorkspaceEdit.create ~changes ())
            in
            match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
            | Some (sym_id, _) ->
                List.iter
                  (fun d ->
                    if not (nav_budget_check budget) then
                      let dnav = nav_for_doc_cached ws cache d in
                      match Hashtbl.find_opt dnav.occs_by_id sym_id with
                      | None -> ()
                      | Some xs ->
                          List.iter
                            (fun (u, loc) ->
                              if not (nav_budget_check budget) then add_edit u loc)
                            xs)
                  docs;
                if nav_budget_check budget then None else apply_changes ()
            | None -> (
                match nav_word_at_position doc pos with
                | None -> None
                | Some (nm, _) ->
                    let key = normalize_name nm in
                    if key = "" || not (allow_fallback_for_ws ws doc) then None
                    else (
                      List.iter
                        (fun d ->
                          if not (nav_budget_check budget) then
                            occurrences_in_doc d ~key
                            |> List.iter (fun (u, loc) ->
                                   if not (nav_budget_check budget) then
                                     add_edit u loc))
                        docs;
                      if nav_budget_check budget then None else apply_changes ()))
        in
        let computed = nav_compute_with_budget_value budget compute in
        if Workspace_budget.exceeded budget then None else computed
