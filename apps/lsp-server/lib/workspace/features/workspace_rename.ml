(* Module overview: Rename preparation and workspace edit generation for Jovial symbols. *)

module T = Lsp.Types
module Metadata = Workspace_symbol_metadata
open Workspace_state
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_navigation_support

let same_loc_range (a : Ast.Loc.t) (b : Ast.Loc.t) : bool =
  a.Ast.Loc.start_pos.line = b.Ast.Loc.start_pos.line
  && a.Ast.Loc.start_pos.col = b.Ast.Loc.start_pos.col
  && a.Ast.Loc.start_pos.offset = b.Ast.Loc.start_pos.offset
  && a.Ast.Loc.end_pos.line = b.Ast.Loc.end_pos.line
  && a.Ast.Loc.end_pos.col = b.Ast.Loc.end_pos.col
  && a.Ast.Loc.end_pos.offset = b.Ast.Loc.end_pos.offset

let loc_contains_loc (outer : Ast.Loc.t) (inner : Ast.Loc.t) : bool =
  outer.Ast.Loc.start_pos.offset <= inner.Ast.Loc.start_pos.offset
  && inner.Ast.Loc.end_pos.offset <= outer.Ast.Loc.end_pos.offset

let loc_in_macro_expansion (graph : Macro_graph.t) (loc : Ast.Loc.t) : bool =
  Macro_graph.expansions graph
  |> List.exists (fun (exp : Macro_graph.expansion) ->
         match exp.expanded_loc with
         | Some expanded -> loc_contains_loc expanded loc
         | None -> false)

let unsafe_generated_symbol_at_position (graph : Macro_graph.t)
    (loc : Ast.Loc.t) : bool =
  loc_in_macro_expansion graph loc
  &&
  match Macro_graph.source_loc_for_generated_loc graph loc with
  | None -> false
  | Some source_loc -> not (same_loc_range loc source_loc)

let define_at_position (doc : Document.t) (pos : T.Position.t) :
    Preprocess.define option =
  doc.Document.defines
  |> List.find_opt (fun (d : Preprocess.define) -> position_in_loc pos d.loc)

let macro_target_at_position (doc : Document.t) (graph : Macro_graph.t)
    ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    (def * Ast.Loc.t * string) option =
  match Macro_graph.macro_use_at_position graph ~uri ~pos with
  | Some exp -> Some (exp.define_def, exp.call_name_loc, exp.define.name)
  | None -> (
      match define_at_position doc pos with
      | None -> None
      | Some define ->
          let d = def_of_preprocess_define doc define in
          Some (d, define.loc, define.name))

let fallback_defs_for_key docs key =
  docs
  |> List.concat_map (fun doc ->
         collect_doc_defs doc |> List.filter (fun d -> d.key = key))
  |> uniq_defs

let fallback_rename_safe docs key =
  fallback_defs_for_key docs key
  |> List.for_all (fun d ->
         d.kind <> sym_kind_field
         && d.metadata.Metadata.jovial_kind <> Metadata.JovialField)

let add_text_edit ~(budget : nav_budget) ~(seen : (string, bool) Hashtbl.t)
    ~(edits_by_uri : (T.DocumentUri.t, T.TextEdit.t list) Hashtbl.t)
    ~(new_name : string) (u : T.DocumentUri.t) (loc : Ast.Loc.t) : unit =
  if nav_budget_check budget then ()
  else
    let lk = loc_key ~uri:u loc in
    if not (Hashtbl.mem seen lk) then (
      Hashtbl.add seen lk true;
      let edit =
        T.TextEdit.create ~range:(Lsp_conv.range_of_loc loc) ~newText:new_name
      in
      let prev =
        match Hashtbl.find_opt edits_by_uri u with
        | None -> []
        | Some xs -> xs
      in
      Hashtbl.replace edits_by_uri u (edit :: prev))

let workspace_edit_of_edits edits_by_uri : T.WorkspaceEdit.t option =
  let changes =
    Hashtbl.fold
      (fun uri edits acc -> (uri, List.rev edits) :: acc)
      edits_by_uri []
  in
  match changes with
  | [] -> None
  | _ -> Some (T.WorkspaceEdit.create ~changes ())

let macro_rename_edit ~(budget : nav_budget) ~(docs : Document.t list)
    ~(target : def) ~(new_name : string) : T.WorkspaceEdit.t option =
  let seen = Hashtbl.create 64 in
  let edits_by_uri : (T.DocumentUri.t, T.TextEdit.t list) Hashtbl.t =
    Hashtbl.create 8
  in
  add_text_edit ~budget ~seen ~edits_by_uri ~new_name target.uri target.loc;
  List.iter
    (fun d ->
      if not (nav_budget_check budget) then
        let graph = Macro_graph.of_document d in
        Macro_graph.uses_of_define graph target
        |> List.iter (fun (exp : Macro_graph.expansion) ->
               add_text_edit ~budget ~seen ~edits_by_uri ~new_name
                 exp.call_site_uri exp.call_name_loc))
    docs;
  if nav_budget_check budget then None else workspace_edit_of_edits edits_by_uri

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
          let macro_graph = Macro_graph.of_document doc in
          match
            macro_target_at_position doc macro_graph ~uri:doc.Document.uri ~pos
          with
          | Some (_, loc, placeholder) ->
              Some
                (`RangeWithPlaceholder
                   (Lsp_conv.range_of_loc loc, placeholder))
          | None -> (
              match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
              | Some (sym_id, sym_loc) ->
                  if unsafe_generated_symbol_at_position macro_graph sym_loc
                  then None
                  else
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
                      if key = "" || not (allow_fallback_for_ws ws doc) then
                        None
                      else
                        let docs = docs_for_rename ws doc in
                        if not (fallback_rename_safe docs key) then None
                        else
                          let has_any =
                            docs
                            |> List.exists (fun d ->
                                   if nav_budget_check budget then false
                                   else occurrences_in_doc d ~key <> [])
                          in
                          if nav_budget_check budget || not has_any then None
                          else
                            Some
                              (`RangeWithPlaceholder
                                 (Lsp_conv.range_of_loc word_loc, nm))))
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
            let macro_graph = Macro_graph.of_document doc in
            let seen = Hashtbl.create 1024 in
            let edits_by_uri : (T.DocumentUri.t, T.TextEdit.t list) Hashtbl.t =
              Hashtbl.create 128
            in
            let add_edit (u : T.DocumentUri.t) (loc : Ast.Loc.t) =
              add_text_edit ~budget ~seen ~edits_by_uri ~new_name u loc
            in
            let apply_changes () =
              workspace_edit_of_edits edits_by_uri
            in
            match
              macro_target_at_position doc macro_graph ~uri:doc.Document.uri
                ~pos
            with
            | Some (target, _, _) ->
                macro_rename_edit ~budget ~docs ~target ~new_name
            | None -> (
                match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
                | Some (sym_id, sym_loc) ->
                    if unsafe_generated_symbol_at_position macro_graph sym_loc
                    then None
                    else (
                      List.iter
                        (fun d ->
                          if not (nav_budget_check budget) then
                            let dnav = nav_for_doc_cached ws cache d in
                            match Hashtbl.find_opt dnav.occs_by_id sym_id with
                            | None -> ()
                            | Some xs ->
                                List.iter
                                  (fun (u, loc) ->
                                    if not (nav_budget_check budget) then
                                      add_edit u loc)
                                  xs)
                        docs;
                      if nav_budget_check budget then None else apply_changes ())
                | None -> (
                    match nav_word_at_position doc pos with
                    | None -> None
                    | Some (nm, _) ->
                        let key = normalize_name nm in
                        if key = "" || not (allow_fallback_for_ws ws doc) then
                          None
                        else if not (fallback_rename_safe docs key) then None
                        else (
                          List.iter
                            (fun d ->
                              if not (nav_budget_check budget) then
                                occurrences_in_doc d ~key
                                |> List.iter (fun (u, loc) ->
                                       if not (nav_budget_check budget) then
                                         add_edit u loc))
                            docs;
                          if nav_budget_check budget then None
                          else apply_changes ())))
        in
        let computed = nav_compute_with_budget_value budget compute in
        if Workspace_budget.exceeded budget then None else computed
