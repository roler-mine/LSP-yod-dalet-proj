(* Module overview: Workspace symbol search and streaming results. *)

module T = Lsp.Types
open Workspace_state
open Workspace_nav_model
open Workspace_navigation_support

let compare_symbol_defs (a : def) (b : def) : int =
  let ka = normalize_name a.name in
  let kb = normalize_name b.name in
  let c0 = String.compare ka kb in
  if c0 <> 0 then c0
  else
    let c1 =
      String.compare
        (Uri_path.docuri_to_string a.uri)
        (Uri_path.docuri_to_string b.uri)
    in
    if c1 <> 0 then c1
    else
      let c2 = compare a.loc.start_pos.line b.loc.start_pos.line in
      if c2 <> 0 then c2 else compare a.loc.start_pos.col b.loc.start_pos.col

let doc_sort_key (doc : Document.t) : string =
  Uri_path.docuri_to_string doc.Document.uri

let workspace_symbol_doc_stages (ws : t) : Document.t list * Document.t list =
  let seen = Hashtbl.create 512 in
  let open_docs = ref [] in
  Hashtbl.iter (fun _ doc -> add_doc_once seen open_docs doc) ws.docs;
  let workspace_docs = ref [] in
  Hashtbl.iter (fun _ doc -> add_doc_once seen workspace_docs doc) ws.files;
  let sort_docs docs =
    List.sort (fun a b -> String.compare (doc_sort_key a) (doc_sort_key b)) docs
  in
  (sort_docs (List.rev !open_docs), sort_docs (List.rev !workspace_docs))

let workspace_symbols_stream (ws : t) ~(query : string)
    ~(emit : T.SymbolInformation.t list -> unit) : T.SymbolInformation.t list =
  let budget = nav_budget_start ws in
  let max_items = 512 in
  let prefix = String.trim query in
  let compute () =
    if nav_budget_check budget then []
    else
      let open_docs, workspace_docs = workspace_symbol_doc_stages ws in
      let symbol_seen = Hashtbl.create 2048 in
      let count = ref 0 in
      let acc = ref [] in
      let collect_stage docs =
        let defs_rev = ref [] in
        List.iter
          (fun doc ->
            if not (nav_budget_check budget) then
              collect_doc_defs doc
              |> List.iter (fun d ->
                     if
                       (not (nav_budget_check budget))
                       && !count < max_items
                       && (starts_with_ci ~prefix d.name
                          || starts_with_ci ~prefix d.key)
                     then
                       let key =
                         Printf.sprintf "%s|%d|%d|%d"
                           (loc_key ~uri:d.uri d.loc)
                           d.kind d.loc.start_pos.line d.loc.start_pos.col
                       in
                       if not (Hashtbl.mem symbol_seen key) then (
                         Hashtbl.replace symbol_seen key true;
                         defs_rev := d :: !defs_rev;
                         incr count)))
          docs;
        let batch =
          List.rev !defs_rev |> List.sort compare_symbol_defs
          |> List.map symbol_info_of_def
        in
        if batch <> [] then (
          emit batch;
          acc := !acc @ batch)
      in
      collect_stage open_docs;
      collect_stage workspace_docs;
      !acc
  in
  nav_compute_with_budget_value budget compute

let workspace_symbols_for (ws : t) ~(query : string) :
    T.SymbolInformation.t list =
  workspace_symbols_stream ws ~query ~emit:(fun _ -> ())
