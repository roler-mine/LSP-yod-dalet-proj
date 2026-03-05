module T = Lsp.Types
open Ast
open Workspace_foundation

let symbol_key_at_position (doc:Document.t) (pos:T.Position.t) : string option =
  match nav_word_at_position doc pos with
  | None -> None
  | Some (nm, _) ->
      let key = normalize_name nm in
      if key = "" then None else Some key

let is_nonempty_location_list_json (j:Yojson.Safe.t) : bool =
  match j with
  | `List (_ :: _) -> true
  | _ -> false

let is_empty_location_list_json (j:Yojson.Safe.t) : bool =
  match j with
  | `List [] -> true
  | _ -> false

let is_nav_lookup_miss_json (j:Yojson.Safe.t) : bool =
  match j with
  | `Null -> true
  | _ -> is_empty_location_list_json j

let is_nonempty_hover_json (j:Yojson.Safe.t) : bool =
  match j with
  | `Null -> false
  | _ -> true

let request_pos_suffix (pos:T.Position.t) : string =
  Printf.sprintf "@%d:%d" pos.line pos.character

let adjust_nav_position (doc:Document.t) (pos:T.Position.t) : T.Position.t =
  match nav_word_at_position doc pos with
  | Some _ -> pos
  | None when pos.character > 0 ->
      let prev = { pos with T.Position.character = pos.character - 1 } in
      (match nav_word_at_position doc prev with
       | Some _ -> prev
       | None -> pos)
  | None -> pos

type nav_budget = {
  ws : t;
  deadline_ms : float;
  mutable exceeded : bool;
  mutable cancelled : bool;
  mutable cancel_recorded : bool;
}

let nav_budget_start (ws:t) : nav_budget =
  {
    ws;
    deadline_ms = Perf_stats.now_ms () +. float_of_int nav_soft_budget_ms;
    exceeded = false;
    cancelled = false;
    cancel_recorded = false;
  }

let nav_budget_check (budget:nav_budget) : bool =
  if budget.cancelled then true
  else if request_cancelled budget.ws then (
    budget.cancelled <- true;
    if not budget.cancel_recorded then (
      budget.cancel_recorded <- true;
      Perf_stats.tick "cancel.applied"
    );
    true
  ) else if budget.exceeded then true
  else if Perf_stats.now_ms () > budget.deadline_ms then (
    budget.exceeded <- true;
    Perf_stats.tick "nav.soft_budget_exceeded";
    true
  ) else
    false

let nav_compute_with_budget (budget:nav_budget) (f:unit -> Yojson.Safe.t) : Yojson.Safe.t =
  let out = f () in
  ignore (nav_budget_check budget);
  out

let nav_cache_write_allowed_for_request (ws:t) ~(uri:T.DocumentUri.t) : bool =
  (not (index_reconcile_pending_for_report ws))
  && open_doc_converged ws ~uri
  && Hashtbl.length ws.open_parse_generation = 0
  && Queue.is_empty ws.open_diag_revalidate_updates
  && ws.startup_fully_nav_ready_ms <> None
  && Queue.is_empty ws.bg_high_large_queue

let allow_fallback_for_ws (ws:t) (doc:Document.t) : bool =
  allow_unscoped_fallback doc
  || ws.startup_fully_nav_ready_ms = None

type nav_cache_hit =
  | CacheExact of Yojson.Safe.t
  | CacheSymbol of Yojson.Safe.t

let has_unique_semantic_symbol_for_key (ws:t) ~(key:string) : bool =
  ws.sem_store_enabled
  &&
  match Semantic_store.sym_ids_for_key ws.semantic_store ~key with
  | [ _ ] -> true
  | _ -> false

let nav_cache_get
    (ws:t)
    ~(kind_exact:string)
    ~(kind_symbol:string)
    ~(uri:T.DocumentUri.t)
    ~(symbol_key:string)
  : nav_cache_hit option =
  match nav_response_cache_get ws ~kind:kind_exact ~uri ~symbol_key with
  | Some payload -> Some (CacheExact payload)
  | None ->
      if not (has_unique_semantic_symbol_for_key ws ~key:symbol_key) then None
      else
        match nav_response_cache_get ws ~kind:kind_symbol ~uri ~symbol_key with
        | Some payload -> Some (CacheSymbol payload)
        | None -> None

let nav_cache_put
    (ws:t)
    ~(kind_exact:string)
    ~(kind_symbol:string)
    ~(uri:T.DocumentUri.t)
    ~(symbol_key:string)
    ~(payload:Yojson.Safe.t)
  : unit =
  nav_response_cache_put ws ~kind:kind_exact ~uri ~symbol_key ~payload;
  if has_unique_semantic_symbol_for_key ws ~key:symbol_key then
    nav_response_cache_put ws ~kind:kind_symbol ~uri ~symbol_key ~payload

let definition_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let definition_for_pos (pos:T.Position.t) : Yojson.Safe.t =
        let pos = adjust_nav_position doc pos in
        let budget = nav_budget_start ws in
        let compute () =
          if nav_budget_check budget then `Null
          else
            match import_under_cursor doc pos with
            | Some imp ->
                let hits = defs_for_import_cursor ws imp in
                if hits = [] then `Null else `List (List.map def_to_loc_json hits)
            | None ->
                let startup_quick_hits =
                  match nav_word_at_position doc pos with
                  | None -> []
                  | Some (nm, _) ->
                      let key = normalize_name nm in
                      if key = "" then [] else proc_impl_defs_by_key ws doc ~key
                in
                if startup_quick_hits <> [] then
                  `List (List.map def_to_loc_json startup_quick_hits)
                else
                let allow_fallback = allow_fallback_for_ws ws doc in
                (match define_under_cursor doc pos with
                 | Some (dm, _) ->
                     let d = def_of_preprocess_define doc dm in
                     `List [ def_to_loc_json d ]
                 | None ->
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
                let hit =
                  match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
                  | None -> None
                  | Some (sym_id, _) ->
                      (match Hashtbl.find_opt nav.defs_by_id sym_id with
                       | Some _ as d0 -> d0
                       | None ->
                           if ws.sem_store_enabled then
                             (match Semantic_store.defs_for_sym_id ws.semantic_store sym_id with
                              | d0 :: _ -> Some (def_of_snapshot_def d0)
                              | [] ->
                                  if nav_budget_check budget then None
                                  else find_def_for_sym_id ws cache ~docs:(docs_for_symbol ()) ~sym_id)
                           else if nav_budget_check budget then None
                           else find_def_for_sym_id ws cache ~docs:(docs_for_symbol ()) ~sym_id)
                in
                (match hit with
                 | Some d ->
                     let defs =
                       if d.kind = sym_kind_func && not (is_likely_proc_implementation ws d)
                       then
                         let impls = proc_impl_defs_by_key ws doc ~key:d.key in
                         if impls = [] then [ d ] else impls
                       else
                         [ d ]
                     in
                     `List (List.map def_to_loc_json defs)
                 | None ->
                     if nav_budget_check budget then `Null
                     else
                       let proc_by_name =
                         if not allow_fallback then []
                         else
                           match nav_word_at_position doc pos with
                           | None -> []
                           | Some (nm, _) ->
                               let key = normalize_name nm in
                               if key = "" then [] else proc_impl_defs_by_key ws doc ~key
                       in
                       if proc_by_name <> [] then
                         `List (List.map def_to_loc_json proc_by_name)
                       else if not allow_fallback || nav_budget_check budget then `Null
                       else
                         let by_name =
                           match nav_word_at_position doc pos with
                           | None -> []
                           | Some (nm, _) -> fallback_defs_by_name ws doc (normalize_name nm)
                         in
                         if by_name = [] then `Null
                         else `List (List.map def_to_loc_json by_name)))
        in
        match symbol_key_at_position doc pos with
        | None ->
            nav_compute_with_budget budget compute
        | Some key ->
            let kind_exact = "definition" ^ request_pos_suffix pos in
            let kind_symbol = "definition:symbol" in
            (match nav_cache_get ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key with
             | Some (CacheExact cached) ->
                 Perf_stats.tick "nav.response_cache_hit";
                 cached
             | Some (CacheSymbol cached) ->
                 Perf_stats.tick "nav.response_cache_symbol_hit";
                 cached
             | None ->
                 Perf_stats.tick "nav.response_cache_miss";
                 let computed = nav_compute_with_budget budget compute in
                 if is_nav_lookup_miss_json computed then
                   schedule_nav_miss_reconcile ws ~doc ~symbol_key:key;
                 if (not budget.exceeded)
                    && nav_cache_write_allowed_for_request ws ~uri
                    && is_nonempty_location_list_json computed
                 then
                   nav_cache_put ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key ~payload:computed;
                 computed)
      in
      let primary = definition_for_pos pos in
      if is_nav_lookup_miss_json primary && pos.character > 0 then
        definition_for_pos { pos with T.Position.character = pos.character - 1 }
      else
        primary

let implementation_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `Null
        else
          (match import_under_cursor doc pos with
           | Some _ ->
               definition_json_for ws ~uri ~pos
           | None ->
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
                 match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
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
                   let impls = List.filter (is_likely_proc_implementation ws) defs in
                   if impls = [] then [] else impls
               in
               let key_opt =
                 match defs with
                 | d :: _ when d.key <> "" -> Some d.key
                 | _ ->
                     (match nav_word_at_position doc pos with
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
                       if allow_fallback_for_ws ws doc && not (nav_budget_check budget)
                       then proc_impl_defs_by_key ws doc ~key
                       else []
               in
               if defs = [] then definition_json_for ws ~uri ~pos
               else `List (List.map def_to_loc_json defs))
      in
      match symbol_key_at_position doc pos with
      | None ->
          nav_compute_with_budget budget compute
      | Some key ->
          let kind_exact = "implementation" ^ request_pos_suffix pos in
          let kind_symbol = "implementation:symbol" in
          (match nav_cache_get ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key with
           | Some (CacheExact cached) ->
               Perf_stats.tick "nav.response_cache_hit";
               cached
           | Some (CacheSymbol cached) ->
               Perf_stats.tick "nav.response_cache_symbol_hit";
               cached
           | None ->
               Perf_stats.tick "nav.response_cache_miss";
               let computed = nav_compute_with_budget budget compute in
               if is_nav_lookup_miss_json computed then
                 schedule_nav_miss_reconcile ws ~doc ~symbol_key:key;
               if (not budget.exceeded)
                  && nav_cache_write_allowed_for_request ws ~uri
                  && is_nonempty_location_list_json computed
               then
                 nav_cache_put ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key ~payload:computed;
               computed)

let occurrences_in_doc_fallback (doc:Document.t) ~(key:string) : (T.DocumentUri.t * Ast.Loc.t) list =
  let text = doc.Document.text in
  let n = String.length text in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if is_ident_char text.[i] && (i = 0 || not (is_ident_char text.[i - 1])) then
      let j = ref (i + 1) in
      while !j < n && is_ident_char text.[!j] do incr j done;
      let tok = String.sub text i (!j - i) in
      let acc =
        if normalize_name tok = key then
          let loc = loc_of_offsets ~file:doc.Document.file ~idx:doc.Document.index ~s:i ~e:!j in
          (doc.Document.uri, loc) :: acc
        else
          acc
      in
      loop !j acc
    else
      loop (i + 1) acc
  in
  loop 0 []

let occurrences_in_doc (doc:Document.t) ~(key:string) : (T.DocumentUri.t * Ast.Loc.t) list =
  try
    Lexer.with_session_state (fun () ->
      let lexbuf = Lexing.from_string doc.Document.text in
      (match doc.Document.file with
       | None -> ()
       | Some f ->
           lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
      let rec loop acc =
        let tok = Lexer.token lexbuf in
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        match tok with
        | Parser.EOF ->
            List.rev acc
        | Parser.ID s when normalize_name s = key ->
            let loc = Ast.Loc.of_lexing_positions sp ep ~file:doc.Document.file in
            loop ((doc.Document.uri, loc) :: acc)
        | _ ->
            loop acc
      in
      loop []
    )
  with _ ->
    occurrences_in_doc_fallback doc ~key

let occurrences_for_docs_with_budget
    (budget:nav_budget)
    (docs:Document.t list)
    ~(key:string)
  : (T.DocumentUri.t * Ast.Loc.t) list =
  let acc = ref [] in
  List.iter (fun d ->
    if not (nav_budget_check budget) then
      acc := List.rev_append (occurrences_in_doc d ~key) !acc
  ) docs;
  List.rev !acc

let locations_json_with_budget
    (budget:nav_budget)
    (occs:(T.DocumentUri.t * Ast.Loc.t) list)
  : Yojson.Safe.t list =
  let seen = Hashtbl.create 256 in
  let out = ref [] in
  List.iter (fun (u, loc) ->
    if not (nav_budget_check budget) then (
      let k = loc_key ~uri:u loc in
      if not (Hashtbl.mem seen k) then (
        Hashtbl.add seen k true;
        out := location_json ~uri:u loc :: !out
      )
    )
  ) occs;
  List.rev !out

let references_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) ~(include_decl:bool) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `List []
        else
          match import_under_cursor doc pos with
          | Some imp ->
              let key = normalize_name imp.name in
              if key = "" then `List []
              else
                let docs = docs_for_lookup ws doc in
                let defs =
                  docs
                  |> List.concat_map collect_doc_defs
                  |> List.filter (fun d -> d.key = key)
                in
                let def_keys =
                  let h = Hashtbl.create 32 in
                  List.iter (fun d -> Hashtbl.replace h (loc_key ~uri:d.uri d.loc) true) defs;
                  h
                in
                let occs = occurrences_for_docs_with_budget budget docs ~key in
                let occs =
                  if include_decl then occs
                  else
                    List.filter (fun (u, loc) -> not (Hashtbl.mem def_keys (loc_key ~uri:u loc))) occs
                in
                `List (locations_json_with_budget budget occs)
          | None ->
              let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
              let nav = nav_for_doc_cached ws cache doc in
              let resolved =
                symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos
              in
              (match resolved with
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
                       ( Semantic_store.defs_for_sym_id ws.semantic_store sym_id
                         |> List.map def_of_snapshot_def
                         |> uniq_defs,
                         Semantic_store.refs_for_sym_id ws.semantic_store sym_id )
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
                              match Hashtbl.find_opt dnav.occs_by_id sym_id with
                              | None -> []
                              | Some xs -> xs) )
                   in
                   let decl_keys = Hashtbl.create 8 in
                   List.iter (fun defn ->
                     Hashtbl.replace decl_keys (loc_key ~uri:defn.uri defn.loc) true
                   ) sym_defs;
                   let occs =
                     if include_decl then base_occs
                     else
                       List.filter (fun (u, loc) ->
                         not (Hashtbl.mem decl_keys (loc_key ~uri:u loc))
                       ) base_occs
                   in
                   `List (locations_json_with_budget budget occs)
               | None ->
                   (match nav_word_at_position doc pos with
                    | None -> `List []
                    | Some (nm, _) ->
                        let key = normalize_name nm in
                        if key = "" then `List []
                        else
                          let docs = docs_for_rename ws doc in
                          let proc_defs =
                            if allow_fallback_for_ws ws doc then proc_defs_by_key ws doc ~key
                            else []
                          in
                          let defs =
                            if proc_defs <> [] then proc_defs
                            else if allow_fallback_for_ws ws doc then
                              docs
                              |> List.concat_map collect_doc_defs
                              |> List.filter (fun d -> d.key = key)
                            else
                              []
                          in
                          if defs = [] then `List []
                          else
                            let def_keys =
                              let h = Hashtbl.create 32 in
                              List.iter (fun d -> Hashtbl.replace h (loc_key ~uri:d.uri d.loc) true) defs;
                              h
                            in
                            let occs = occurrences_for_docs_with_budget budget docs ~key in
                            let occs =
                              if include_decl then occs
                              else
                                List.filter
                                  (fun (u, loc) -> not (Hashtbl.mem def_keys (loc_key ~uri:u loc)))
                                  occs
                            in
                            `List (locations_json_with_budget budget occs)))
      in
      match symbol_key_at_position doc pos with
      | None ->
          nav_compute_with_budget budget compute
      | Some key ->
          let kind_exact =
            (if include_decl then "references:withDecl" else "references:noDecl")
            ^ request_pos_suffix pos
          in
          let kind_symbol =
            if include_decl then "references:withDecl:symbol"
            else "references:noDecl:symbol"
          in
          (match nav_cache_get ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key with
           | Some (CacheExact cached) ->
               Perf_stats.tick "nav.response_cache_hit";
               cached
           | Some (CacheSymbol cached) ->
               Perf_stats.tick "nav.response_cache_symbol_hit";
               cached
           | None ->
               Perf_stats.tick "nav.response_cache_miss";
               let computed = nav_compute_with_budget budget compute in
               if is_nav_lookup_miss_json computed then
                 schedule_nav_miss_reconcile ws ~doc ~symbol_key:key;
               if (not budget.exceeded)
                  && nav_cache_write_allowed_for_request ws ~uri
                  && is_nonempty_location_list_json computed
               then
                 nav_cache_put ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key ~payload:computed;
               computed)

let hover_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `Null
        else
          let import_text =
            match import_under_cursor doc pos with
            | None -> None
            | Some imp ->
                let resolved =
                  match ws.index with
                  | None -> None
                  | Some idx -> Workspace_index.find_compool idx ~name:imp.name
                in
                Some
                  (match resolved with
                   | None -> Printf.sprintf "COMPOOL `%s` (unresolved in workspace index)." imp.name
                   | Some p -> Printf.sprintf "COMPOOL `%s` -> `%s`" imp.name (Filename.basename p))
          in
          (match define_under_cursor doc pos with
           | Some (dm, word_loc) ->
               let d = def_of_preprocess_define doc dm in
               let primary_decl =
                 match source_line_for_def ws d with
                 | Some line when String.trim line <> "" -> line
                 | _ ->
                     if dm.requires_call then
                       Printf.sprintf "DEFINE %s(%s) \"%s\";"
                         dm.name
                         (String.concat "," dm.formals)
                         dm.body
                     else
                       Printf.sprintf "DEFINE %s \"%s\";" dm.name dm.body
               in
               let head =
                 Printf.sprintf "### define `%s`\nDefined at `%s`"
                   d.name
                   (file_line_of_def d)
               in
               let decl_block =
                 let line = truncate_text 280 primary_decl in
                 if String.trim line = "" then ""
                 else Printf.sprintf "\n```jovial\n%s\n```" line
               in
               let expansion =
                 if dm.formals = [] then
                   Printf.sprintf "\nExpands to: `%s`" (truncate_text 180 dm.body)
                 else
                   Printf.sprintf
                     "\nFormals: `%s`\nExpansion template: `%s`"
                     (String.concat ", " dm.formals)
                     (truncate_text 180 dm.body)
               in
               let body = head ^ decl_block ^ expansion in
               `Assoc [
                 "contents",
                 `Assoc [ "kind", `String "markdown"; "value", `String body ];
                 "range", range_json_of_loc word_loc;
               ]
           | None ->
          let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
          let nav = nav_for_doc_cached ws cache doc in
          let word = nav_word_at_position doc pos in
          let resolved = symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos in
          let defs, hover_loc =
            match resolved with
            | Some (sym_id, loc) ->
                let docs_cache : Document.t list option ref = ref None in
                let docs_for_symbol () =
                  match !docs_cache with
                  | Some xs -> xs
                  | None ->
                      let xs = docs_for_lookup ws doc in
                      docs_cache := Some xs;
                      xs
                in
                let defn =
                  match Hashtbl.find_opt nav.defs_by_id sym_id with
                  | Some _ as d0 -> d0
                  | None ->
                      if ws.sem_store_enabled then
                        (match Semantic_store.defs_for_sym_id ws.semantic_store sym_id with
                         | d0 :: _ -> Some (def_of_snapshot_def d0)
                         | [] ->
                             if nav_budget_check budget then None
                             else find_def_for_sym_id ws cache ~docs:(docs_for_symbol ()) ~sym_id)
                      else if nav_budget_check budget then None
                      else find_def_for_sym_id ws cache ~docs:(docs_for_symbol ()) ~sym_id
                in
                (match defn with
                 | Some d -> ([d], Some loc)
                 | None ->
                     (match word with
                      | None -> ([], Some loc)
                      | Some (nm, wloc) ->
                          let defs0 =
                            if allow_fallback_for_ws ws doc && not (nav_budget_check budget)
                            then fallback_defs_by_name ws doc (normalize_name nm)
                            else []
                          in
                          (defs0, Some wloc)))
            | None ->
                (match word with
                 | None -> ([], None)
                 | Some (nm, wloc) ->
                     let defs0 =
                       if allow_fallback_for_ws ws doc && not (nav_budget_check budget)
                       then fallback_defs_by_name ws doc (normalize_name nm)
                       else []
                     in
                     (defs0, Some wloc))
          in
          let defs =
            match defs with
            | d :: _ when d.kind = sym_kind_func && not (is_likely_proc_implementation ws d) ->
                let impls = proc_impl_defs_by_key ws doc ~key:d.key in
                if impls = [] then defs else impls
            | [] ->
                (match word with
                 | None -> []
                 | Some (nm, _) ->
                     let key = normalize_name nm in
                     if key = "" || not (allow_fallback_for_ws ws doc) || nav_budget_check budget
                     then []
                     else proc_impl_defs_by_key ws doc ~key)
            | _ ->
                defs
          in
          if defs = [] then
            (match import_text with
             | None ->
                 (match word with
                  | None -> `Null
                  | Some (nm, word_loc) ->
                      `Assoc [
                        "contents",
                        `Assoc [
                          "kind", `String "markdown";
                          "value",
                          `String (Printf.sprintf "No definition found for `%s` in current workspace scope." nm);
                        ];
                        "range", range_json_of_loc word_loc;
                      ])
             | Some txt ->
                 `Assoc [
                   "contents", `Assoc [ "kind", `String "markdown"; "value", `String txt ];
                 ])
          else
            let rec top_lines acc = function
              | [] -> List.rev acc
              | _ when nav_budget_check budget -> List.rev acc
              | d :: tl ->
                  let head =
                    Printf.sprintf "### %s `%s`\nDefined at `%s`"
                      (kind_name d.kind) d.name (file_line_of_def d)
                  in
                  let sig_line = proc_signature_for_def ws d in
                  let src_line = source_line_for_def ws d in
                  let primary_decl =
                    match sig_line with
                    | Some sig_line -> Some sig_line
                    | None -> src_line
                  in
                  let decl_block =
                    match primary_decl with
                    | None -> ""
                    | Some line ->
                        let line = truncate_text 280 line in
                        if String.trim line = "" then ""
                        else Printf.sprintf "\n```jovial\n%s\n```" line
                  in
                  let extra =
                    match src_line, sig_line with
                    | Some src, Some sig_line when String.trim src <> String.trim sig_line ->
                        let src = truncate_text 280 src in
                        if String.trim src = "" then ""
                        else Printf.sprintf "\nDeclaration:\n```jovial\n%s\n```" src
                    | _ -> ""
                  in
                  top_lines ((head ^ decl_block ^ extra) :: acc) tl
            in
            let top_lines = top_lines [] defs in
            let lines =
              match import_text with
              | None -> top_lines
              | Some imp -> imp :: "" :: top_lines
            in
            let body = String.concat "\n" lines in
            let base =
              [ "contents", `Assoc [ "kind", `String "markdown"; "value", `String body ] ]
            in
            let with_range =
              match hover_loc with
              | None -> base
              | Some loc -> ("range", range_json_of_loc loc) :: base
            in
            `Assoc with_range)
      in
      match symbol_key_at_position doc pos with
      | None ->
          nav_compute_with_budget budget compute
      | Some key ->
          let kind_exact = "hover" ^ request_pos_suffix pos in
          let kind_symbol = "hover:symbol" in
          (match nav_cache_get ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key with
           | Some (CacheExact cached) ->
               Perf_stats.tick "nav.response_cache_hit";
               cached
           | Some (CacheSymbol cached) ->
               Perf_stats.tick "nav.response_cache_symbol_hit";
               cached
           | None ->
               Perf_stats.tick "nav.response_cache_miss";
               let computed = nav_compute_with_budget budget compute in
               if (not budget.exceeded)
                  && nav_cache_write_allowed_for_request ws ~uri
                  && is_nonempty_hover_json computed
               then
                 nav_cache_put ws ~kind_exact ~kind_symbol ~uri ~symbol_key:key ~payload:computed;
               computed)

let prepare_rename_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `Null
        else
          let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
          let nav = nav_for_doc_cached ws cache doc in
          (match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
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
               if nav_budget_check budget || not has_any then `Null
               else
                 let placeholder =
                   match word_at_position doc pos with
                   | Some (nm, _) -> nm
                   | None ->
                       (match Hashtbl.find_opt nav.defs_by_id sym_id with
                        | Some d -> d.name
                        | None -> "name")
                 in
                 `Assoc [
                   "range", range_json_of_loc sym_loc;
                   "placeholder", `String placeholder;
                 ]
           | None ->
               (match nav_word_at_position doc pos with
                | None -> `Null
                | Some (nm, word_loc) ->
                    let key = normalize_name nm in
                    if key = "" || not (allow_fallback_for_ws ws doc) then `Null
                    else
                      let has_any =
                        docs_for_rename ws doc
                        |> List.exists (fun d ->
                             if nav_budget_check budget then false
                             else occurrences_in_doc d ~key <> [])
                      in
                      if nav_budget_check budget || not has_any then `Null
                      else
                        `Assoc [
                          "range", range_json_of_loc word_loc;
                          "placeholder", `String nm;
                        ]))
      in
      let computed = nav_compute_with_budget budget compute in
      if budget.exceeded then `Null else computed

let rename_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) ~(new_name:string) : Yojson.Safe.t =
  if not (is_valid_rename_name new_name) then `Null
  else
    match doc_of_uri ws uri with
    | None -> `Null
    | Some doc ->
        let budget = nav_budget_start ws in
        let compute () =
          if nav_budget_check budget then `Null
          else
            let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 64 in
            let nav = nav_for_doc_cached ws cache doc in
            let docs = docs_for_rename ws doc in
            let seen = Hashtbl.create 1024 in
            let edits_by_uri : (string, Yojson.Safe.t list) Hashtbl.t = Hashtbl.create 128 in
            let add_edit (u:T.DocumentUri.t) (loc:Ast.Loc.t) =
              if nav_budget_check budget then ()
              else
                let lk = loc_key ~uri:u loc in
                if not (Hashtbl.mem seen lk) then (
                  Hashtbl.add seen lk true;
                  let uri_s = Uri_path.docuri_to_string u in
                  let edit =
                    `Assoc [
                      "range", range_json_of_loc loc;
                      "newText", `String new_name;
                    ]
                  in
                  let prev =
                    match Hashtbl.find_opt edits_by_uri uri_s with
                    | None -> []
                    | Some xs -> xs
                  in
                  Hashtbl.replace edits_by_uri uri_s (edit :: prev)
                )
            in
            let apply_changes () =
              let changes =
                Hashtbl.fold (fun uri_s edits acc ->
                  (uri_s, `List (List.rev edits)) :: acc
                ) edits_by_uri []
              in
              if changes = [] then `Null
              else `Assoc [ "changes", `Assoc changes ]
            in
            (match symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos with
             | Some (sym_id, _) ->
                 List.iter (fun d ->
                   if not (nav_budget_check budget) then
                     let dnav = nav_for_doc_cached ws cache d in
                     match Hashtbl.find_opt dnav.occs_by_id sym_id with
                     | None -> ()
                     | Some xs ->
                         List.iter (fun (u, loc) ->
                           if not (nav_budget_check budget) then add_edit u loc
                         ) xs
                 ) docs;
                 if nav_budget_check budget then `Null else apply_changes ()
             | None ->
                 (match nav_word_at_position doc pos with
                  | None -> `Null
                  | Some (nm, _) ->
                      let key = normalize_name nm in
                      if key = "" || not (allow_fallback_for_ws ws doc) then `Null
                      else (
                        List.iter (fun d ->
                          if not (nav_budget_check budget) then
                            occurrences_in_doc d ~key
                            |> List.iter (fun (u, loc) ->
                                 if not (nav_budget_check budget) then add_edit u loc)
                        ) docs;
                        if nav_budget_check budget then `Null else apply_changes ()
                      )))
        in
        let computed = nav_compute_with_budget budget compute in
        if budget.exceeded then `Null else computed

let starts_with_ci ~(prefix:string) (s:string) : bool =
  let p = normalize_name prefix in
  if p = "" then true
  else
    let u = normalize_name s in
    let lp = String.length p in
    String.length u >= lp && String.sub u 0 lp = p

let completion_item_kind_of_def_kind (k:int) : int =
  if k = sym_kind_module then 9
  else if k = sym_kind_type then 7
  else if k = sym_kind_field then 10
  else if k = sym_kind_func then 3
  else if k = sym_kind_const then 21
  else 6

let completion_item_json
    ~(label:string)
    ~(kind:int)
    ?detail
    ?insert_text
    ?sort_text
    ()
  : Yojson.Safe.t =
  let fields =
    [
      ("label", `String label);
      ("kind", `Int kind);
    ]
  in
  let fields =
    match detail with
    | Some d when String.trim d <> "" -> ("detail", `String d) :: fields
    | _ -> fields
  in
  let fields =
    match insert_text with
    | Some txt when txt <> "" -> ("insertText", `String txt) :: fields
    | _ -> fields
  in
  let fields =
    match sort_text with
    | Some st when st <> "" -> ("sortText", `String st) :: fields
    | _ -> fields
  in
  `Assoc fields

let completion_keywords : (string * int * string option) list =
  [
    ("START", 14, None);
    ("TERM", 14, None);
    ("BEGIN", 14, None);
    ("END", 14, None);
    ("DEF", 14, None);
    ("REF", 14, None);
    ("STATIC", 14, None);
    ("CONSTANT", 14, None);
    ("PROC", 14, Some "procedure declaration");
    ("ITEM", 14, Some "item declaration");
    ("TABLE", 14, Some "table declaration");
    ("TYPE", 14, Some "type declaration");
    ("IF", 14, None);
    ("THEN", 14, None);
    ("ELSE", 14, None);
    ("WHILE", 14, None);
    ("FOR", 14, None);
    ("BY", 14, None);
    ("FALLTHRU", 14, None);
    ("RETURN", 14, None);
    ("GOTO", 14, None);
    ("EXIT", 14, None);
    ("ABORT", 14, None);
    ("STOP", 14, None);
    ("CASE", 14, None);
    ("DEFAULT", 14, None);
    ("COMPOOL", 14, Some "compool directive");
    ("ICOMPOOL", 14, Some "import compool directive");
    ("ICOPY", 14, Some "copy directive");
    ("ISKIP", 14, Some "skip directive");
    ("IBEGIN", 14, Some "directive scope begin");
    ("IEND", 14, Some "directive scope end");
    ("ILINKAGE", 14, Some "linkage directive");
    ("ITRACE", 14, Some "trace directive");
    ("IINTERFERENCE", 14, Some "interference directive");
    ("IREDUCIBLE", 14, Some "reducible directive");
    ("ILIST", 14, Some "listing directive");
    ("INOLIST", 14, Some "listing directive");
    ("IEJECT", 14, Some "listing directive");
    ("IBASE", 14, Some "base directive");
    ("IISBASE", 14, Some "base directive");
    ("IDROP", 14, Some "drop directive");
    ("ILEFTRIGHT", 14, Some "layout directive");
    ("IREARRANGE", 14, Some "rearrange directive");
    ("IINITIALIZE", 14, Some "initialize directive");
    ("IORDER", 14, Some "order directive");
    ("DEFINE", 14, Some "macro directive");
    ("PROGRAM", 14, Some "program directive");
    ("BLOCK", 14, Some "block directive");
    ("REC", 14, Some "recursive subroutine");
    ("RENT", 14, Some "reentrant subroutine");
    ("LISTEXP", 14, Some "define list option");
    ("LISTINV", 14, Some "define list option");
    ("LISTBOTH", 14, Some "define list option");
    ("INLINE", 14, Some "inline declaration");
    ("LABEL", 14, Some "statement-name declaration");
    ("LIKE", 14, Some "table type option");
    ("OVERLAY", 14, Some "overlay declaration");
    ("PARALLEL", 14, Some "table structure option");
    ("POS", 14, Some "preset positioner");
    ("INSTANCE", 14, Some "def block instance");
    ("NULL", 14, Some "pointer literal");
    ("TRUE", 14, None);
    ("FALSE", 14, None);
    ("MOD", 14, None);
    ("AND", 14, None);
    ("OR", 14, None);
    ("NOT", 14, None);
    ("XOR", 14, None);
    ("EQV", 14, None);
  ]

let completion_types_builtin : (string * int * string option) list =
  [
    ("A", 7, Some "fixed type indicator");
    ("B", 7, Some "built-in type");
    ("U", 7, Some "built-in type");
    ("S", 7, Some "built-in type");
    ("F", 7, Some "built-in type");
    ("C", 7, Some "built-in type");
    ("P", 7, Some "built-in type");
    ("W", 7, Some "compatibility type marker");
    ("V", 7, Some "compatibility/status marker");
    ("STATUS", 7, Some "built-in type");
  ]

let completion_functions_builtin : (string * int * string option) list =
  [
    ("ABS", 3, Some "built-in function");
    ("BIT", 3, Some "built-in function");
    ("BITSIZE", 3, Some "built-in function");
    ("BYTE", 3, Some "built-in function");
    ("BYTESIZE", 3, Some "built-in function");
    ("FIRST", 3, Some "built-in function");
    ("LAST", 3, Some "built-in function");
    ("LBOUND", 3, Some "built-in function");
    ("LOC", 3, Some "built-in function");
    ("NEXT", 3, Some "built-in function");
    ("NWDSEN", 3, Some "built-in function");
    ("REP", 3, Some "built-in function");
    ("SGN", 3, Some "built-in function");
    ("SHIFTL", 3, Some "built-in function");
    ("SHIFTR", 3, Some "built-in function");
    ("UBOUND", 3, Some "built-in function");
    ("V", 3, Some "built-in status constructor");
    ("WORDSIZE", 3, Some "built-in function");
  ]

let completion_snippets : (string * string * int * string option) list =
  [
    ("!COMPOOL", "!COMPOOL(\"COMP\");", 15, Some "import compool");
    ("!ICOMPOOL", "!ICOMPOOL(\"COMP\");", 15, Some "import compool");
  ]

let completion_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        let prefix =
          match word_at_position doc pos with
          | None -> ""
          | Some (nm, _) -> nm
        in
        let seen = Hashtbl.create 512 in
        let out = ref [] in
        let count = ref 0 in
        let max_items = 500 in
        let add_item ~(uniq_key:string) (item:Yojson.Safe.t) : unit =
          if nav_budget_check budget then ()
          else if !count < max_items && not (Hashtbl.mem seen uniq_key) then (
            Hashtbl.replace seen uniq_key true;
            out := item :: !out;
            incr count
          )
        in

        let add_symbol_item (d:def) : unit =
          if starts_with_ci ~prefix d.name then
            let detail =
              match d.container with
              | None -> Some (kind_name d.kind)
              | Some c -> Some (Printf.sprintf "%s in %s" (kind_name d.kind) c)
            in
            let kind = completion_item_kind_of_def_kind d.kind in
            let uniq_key = Printf.sprintf "sym|%s|%d" (normalize_name d.name) kind in
            let sort_text =
              if same_uri d.uri doc.Document.uri then Some ("0_" ^ normalize_name d.name)
              else Some ("1_" ^ normalize_name d.name)
            in
            add_item
              ~uniq_key
              (completion_item_json
                 ~label:d.name
                 ~kind
                 ?detail
                 ?sort_text
                 ())
        in

        let add_keyword (label:string) (kind:int) (detail:string option) : unit =
          if starts_with_ci ~prefix label then
            let uniq_key = "kw|" ^ normalize_name label in
            add_item
              ~uniq_key
              (completion_item_json
                 ~label
                 ~kind
                 ?detail
                 ~sort_text:("2_" ^ normalize_name label)
                 ())
        in

        let add_builtin_function (label:string) (kind:int) (detail:string option) : unit =
          if starts_with_ci ~prefix label then
            let uniq_key = "fn|" ^ normalize_name label in
            add_item
              ~uniq_key
              (completion_item_json
                 ~label
                 ~kind
                 ?detail
                 ~sort_text:("3_" ^ normalize_name label)
                 ())
        in

        let add_snippet (label:string) (insert_text:string) (kind:int) (detail:string option) : unit =
          if starts_with_ci ~prefix label || starts_with_ci ~prefix insert_text then
            let uniq_key = "snip|" ^ normalize_name label in
            add_item
              ~uniq_key
              (completion_item_json
                 ~label
                 ~kind
                 ?detail
                 ~insert_text
                 ~sort_text:("4_" ^ normalize_name label)
                 ())
        in

        docs_for_lookup ws doc
        |> List.iter (fun d ->
             if not (nav_budget_check budget) then
               collect_doc_defs d
               |> List.iter (fun defn ->
                    if not (nav_budget_check budget) then add_symbol_item defn));

        List.iter (fun (label, kind, detail) ->
          if not (nav_budget_check budget) then add_keyword label kind detail
        ) completion_keywords;
        List.iter (fun (label, kind, detail) ->
          if not (nav_budget_check budget) then add_keyword label kind detail
        ) completion_types_builtin;
        List.iter (fun (label, kind, detail) ->
          if not (nav_budget_check budget) then add_builtin_function label kind detail
        ) completion_functions_builtin;
        List.iter (fun (label, insert_text, kind, detail) ->
          if not (nav_budget_check budget) then add_snippet label insert_text kind detail
        ) completion_snippets;

        `List (List.rev !out)
      in
      nav_compute_with_budget budget compute

let declaration_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `Null
        else
          match nav_word_at_position doc pos with
          | Some (nm, _) ->
              let key = normalize_name nm in
              if key = "" then definition_json_for ws ~uri ~pos
              else
                let decls =
                  proc_defs_by_key ws doc ~key
                  |> List.filter (fun d -> not (is_likely_proc_implementation ws d))
                in
                if decls = [] then definition_json_for ws ~uri ~pos
                else `List (List.map def_to_loc_json (uniq_defs decls))
          | None ->
              definition_json_for ws ~uri ~pos
      in
      nav_compute_with_budget budget compute

let type_definition_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `Null
        else
          let key_opt =
            match nav_word_at_position doc pos with
            | None -> None
            | Some (nm, _) ->
                let key = normalize_name nm in
                if key = "" then None else Some key
          in
          match key_opt with
          | None -> `Null
          | Some key ->
              let docs = docs_for_lookup ws doc in
              let defs =
                docs
                |> List.concat_map collect_doc_defs
                |> List.filter (fun d -> d.kind = sym_kind_type && d.key = key)
                |> uniq_defs
              in
              if defs = [] then `Null
              else `List (List.map def_to_loc_json defs)
      in
      nav_compute_with_budget budget compute

let doc_sort_key (doc:Document.t) : string =
  Uri_path.docuri_to_string doc.Document.uri

let workspace_symbols_json_for (ws:t) ~(query:string) : Yojson.Safe.t =
  let budget = nav_budget_start ws in
  let max_items = 512 in
  let prefix = String.trim query in
  let compute () =
    if nav_budget_check budget then `List []
    else
      let docs_seen = Hashtbl.create 512 in
      let docs = ref [] in
      let add_doc (d:Document.t) : unit =
        if nav_budget_check budget then ()
        else
          let k = Uri_path.docuri_to_string d.Document.uri in
          if not (Hashtbl.mem docs_seen k) then (
            Hashtbl.replace docs_seen k true;
            docs := d :: !docs
          )
      in
      Hashtbl.iter (fun _ d -> add_doc d) ws.docs;
      Hashtbl.iter (fun _ d -> add_doc d) ws.files;
      let docs =
        !docs
        |> List.sort (fun a b -> String.compare (doc_sort_key a) (doc_sort_key b))
      in
      let symbol_seen = Hashtbl.create 2048 in
      let out = ref [] in
      let count = ref 0 in
      let add_symbol (d:def) : unit =
        if nav_budget_check budget || !count >= max_items then ()
        else if starts_with_ci ~prefix d.name || starts_with_ci ~prefix d.key then
          let key =
            Printf.sprintf "%s|%d|%d|%d"
              (loc_key ~uri:d.uri d.loc)
              d.kind
              d.loc.start_pos.line
              d.loc.start_pos.col
          in
          if not (Hashtbl.mem symbol_seen key) then (
            Hashtbl.replace symbol_seen key true;
            out := d :: !out;
            incr count
          )
      in
      List.iter (fun d ->
        if not (nav_budget_check budget) then
          collect_doc_defs d |> List.iter add_symbol
      ) docs;
      let sorted =
        List.rev !out
        |> List.sort (fun a b ->
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
                 if c2 <> 0 then c2
                 else compare a.loc.start_pos.col b.loc.start_pos.col)
      in
      `List (List.map symbol_json sorted)
  in
  nav_compute_with_budget budget compute

let split_signature_params (label:string) : string list =
  match String.index_opt label '(' with
  | None -> []
  | Some i0 ->
      (match String.rindex_opt label ')' with
       | None -> []
       | Some i1 when i1 <= i0 -> []
       | Some i1 ->
           let inside = String.sub label (i0 + 1) (i1 - i0 - 1) |> String.trim in
           if inside = "" then []
           else
             inside
             |> String.split_on_char ','
             |> List.map String.trim
             |> List.filter (fun s -> s <> ""))

let signature_params_json (label:string) : Yojson.Safe.t =
  split_signature_params label
  |> List.map (fun p -> `Assoc [ "label", `String p ])
  |> fun xs -> `List xs

let call_context_at_position (doc:Document.t) (pos:T.Position.t) : (string * int) option =
  match Text_index.offset_of_line_col doc.Document.index ~line:pos.line ~col:pos.character with
  | None -> None
  | Some raw_cursor ->
      let text = doc.Document.text in
      let n = String.length text in
      let cursor = max 0 (min n raw_cursor) in
      let stack : int list ref = ref [] in
      let in_single = ref false in
      let in_double = ref false in
      let i = ref 0 in
      while !i < cursor do
        let c = text.[!i] in
        if !in_single then (
          if c = '\'' then
            if !i + 1 < cursor && text.[!i + 1] = '\'' then
              i := !i + 1
            else
              in_single := false
        ) else if !in_double then (
          if c = '"' then
            if !i + 1 < cursor && text.[!i + 1] = '"' then
              i := !i + 1
            else
              in_double := false
        ) else (
          match c with
          | '\'' -> in_single := true
          | '"' -> in_double := true
          | '(' -> stack := !i :: !stack
          | ')' ->
              (match !stack with
               | _ :: tl -> stack := tl
               | [] -> ())
          | _ -> ()
        );
        incr i
      done;
      match !stack with
      | [] -> None
      | open_idx :: _ ->
          let rec skip_ws_left j =
            if j < 0 then -1
            else
              match text.[j] with
              | ' ' | '\t' | '\r' | '\n' -> skip_ws_left (j - 1)
              | _ -> j
          in
          let j0 = skip_ws_left (open_idx - 1) in
          if j0 < 0 then None
          else
            let rec start_ident j =
              if j >= 0 && is_ident_char text.[j] then start_ident (j - 1) else j + 1
            in
            let start = start_ident j0 in
            if start > j0 then None
            else
              let name = String.sub text start (j0 - start + 1) in
              let key = normalize_name name in
              if key = "" then None
              else
                let depth = ref 1 in
                let in_single = ref false in
                let in_double = ref false in
                let commas = ref 0 in
                let k = ref (open_idx + 1) in
                while !k < cursor do
                  let c = text.[!k] in
                  if !in_single then (
                    if c = '\'' then
                      if !k + 1 < cursor && text.[!k + 1] = '\'' then
                        k := !k + 1
                      else
                        in_single := false
                  ) else if !in_double then (
                    if c = '"' then
                      if !k + 1 < cursor && text.[!k + 1] = '"' then
                        k := !k + 1
                      else
                        in_double := false
                  ) else (
                    match c with
                    | '\'' -> in_single := true
                    | '"' -> in_double := true
                    | '(' -> incr depth
                    | ')' -> if !depth > 0 then decr depth
                    | ',' when !depth = 1 -> incr commas
                    | _ -> ()
                  );
                  incr k
                done;
                Some (key, max 0 !commas)

let signature_help_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `Null
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then `Null
        else
          match call_context_at_position doc pos with
          | None -> `Null
          | Some (key, active_param) ->
              let defs =
                if nav_budget_check budget then []
                else
                  let docs = docs_for_lookup ws doc in
                  let from_docs =
                    docs
                    |> List.concat_map collect_doc_defs
                    |> List.filter (fun d -> d.kind = sym_kind_func && d.key = key)
                    |> uniq_defs
                  in
                  if from_docs <> [] then from_docs
                  else if allow_fallback_for_ws ws doc then proc_defs_by_key ws doc ~key
                  else []
              in
              if defs = [] then `Null
              else
                let signatures =
                  defs
                  |> List.filter_map (fun d ->
                       if nav_budget_check budget then None
                       else
                         match proc_signature_for_def ws d with
                         | Some label ->
                             Some
                               (`Assoc [
                                  "label", `String label;
                                  "parameters", signature_params_json label;
                                ])
                         | None ->
                             Some
                               (`Assoc [
                                  "label", `String d.name;
                                  "parameters", `List [];
                                ]))
                in
                if signatures = [] then `Null
                else
                  `Assoc [
                    "signatures", `List signatures;
                    "activeSignature", `Int 0;
                    "activeParameter", `Int active_param;
                  ]
      in
      nav_compute_with_budget budget compute

let range_intersects (a:T.Range.t) (b:T.Range.t) : bool =
  compare_pos a.start b.end_ <= 0 && compare_pos b.start a.end_ <= 0

let range_pos_json (p:T.Position.t) : Yojson.Safe.t =
  `Assoc [ "line", `Int p.line; "character", `Int p.character ]

let range_zero_json (p:T.Position.t) : Yojson.Safe.t =
  `Assoc [ "start", range_pos_json p; "end", range_pos_json p ]

let workspace_single_edit_json ~(uri:T.DocumentUri.t) ~(pos:T.Position.t) ~(new_text:string) : Yojson.Safe.t =
  let uri_s = Uri_path.docuri_to_string uri in
  `Assoc [
    "changes",
    `Assoc [
      (uri_s,
       `List [
         `Assoc [
           "range", range_zero_json pos;
           "newText", `String new_text;
         ]
       ])
    ];
  ]

let parse_missing_compool_name (msg:string) : string option =
  let prefix = "Missing COMPOOL:" in
  let lp = String.length prefix in
  if String.length msg < lp || String.sub msg 0 lp <> prefix then None
  else
    let name = String.trim (String.sub msg lp (String.length msg - lp)) in
    let k = normalize_name name in
    if k = "" then None else Some k

let find_substring_index ~(haystack:string) ~(needle:string) : int option =
  let hn = String.length haystack in
  let nn = String.length needle in
  if nn = 0 || hn < nn then None
  else
    let rec loop i =
      if i + nn > hn then None
      else if String.sub haystack i nn = needle then Some i
      else loop (i + 1)
    in
    loop 0

let parse_compool_name_from_hint (msg:string) : string option =
  let upper = String.uppercase_ascii msg in
  match find_substring_index ~haystack:upper ~needle:"COMPOOL " with
  | None -> None
  | Some i ->
      let j0 = i + String.length "COMPOOL " in
      let n = String.length upper in
      let rec skip j =
        if j >= n then n
        else
          match upper.[j] with
          | ' ' | '\t' | '\'' | '"' | '`' -> skip (j + 1)
          | _ -> j
      in
      let rec take j =
        if j >= n then j
        else
          match upper.[j] with
          | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> take (j + 1)
          | _ -> j
      in
      let s = skip j0 in
      let e = take s in
      if e <= s then None
      else
        let name = String.sub upper s (e - s) in
        let k = normalize_name name in
        if k = "" then None else Some k

let import_insert_position (doc:Document.t) : T.Position.t * bool =
  let idx = doc.Document.index in
  let line_count = Text_index.line_count idx in
  let line_max =
    Document.imports doc
    |> List.fold_left (fun acc (imp:Preprocess.import) ->
         let line0 = max 0 (imp.loc.start_pos.line - 1) in
         if line0 > acc then line0 else acc)
         (-1)
  in
  if line_max >= 0 && line_count > 0 then
    let line = min line_max (line_count - 1) in
    let ch =
      match Text_index.line_length idx ~line with
      | Some n -> max 0 n
      | None -> 0
    in
    ((({ line; character = ch } : T.Position.t)), true)
  else
    ((({ line = 0; character = 0 } : T.Position.t)), false)

let has_import_for_compool (doc:Document.t) (name:string) : bool =
  let key = normalize_name name in
  Document.imports doc
  |> List.exists (fun (imp:Preprocess.import) -> normalize_name imp.name = key)

let quickfix_add_import_action
    ~(doc:Document.t)
    ~(diag:T.Diagnostic.t)
    ~(compool:string)
  : Yojson.Safe.t option =
  let key = normalize_name compool in
  if key = "" || has_import_for_compool doc key then None
  else
    let pos, append_after_line = import_insert_position doc in
    let text =
      if append_after_line then Printf.sprintf "\n!COMPOOL(\"%s\");" key
      else Printf.sprintf "!COMPOOL(\"%s\");\n" key
    in
    Some
      (`Assoc [
         "title", `String (Printf.sprintf "Import COMPOOL %s" key);
         "kind", `String "quickfix";
         "isPreferred", `Bool true;
         "diagnostics", `List [ T.Diagnostic.yojson_of_t diag ];
         "edit", workspace_single_edit_json ~uri:doc.uri ~pos ~new_text:text;
       ])

let code_actions_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(range:T.Range.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let diag_in_range (d:T.Diagnostic.t) : bool =
        range_intersects d.range range
      in
      let seen = Hashtbl.create 32 in
      let actions = ref [] in
      let add_action (key:string) (action:Yojson.Safe.t option) =
        if not (Hashtbl.mem seen key) then
          match action with
          | None -> ()
          | Some a ->
              Hashtbl.replace seen key true;
              actions := a :: !actions
      in
      Document.diagnostics doc
      |> List.filter diag_in_range
      |> List.iter (fun (d:T.Diagnostic.t) ->
           let msg =
             match d.message with
             | `String s -> s
             | `MarkupContent mc -> mc.value
           in
           let compool_opt =
             match parse_missing_compool_name msg with
             | Some c -> Some c
             | None -> parse_compool_name_from_hint msg
           in
           match compool_opt with
           | None -> ()
           | Some c ->
               add_action
                 ("import|" ^ normalize_name c)
                 (quickfix_add_import_action ~doc ~diag:d ~compool:c));
      `List (List.rev !actions)

let collect_proc_param_map (doc:Document.t) (out:(string, string list) Hashtbl.t) : unit =
  let rec add_expr (e:Ast.expr Ast.node) : unit =
    match e.v with
    | Ast.EName _ | Ast.ELit _ -> ()
    | Ast.EUnop { rhs; _ } -> add_expr rhs
    | Ast.EBinop { lhs; rhs; _ } -> add_expr lhs; add_expr rhs
    | Ast.ECall { args; _ } -> List.iter add_expr args
    | Ast.EIndex { base; index } -> add_expr base; List.iter add_expr index
    | Ast.EField { base; _ } -> add_expr base
    | Ast.EAt { field; ptr } -> add_expr field; add_expr ptr
    | Ast.EDeref { ptr } -> add_expr ptr
    | Ast.EParen x -> add_expr x
  in
  let rec add_stmt (s:Ast.stmt Ast.node) : unit =
    match s.v with
    | Ast.SEmpty | Ast.SGoto _ -> ()
    | Ast.SDecl d -> add_decl d
    | Ast.SBlock xs -> List.iter add_stmt xs
    | Ast.SAssign { lhs; rhs } -> add_expr lhs; add_expr rhs
    | Ast.SCallStmt { args; _ } -> List.iter add_expr args
    | Ast.SIf { cond; then_; else_ } ->
        add_expr cond;
        add_stmt then_;
        (match else_ with None -> () | Some e -> add_stmt e)
    | Ast.SWhile { cond; body } ->
        add_expr cond;
        add_stmt body
    | Ast.SFor { init; cond; step; body } ->
        (match init with None -> () | Some i -> add_stmt i);
        (match cond with None -> () | Some c -> add_expr c);
        (match step with None -> () | Some st -> add_stmt st);
        add_stmt body
    | Ast.SReturn eo ->
        (match eo with None -> () | Some e -> add_expr e)
    | Ast.SLabel { body; _ } -> add_stmt body
  and add_decl (d:Ast.decl Ast.node) : unit =
    match d.v with
    | Ast.DVar { init; _ } ->
        (match init with None -> () | Some e -> add_expr e)
    | Ast.DConst { value; _ } ->
        add_expr value
    | Ast.DType _ | Ast.DDirective _ ->
        ()
    | Ast.DProc p ->
        let key = normalize_name p.v.name.v in
        if key <> "" && not (Hashtbl.mem out key) then
          Hashtbl.add out key (List.map (fun prm -> prm.v.pname.v) p.v.params);
        List.iter add_decl p.v.locals;
        add_stmt p.v.body
  in
  match doc.Document.ast with
  | None -> ()
  | Some prog ->
      List.iter (function
        | Ast.TopDecl d -> add_decl d
        | Ast.TopStmt s -> add_stmt s
      ) prog

let inlay_hints_json_for (ws:t) ~(uri:T.DocumentUri.t) ~(range:T.Range.t) : Yojson.Safe.t =
  match doc_of_uri ws uri with
  | None -> `List []
  | Some doc ->
      let proc_params : (string, string list) Hashtbl.t = Hashtbl.create 128 in
      docs_for_lookup ws doc |> List.iter (fun d -> collect_proc_param_map d proc_params);
      let seen = Hashtbl.create 256 in
      let hints = ref [] in
      let add_call_hints (callee:Ast.ident) (args:Ast.expr Ast.node list) =
        let key = normalize_name callee.v in
        match Hashtbl.find_opt proc_params key with
        | None -> ()
        | Some params ->
            List.iteri (fun i (arg:Ast.expr Ast.node) ->
              match nth_opt params i with
              | None -> ()
              | Some pname ->
                  let pos = Lsp_conv.position_of_ast_pos arg.loc.start_pos in
                  if pos_in_range pos range then
                    let hk = Printf.sprintf "%d|%d|%s" pos.line pos.character pname in
                    if not (Hashtbl.mem seen hk) then (
                      Hashtbl.add seen hk true;
                      hints :=
                        `Assoc [
                          "position", T.Position.yojson_of_t pos;
                          "label", `String (pname ^ ":");
                          "kind", `Int 2;
                          "paddingRight", `Bool true;
                        ]
                        :: !hints
                    )
            ) args
      in
      let rec walk_expr (e:Ast.expr Ast.node) : unit =
        match e.v with
        | Ast.EName _ | Ast.ELit _ -> ()
        | Ast.EUnop { rhs; _ } -> walk_expr rhs
        | Ast.EBinop { lhs; rhs; _ } -> walk_expr lhs; walk_expr rhs
        | Ast.ECall { callee; args } ->
            add_call_hints callee args;
            List.iter walk_expr args
        | Ast.EIndex { base; index } ->
            walk_expr base;
            List.iter walk_expr index
        | Ast.EField { base; _ } ->
            walk_expr base
        | Ast.EAt { field; ptr } ->
            walk_expr field;
            walk_expr ptr
        | Ast.EDeref { ptr } ->
            walk_expr ptr
        | Ast.EParen x ->
            walk_expr x
      in
      let rec walk_stmt (s:Ast.stmt Ast.node) : unit =
        match s.v with
        | Ast.SEmpty | Ast.SGoto _ -> ()
        | Ast.SDecl d -> walk_decl d
        | Ast.SBlock xs -> List.iter walk_stmt xs
        | Ast.SAssign { lhs; rhs } -> walk_expr lhs; walk_expr rhs
        | Ast.SCallStmt { callee; args; _ } ->
            add_call_hints callee args;
            List.iter walk_expr args
        | Ast.SIf { cond; then_; else_ } ->
            walk_expr cond;
            walk_stmt then_;
            (match else_ with None -> () | Some e -> walk_stmt e)
        | Ast.SWhile { cond; body } ->
            walk_expr cond;
            walk_stmt body
        | Ast.SFor { init; cond; step; body } ->
            (match init with None -> () | Some i -> walk_stmt i);
            (match cond with None -> () | Some c -> walk_expr c);
            (match step with None -> () | Some st -> walk_stmt st);
            walk_stmt body
        | Ast.SReturn eo ->
            (match eo with None -> () | Some e -> walk_expr e)
        | Ast.SLabel { body; _ } ->
            walk_stmt body
      and walk_decl (d:Ast.decl Ast.node) : unit =
        match d.v with
        | Ast.DVar { init; _ } ->
            (match init with None -> () | Some e -> walk_expr e)
        | Ast.DConst { value; _ } ->
            walk_expr value
        | Ast.DType _ | Ast.DDirective _ ->
            ()
        | Ast.DProc p ->
            List.iter walk_decl p.v.locals;
            walk_stmt p.v.body
      in
      (match doc.Document.ast with
       | None -> ()
       | Some prog ->
           List.iter (function
             | Ast.TopDecl d -> walk_decl d
             | Ast.TopStmt s -> walk_stmt s
            ) prog);
      `List (List.rev !hints)
