module T = Lsp.Types
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_hover_markdown
open Workspace_tuning
module Metadata = Workspace_symbol_metadata

module Perf_stats = Workspace_foundation.Perf_stats

let symbol_key_at_position (doc : Document.t) (pos : T.Position.t) :
    string option =
  match nav_word_at_position doc pos with
  | None -> None
  | Some (nm, _) ->
      let key = normalize_name nm in
      if key = "" then None else Some key

let adjust_nav_position (doc : Document.t) (pos : T.Position.t) : T.Position.t =
  match nav_word_at_position doc pos with
  | Some _ -> pos
  | None when word_at_position doc pos <> None -> pos
  | None when pos.character > 0 -> (
      let prev = { pos with T.Position.character = pos.character - 1 } in
      match nav_word_at_position doc prev with Some _ -> prev | None -> pos)
  | None -> pos

type nav_budget = {
  ws : t;
  deadline_ms : float;
  mutable exceeded : bool;
  mutable cancelled : bool;
  mutable cancel_recorded : bool;
}

let nav_budget_start (ws : t) : nav_budget =
  {
    ws;
    deadline_ms = Perf_stats.now_ms () +. float_of_int nav_soft_budget_ms;
    exceeded = false;
    cancelled = false;
    cancel_recorded = false;
  }

let nav_budget_check (budget : nav_budget) : bool =
  if budget.cancelled then true
  else if request_cancelled budget.ws then (
    budget.cancelled <- true;
    if not budget.cancel_recorded then (
      budget.cancel_recorded <- true;
      Perf_stats.tick "cancel.applied");
    true)
  else if budget.exceeded then true
  else if Perf_stats.now_ms () > budget.deadline_ms then (
    budget.exceeded <- true;
    Perf_stats.tick "nav.soft_budget_exceeded";
    true)
  else false

let allow_fallback_for_ws (ws : t) (doc : Document.t) : bool =
  allow_unscoped_fallback doc || ws.startup_fully_nav_ready_ms = None

let prefer_local_defs_before_position (doc : Document.t) (pos : T.Position.t)
    (defs : def list) : def list =
  match
    Text_index.offset_of_line_col doc.Document.index ~line:pos.T.Position.line
      ~col:pos.T.Position.character
  with
  | None -> defs
  | Some cursor_off -> (
      let local_before =
        defs
        |> List.filter (fun d ->
            same_uri d.uri doc.Document.uri
            && d.loc.Ast.Loc.start_pos.offset <= cursor_off)
      in
      match local_before with
      | [] -> defs
      | d :: rest ->
          let best =
            List.fold_left
              (fun best cand ->
                if
                  cand.loc.Ast.Loc.start_pos.offset
                  > best.loc.Ast.Loc.start_pos.offset
                then cand
                else best)
              d rest
          in
          [ best ])

let occurrences_in_doc_fallback (doc : Document.t) ~(key : string) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  let text = doc.Document.text in
  let n = String.length text in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if is_ident_char text.[i] && (i = 0 || not (is_ident_char text.[i - 1]))
    then (
      let j = ref (i + 1) in
      while !j < n && is_ident_char text.[!j] do
        incr j
      done;
      let tok = String.sub text i (!j - i) in
      let acc =
        if normalize_name tok = key then
          let loc =
            loc_of_offsets ~file:doc.Document.file ~idx:doc.Document.index ~s:i
              ~e:!j
          in
          (doc.Document.uri, loc) :: acc
        else acc
      in
      loop !j acc)
    else loop (i + 1) acc
  in
  loop 0 []

let occurrences_in_doc (doc : Document.t) ~(key : string) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  try
    Lexer.with_session_state (fun lexer ->
        let lexbuf = Lexing.from_string doc.Document.text in
        (match doc.Document.file with
        | None -> ()
        | Some f ->
            lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
        let rec loop acc =
          let tok = Lexer.token lexer lexbuf in
          let sp = Lexing.lexeme_start_p lexbuf in
          let ep = Lexing.lexeme_end_p lexbuf in
          match tok with
          | Parser.EOF -> List.rev acc
          | Parser.ID s when normalize_name s = key ->
              let loc =
                Ast.Loc.of_lexing_positions sp ep ~file:doc.Document.file
              in
              loop ((doc.Document.uri, loc) :: acc)
          | _ -> loop acc
        in
        loop [])
  with _ -> occurrences_in_doc_fallback doc ~key

let occurrences_for_docs_with_budget (budget : nav_budget)
    (docs : Document.t list) ~(key : string) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  let acc = ref [] in
  List.iter
    (fun d ->
      if not (nav_budget_check budget) then
        acc := List.rev_append (occurrences_in_doc d ~key) !acc)
    docs;
  List.rev !acc

let hover_current_file_fact (doc : Document.t) : string =
  match doc.Document.file with
  | Some p -> Printf.sprintf "Current file: `%s`" p
  | None ->
      Printf.sprintf "Current file: `%s`"
        (Uri_path.docuri_to_string doc.Document.uri)

let builtin_type_hover_at (doc : Document.t) (pos : T.Position.t) :
    T.Hover.t option =
  match word_at_position doc pos with
  | None -> None
  | Some (name, loc) ->
      if
        (not (Metadata.is_builtin_type_name name))
        || not (is_builtin_type_context_at_loc doc name loc)
      then None
      else
        let line1 = loc.Ast.Loc.start_pos.line in
        let display, dims =
          match line_text_in_doc doc ~line1 with
          | None -> (name, [])
          | Some line ->
              let col = loc.Ast.Loc.start_pos.col in
              let tokens = tokenize_ident_words line in
              let rec after_current = function
                | [] -> []
                | (tok, s, e) :: rest ->
                    if s <= col && col < e then (tok, s, e) :: rest
                    else after_current rest
              in
              let toks = after_current tokens in
              let max_dims =
                match normalize_name name with "A" -> 2 | "P" -> 1 | _ -> 1
              in
              let rec take n xs =
                if n <= 0 then []
                else
                  match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl
              in
              let rest = match toks with [] -> [] | _ :: tl -> tl in
              let dims =
                rest
                |> List.filter_map (fun (tok, _, _) ->
                       let k = normalize_name tok in
                       if
                         k = "" || is_reserved_keyword k
                         || Metadata.is_builtin_type_name k
                       then None
                       else Some tok)
                |> take max_dims
              in
              let sep = if normalize_name name = "A" then "," else " " in
              let display =
                match dims with
                | [] -> name
                | _ -> name ^ " " ^ String.concat sep dims
              in
              (display, dims)
        in
        let cls, meaning =
          match normalize_name name with
          | "U" ->
              ( "unsigned integer",
                match dims with
                | n :: _ -> "unsigned integer, " ^ n ^ " bits"
                | [] -> "unsigned integer" )
          | "S" ->
              ( "signed integer",
                match dims with
                | n :: _ -> "signed integer, " ^ n ^ " magnitude bits plus sign"
                | [] -> "signed integer" )
          | "F" ->
              ( "floating",
                match dims with
                | n :: _ -> "floating value with mantissa precision " ^ n
                | [] -> "floating value" )
          | "A" ->
              ( "fixed",
                match dims with
                | scale :: fraction :: _ ->
                    "fixed value, scale " ^ scale ^ ", fraction " ^ fraction
                | _ -> "fixed value" )
          | "B" ->
              ( "bit string",
                match dims with
                | n :: _ -> "bit string, " ^ n ^ " bits"
                | [] -> "bit string" )
          | "C" ->
              ( "character string",
                match dims with
                | n :: _ -> "character string, " ^ n ^ " characters"
                | [] -> "character string" )
          | "STATUS" -> ("status", "status enumeration/list")
          | "P" -> (
              match dims with
              | target :: _ -> ("pointer", "typed pointer to " ^ target)
              | [] -> ("pointer", "pointer type"))
          | _ -> ("built-in", "built-in type")
        in
        let facts =
          [
            Printf.sprintf "Type class: %s" cls;
            Printf.sprintf "Meaning: %s" meaning;
          ]
        in
        let facts =
          match (normalize_name name, dims) with
          | ("U" | "S" | "B"), n :: _ ->
              (Printf.sprintf "Size: %s bits" n) :: facts
          | "C", n :: _ -> (Printf.sprintf "Size: %s characters" n) :: facts
          | "F", n :: _ -> (Printf.sprintf "Precision: %s" n) :: facts
          | "A", scale :: fraction :: _ ->
              Printf.sprintf "Scale: %s" scale
              :: Printf.sprintf "Fraction: %s" fraction
              :: facts
          | _ -> facts
        in
        Some
          (hover_markdown ~range:(Lsp_conv.range_of_loc loc)
             (hover_panel ~name:display
                ~summary:(Printf.sprintf "JOVIAL built-in %s type" cls)
                ~facts:(List.rev facts) ~sections:[]))

let type_origin_label = function
  | Metadata.BuiltinType -> "built-in type"
  | Metadata.UserDefinedType _ -> "user-defined type"
  | Metadata.InferredType -> "inferred type"
  | Metadata.UnknownType -> "unknown type"

let type_decl_location_fact (ti : Metadata.jovial_type_info) : string option =
  match (ti.Metadata.type_decl_uri, ti.Metadata.type_decl_loc) with
  | Some uri, Some loc ->
      let file =
        match loc.Ast.Loc.file with
        | Some p -> Filename.basename p
        | None -> (
            match Uri_path.file_path_of_uri uri with
            | Some p -> Filename.basename p
            | None -> Uri_path.docuri_to_string uri)
      in
      Some
        (Printf.sprintf "Type declared at: `%s:%d:%d`" file
           loc.Ast.Loc.start_pos.line
           (loc.Ast.Loc.start_pos.col + 1))
  | _, Some loc ->
      let file = match loc.Ast.Loc.file with Some p -> Filename.basename p | None -> "<unknown>" in
      Some
        (Printf.sprintf "Type declared at: `%s:%d:%d`" file
           loc.Ast.Loc.start_pos.line
           (loc.Ast.Loc.start_pos.col + 1))
  | _ -> None

let type_resolution_facts ws doc (ti : Metadata.jovial_type_info) =
  let base = [ Printf.sprintf "Type origin: %s" (type_origin_label ti.origin) ] in
  match ti.origin with
  | Metadata.UserDefinedType name ->
      let key = normalize_name name in
      let resolved_fact (resolved : Metadata.jovial_type_info) =
        let detail =
          match resolved.explanation with Some e -> " - " ^ e | None -> ""
        in
        Printf.sprintf "Resolved type: `%s`%s" resolved.display detail
      in
      let from_metadata =
        match ti.resolved_display with
        | None -> None
        | Some display ->
            let detail =
              match ti.explanation with Some e -> " - " ^ e | None -> ""
            in
            Some (Printf.sprintf "Resolved type: `%s`%s" display detail)
      in
      let declared = type_decl_location_fact ti in
      let from_decl_uri () =
        match ti.type_decl_uri with
        | None -> None
        | Some uri -> (
            match doc_of_uri ws uri with
            | None -> None
            | Some type_doc ->
                collect_doc_defs type_doc
                |> List.find_map (fun target ->
                       if target.kind = sym_kind_type && target.key = key then
                         Option.map resolved_fact
                           target.metadata.Metadata.type_info
                       else None))
      in
      (match (from_metadata, declared) with
      | Some resolved, Some declared -> base @ [ resolved; declared ]
      | Some resolved, None -> base @ [ resolved ]
      | None, _ ->
          let find_type_defs docs =
            docs
            |> List.concat_map collect_doc_defs
            |> List.filter (fun d -> d.kind = sym_kind_type && d.key = key)
            |> uniq_defs
          in
          let type_defs =
            let scoped = find_type_defs (docs_for_lookup ws doc) in
            if scoped <> [] then scoped else find_type_defs (docs_for_rename ws doc)
          in
          (match type_defs with
          | target :: _ ->
              let resolved =
                match target.metadata.Metadata.type_info with
                | Some resolved ->
                    [ resolved_fact resolved ]
                | None -> (
                    match from_decl_uri () with
                    | Some resolved -> [ resolved ]
                    | None -> [ "Resolved type: pending semantic index" ])
              in
              base @ resolved
              @ [ Printf.sprintf "Type declared at: `%s`" (file_line_of_def target) ]
          | [] -> (
              match from_decl_uri () with
              | Some resolved -> (
                  match declared with
                  | Some declared -> base @ [ resolved; declared ]
                  | None -> base @ [ resolved ])
              | None -> base @ [ "Resolved type: pending semantic index" ])))
  | Metadata.BuiltinType -> (
      match ti.explanation with
      | Some e -> base @ [ Printf.sprintf "Resolved type: `%s` - %s" ti.display e ]
      | None -> base)
  | Metadata.InferredType | Metadata.UnknownType -> base

let hover_def_facts ws doc d =
  let kind = jovial_kind_for_def ws d in
  let metadata = d.metadata in
  let facts =
    [
      Printf.sprintf "Classification: %s" kind;
      Printf.sprintf "Declaration role: %s"
        (Metadata.decl_role_label metadata.decl_role);
    ]
  in
  let facts =
    match metadata.type_info with
    | None -> facts
    | Some ti ->
        facts
        @ [ Printf.sprintf "Type: `%s`" ti.Metadata.display ]
        @ type_resolution_facts ws doc ti
  in
  let facts =
    facts
    @ [
        Printf.sprintf "Constant: %s"
          (if metadata.is_constant then "yes" else "no");
      ]
  in
  let facts =
    match metadata.storage with
    | None -> facts
    | Some storage ->
        facts @ [ Printf.sprintf "Storage: %s" (Metadata.storage_label storage) ]
  in
  let facts =
    match d.container with
    | None -> facts
    | Some c -> facts @ [ Printf.sprintf "Scope: `%s`" c ]
  in
  facts
  @ [
      source_path_fact_for_def d;
      declaration_location_fact_for_def d;
      Printf.sprintf "Symbol key: `%s`" d.key;
      Printf.sprintf "Role: %s" (semantic_role_for_def ws d);
    ]

let hover_summary_for_def ws d =
  let kind = jovial_kind_for_def ws d in
  match d.metadata.Metadata.external_kind with
  | Metadata.ExternalDef -> Printf.sprintf "JOVIAL external DEF %s" kind
  | Metadata.ExternalRef ->
      Printf.sprintf "JOVIAL external REF %s import" kind
  | Metadata.ExternalSystem -> Printf.sprintf "JOVIAL system %s" kind
  | Metadata.ExternalLocal -> Metadata.metadata_summary d.metadata

let change_impact_for_def ws d =
  match d.metadata.Metadata.external_kind with
  | Metadata.ExternalRef ->
      "Changing this REF can break module linkage or calls in this file, but \
       the actual implementation is controlled by the matching DEF."
  | Metadata.ExternalDef ->
      "Callers, parameter bindings, return type, external DEF/REF \
       declarations, and COMPOOL or module users may be affected. Run Find \
       References before changing this declaration."
  | _ -> (
      match d.metadata.Metadata.jovial_kind with
      | Metadata.JovialProcedure | Metadata.JovialFunction ->
          "Callers, parameter bindings, return type, external DEF/REF \
           declarations, and COMPOOL or module users may be affected. Run Find \
           References before changing this declaration."
      | Metadata.JovialDefine ->
          "Macro expansion changes can affect every use site, including code \
           that only sees the DEFINE through imported or included declarations. \
           Run Find References before changing this declaration."
      | Metadata.JovialTable | Metadata.JovialBlock
      | Metadata.JovialConstantTable ->
          "Type, size, layout, or name changes can affect assignments, \
           formulas, table/block layout, COMPOOL users, and external DEF/REF \
           users. Run Find References before changing this declaration."
      | _ -> (
          match jovial_kind_for_def ws d with
  | "define" ->
      "Macro expansion changes can affect every use site, including code that \
       only sees the DEFINE through imported or included declarations. Run Find \
       References before changing this declaration."
  | "table" | "block" ->
      "Type, size, layout, or name changes can affect assignments, formulas, \
       table/block layout, COMPOOL users, and external DEF/REF users. Run Find \
       References before changing this declaration."
  | _ ->
      "Type, size, layout, or name changes can affect assignments, formulas, \
       table entries, COMPOOL users, and external DEF/REF users. Run Find \
       References before changing this declaration."))

let hover_body_for_def ws doc d =
  let sig_line = proc_signature_for_def ws d in
  let src_line = source_line_for_def ws d in
  let navigation_section =
    if d.kind = sym_kind_func then
      let targets =
        proc_real_defs_by_key ws doc ~key:d.key
      in
      match
        targets
        |> List.filter (fun target ->
               loc_key ~uri:target.uri target.loc
               <> loc_key ~uri:d.uri d.loc)
      with
      | target :: _ ->
          hover_inline_section "Navigation"
            (Printf.sprintf "Definition resolves to `%s` in `%s`."
               (String.trim
                  (match source_line_for_def ws target with
                  | Some line when String.trim line <> "" -> line
                  | _ -> target.name))
               (file_line_of_def target))
      | [] when Metadata.is_external_ref d.metadata ->
          hover_inline_section "Navigation"
            "Target DEF/implementation: not found yet."
      | [] -> ""
    else ""
  in
  let primary_decl =
    match sig_line with Some sig_line -> Some sig_line | None -> src_line
  in
  let decl_block =
    match primary_decl with
    | None -> ""
    | Some line ->
        let line = truncate_text 600 line in
        if String.trim line = "" then ""
        else hover_code_section "Declaration" line
  in
  let source_block =
    match src_line with
    | None -> ""
    | Some line ->
        let line = truncate_text 600 line in
        if String.trim line = "" then ""
        else hover_code_section "Source declaration" line
  in
  let preview_block =
    match implementation_preview_for_def ws d ~max_lines:12 with
    | None -> ""
    | Some preview ->
        let preview = truncate_text 2000 preview in
        if String.trim preview = "" then ""
        else hover_code_section "Implementation preview" preview
  in
  let impact =
    hover_inline_section "Change impact" (change_impact_for_def ws d)
  in
  let meaning =
    match d.metadata.Metadata.external_kind with
    | Metadata.ExternalRef ->
        hover_inline_section "Meaning"
          "This REF makes the external symbol visible in this module. It is \
           not the real definition or implementation. Navigation from calls \
           should resolve to the matching DEF/implementation when available."
    | Metadata.ExternalDef ->
        hover_inline_section "Meaning"
          "This DEF is the exported external declaration. Calls and matching \
           REFs may resolve here; an implementation preview appears when a \
           body is available."
    | _ -> ""
  in
  let sections =
    List.filter
      (fun section -> String.trim section <> "")
      [ decl_block; source_block; preview_block; navigation_section; meaning; impact ]
  in
  hover_panel ~name:d.name
    ~summary:(hover_summary_for_def ws d)
    ~facts:(hover_def_facts ws doc d) ~sections

let hover_semantic_for (ws : t) (doc : Document.t) ~(pos : T.Position.t) :
    T.Hover.t option =
  let budget = nav_budget_start ws in
  let compute () =
    if nav_budget_check budget then None
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
              | None ->
                  hover_panel ~name:("COMPOOL " ^ imp.name)
                    ~summary:"JOVIAL COMPOOL import"
                    ~facts:
                      [
                        "Classification: COMPOOL import";
                        "Declaration role: COMPOOL import";
                        Printf.sprintf "Imported COMPOOL: `%s`" imp.name;
                        "Status: unresolved";
                        hover_current_file_fact doc;
                      ]
                    ~sections:
                      [
                        hover_inline_section "Next check"
                          "No matching compool declaration was found in the \
                           current workspace. Confirm the COMPOOL is declared \
                           in the source roots or imported through the \
                           expected external DEF/REF pair.";
                      ]
              | Some p ->
                  hover_panel ~name:("COMPOOL " ^ imp.name)
                    ~summary:"JOVIAL COMPOOL import"
                    ~facts:
                      [
                        "Classification: COMPOOL import";
                        "Declaration role: COMPOOL import";
                        Printf.sprintf "Imported COMPOOL: `%s`" imp.name;
                        Printf.sprintf "Resolved COMPOOL file: `%s`" p;
                      ]
                    ~sections:
                      [
                        hover_inline_section "Change impact"
                          "Changing declarations in this COMPOOL can affect \
                           every module that imports it. Run Find References \
                           before changing shared names, types, layout, or \
                           external DEF/REF declarations.";
                      ])
      in
      match builtin_type_hover_at doc pos with
      | Some hover -> Some hover
      | None -> (
      match define_under_cursor doc pos with
      | Some (dm, word_loc) ->
          let d = def_of_preprocess_define doc dm in
          let primary_decl =
            match source_line_for_def ws d with
            | Some line when String.trim line <> "" -> line
            | _ ->
                if dm.requires_call then
                  Printf.sprintf "DEFINE %s(%s) \"%s\";" dm.name
                    (String.concat "," dm.formals)
                    dm.body
                else Printf.sprintf "DEFINE %s \"%s\";" dm.name dm.body
          in
          let decl_block =
            let line = truncate_text 280 primary_decl in
            if String.trim line = "" then ""
            else hover_code_section "Declaration" line
          in
          let sections =
            (if decl_block = "" then [] else [ decl_block ])
            @
            (match source_line_for_def ws d with
            | Some line when String.trim line <> "" ->
                [ hover_code_section "Source declaration" line ]
            | _ -> [])
            @
            (if dm.formals = [] then
               [
                 hover_inline_section "Expansion"
                   (Printf.sprintf "`%s`" (truncate_text 180 dm.body));
               ]
             else
               [
                 hover_inline_section "Formals"
                   (Printf.sprintf "`%s`" (String.concat ", " dm.formals));
                 hover_inline_section "Expansion template"
                   (Printf.sprintf "`%s`" (truncate_text 180 dm.body));
               ])
            @ [ hover_inline_section "Change impact" (change_impact_for_def ws d) ]
          in
            Some
              (hover_markdown ~range:(Lsp_conv.range_of_loc word_loc)
                 (hover_panel ~name:d.name ~summary:"JOVIAL define"
                  ~facts:(hover_def_facts ws doc d) ~sections))
      | None ->
          let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
          let nav = nav_for_doc_cached ws cache doc in
          let word = nav_word_at_position doc pos in
          let resolved =
            symbol_at_position_in_nav nav ~uri:doc.Document.uri ~pos
          in
          let defs, hover_loc =
            match resolved with
            | Some (sym_id, loc) -> (
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
                        match
                          Semantic_store.defs_for_sym_id ws.semantic_store
                            sym_id
                        with
                        | d0 :: _ -> Some (def_of_snapshot_def d0)
                        | [] ->
                            if nav_budget_check budget then None
                            else
                              find_def_for_sym_id ws cache
                                ~docs:(docs_for_symbol ()) ~sym_id
                      else if nav_budget_check budget then None
                      else
                        find_def_for_sym_id ws cache ~docs:(docs_for_symbol ())
                          ~sym_id
                in
                match defn with
                | Some d -> ([ d ], Some loc)
                | None -> (
                    match word with
                    | None -> ([], Some loc)
                    | Some (nm, wloc) ->
                        let defs0 =
                          if
                            allow_fallback_for_ws ws doc
                            && not (nav_budget_check budget)
                          then
                            fallback_defs_by_name ws doc (normalize_name nm)
                            |> prefer_local_defs_before_position doc pos
                          else []
                        in
                        (defs0, Some wloc)))
            | None -> (
                match word with
                | None -> ([], None)
                | Some (nm, wloc) ->
                    let defs0 =
                      if
                        allow_fallback_for_ws ws doc
                        && not (nav_budget_check budget)
                      then
                        fallback_defs_by_name ws doc (normalize_name nm)
                        |> prefer_local_defs_before_position doc pos
                      else []
                    in
                    (defs0, Some wloc))
          in
          let defs =
            match defs with
            | d :: _
              when d.kind = sym_kind_func
                   && not (is_likely_proc_implementation ws d) ->
                let hovering_decl =
                  match hover_loc with
                  | None -> false
                  | Some loc ->
                      same_uri d.uri doc.Document.uri
                      && loc_key ~uri:d.uri loc = loc_key ~uri:d.uri d.loc
                in
                if hovering_decl then defs
                else
                  let real_defs = proc_real_defs_by_key ws doc ~key:d.key in
                  if real_defs = [] then defs else real_defs
            | [] -> (
                match word with
                | None -> []
                | Some (nm, _) ->
                    let key = normalize_name nm in
                    if
                      key = "" || (not (allow_fallback_for_ws ws doc))
                      || nav_budget_check budget
                    then []
                    else proc_real_defs_by_key ws doc ~key)
            | _ -> defs
          in
          if defs = [] then
            match import_text with
            | Some txt -> Some (hover_markdown txt)
            | None -> (
                match word with
                | None -> None
                | Some (nm, word_loc) ->
                    Some
                      (hover_markdown ~range:(Lsp_conv.range_of_loc word_loc)
                         (hover_panel ~name:nm
                            ~summary:"Unresolved JOVIAL symbol"
                            ~facts:
                              [
                                "Status: no visible declaration was found for \
                                 this reference.";
                                hover_current_file_fact doc;
                                Printf.sprintf "Position: line %d, column %d"
                                  word_loc.Ast.Loc.start_pos.line
                                  (word_loc.Ast.Loc.start_pos.col + 1);
                              ]
                            ~sections:
                              [
                                hover_inline_section "Next check"
                                  "Confirm the symbol is declared in scope, \
                                   imported from the expected COMPOOL, or \
                                   available through the expected external \
                                   DEF/REF pair.";
                              ])))
          else
            let rec top_lines acc = function
              | [] -> List.rev acc
              | _ when nav_budget_check budget -> List.rev acc
              | d :: tl -> top_lines (hover_body_for_def ws doc d :: acc) tl
            in
            let top_lines = top_lines [] defs in
            let lines =
              match import_text with None -> top_lines | Some imp -> imp :: top_lines
            in
            let body = String.concat "\n\n---\n\n" lines in
            let range = Option.map Lsp_conv.range_of_loc hover_loc in
            Some (hover_markdown ?range body))
  in
  let out = compute () in
  ignore (nav_budget_check budget);
  out
let starts_with_ci ~(prefix : string) (s : string) : bool =
  let p = normalize_name prefix in
  if p = "" then true
  else
    let u = normalize_name s in
    let lp = String.length p in
    String.length u >= lp && String.sub u 0 lp = p

let completion_item_kind_of_def_kind (k : int) : int =
  if k = sym_kind_module then 9
  else if k = sym_kind_type then 7
  else if k = sym_kind_field then 10
  else if k = sym_kind_func then 3
  else if k = sym_kind_const then 21
  else 6

let completion_item_kind_of_metadata (d : def) : int =
  match d.metadata.Metadata.jovial_kind with
  | Metadata.JovialProgram | Metadata.JovialModule | Metadata.JovialCompool
  | Metadata.JovialCompoolImport | Metadata.JovialBlock ->
      9
  | Metadata.JovialType | Metadata.JovialBuiltinType -> 7
  | Metadata.JovialField -> 10
  | Metadata.JovialProcedure | Metadata.JovialFunction -> 3
  | Metadata.JovialDefine | Metadata.JovialConstantItem
  | Metadata.JovialConstantTable | Metadata.JovialStatusConstant ->
      21
  | _ -> completion_item_kind_of_def_kind d.kind

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

let split_signature_params (label : string) : string list =
  match String.index_opt label '(' with
  | None -> []
  | Some i0 -> (
      match String.rindex_opt label ')' with
      | None -> []
      | Some i1 when i1 <= i0 -> []
      | Some i1 ->
          let inside = String.sub label (i0 + 1) (i1 - i0 - 1) |> String.trim in
          if inside = "" then []
          else
            inside |> String.split_on_char ',' |> List.map String.trim
            |> List.filter (fun s -> s <> ""))

let signature_params_json (label : string) : Yojson.Safe.t =
  split_signature_params label
  |> List.map (fun p -> `Assoc [ ("label", `String p) ])
  |> fun xs -> `List xs

let call_context_at_position (doc : Document.t) (pos : T.Position.t) :
    (string * int) option =
  match
    Text_index.offset_of_line_col doc.Document.index ~line:pos.line
      ~col:pos.character
  with
  | None -> None
  | Some raw_cursor -> (
      let text = doc.Document.text in
      let n = String.length text in
      let cursor = max 0 (min n raw_cursor) in
      let stack : int list ref = ref [] in
      let in_single = ref false in
      let in_double = ref false in
      let i = ref 0 in
      while !i < cursor do
        let c = text.[!i] in
        (if !in_single then (
           if c = '\'' then
             if !i + 1 < cursor && text.[!i + 1] = '\'' then i := !i + 1
             else in_single := false)
         else if !in_double then (
           if c = '"' then
             if !i + 1 < cursor && text.[!i + 1] = '"' then i := !i + 1
             else in_double := false)
         else
           match c with
           | '\'' -> in_single := true
           | '"' -> in_double := true
           | '(' -> stack := !i :: !stack
           | ')' -> ( match !stack with _ :: tl -> stack := tl | [] -> ())
           | _ -> ());
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
              if j >= 0 && is_ident_char text.[j] then start_ident (j - 1)
              else j + 1
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
                  (if !in_single then (
                     if c = '\'' then
                       if !k + 1 < cursor && text.[!k + 1] = '\'' then
                         k := !k + 1
                       else in_single := false)
                   else if !in_double then (
                     if c = '"' then
                       if !k + 1 < cursor && text.[!k + 1] = '"' then
                         k := !k + 1
                       else in_double := false)
                   else
                     match c with
                     | '\'' -> in_single := true
                     | '"' -> in_double := true
                     | '(' -> incr depth
                     | ')' -> if !depth > 0 then decr depth
                     | ',' when !depth = 1 -> incr commas
                     | _ -> ());
                  incr k
                done;
                Some (key, max 0 !commas))

let range_intersects (a : T.Range.t) (b : T.Range.t) : bool =
  compare_pos a.start b.end_ <= 0 && compare_pos b.start a.end_ <= 0

let parse_missing_compool_name (msg : string) : string option =
  let prefix = "Missing COMPOOL:" in
  let lp = String.length prefix in
  if String.length msg < lp || String.sub msg 0 lp <> prefix then None
  else
    let name = String.trim (String.sub msg lp (String.length msg - lp)) in
    let k = normalize_name name in
    if k = "" then None else Some k

let find_substring_index ~(haystack : string) ~(needle : string) : int option =
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

let parse_compool_name_from_hint (msg : string) : string option =
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

let import_insert_position (doc : Document.t) : T.Position.t * bool =
  let idx = doc.Document.index in
  let line_count = Text_index.line_count idx in
  let line_max =
    Document.imports doc
    |> List.fold_left
         (fun acc (imp : Preprocess.import) ->
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
    (({ line; character = ch } : T.Position.t), true)
  else (({ line = 0; character = 0 } : T.Position.t), false)

let has_import_for_compool (doc : Document.t) (name : string) : bool =
  let key = normalize_name name in
  Document.imports doc
  |> List.exists (fun (imp : Preprocess.import) ->
      normalize_name imp.name = key)

let nav_compute_with_budget_value (budget : nav_budget) (f : unit -> 'a) : 'a =
  let out = f () in
  ignore (nav_budget_check budget);
  out

let schedule_nav_miss_for_result (ws : t) (doc : Document.t)
    (pos : T.Position.t) ~(empty : bool) : unit =
  if empty then
    match symbol_key_at_position doc pos with
    | None -> ()
    | Some key -> schedule_nav_miss_reconcile ws ~doc ~symbol_key:key

let locations_with_budget (budget : nav_budget)
    (occs : (T.DocumentUri.t * Ast.Loc.t) list) : T.Location.t list =
  let seen = Hashtbl.create 256 in
  let out = ref [] in
  List.iter
    (fun (u, loc) ->
      if not (nav_budget_check budget) then
        let k = loc_key ~uri:u loc in
        if not (Hashtbl.mem seen k) then (
          Hashtbl.add seen k true;
          out := Lsp_conv.location_of_loc ~uri:u loc :: !out))
    occs;
  List.rev !out

let uri_key (uri : T.DocumentUri.t) : string = Uri_path.docuri_to_string uri

let doc_key (doc : Document.t) : string = uri_key doc.Document.uri

let add_doc_once (seen : (string, bool) Hashtbl.t) (out : Document.t list ref)
    (doc : Document.t) : unit =
  let key = doc_key doc in
  if not (Hashtbl.mem seen key) then (
    Hashtbl.replace seen key true;
    out := doc :: !out)

let docs_except_seen (seen : (string, bool) Hashtbl.t) (docs : Document.t list)
    : Document.t list =
  let out = ref [] in
  List.iter
    (fun doc ->
      let key = doc_key doc in
      if not (Hashtbl.mem seen key) then (
        Hashtbl.replace seen key true;
        out := doc :: !out))
    docs;
  List.rev !out

let reference_doc_stages (ws : t) (doc : Document.t) :
    Document.t list * Document.t list * Document.t list =
  let seen = Hashtbl.create 64 in
  let current = [ doc ] in
  Hashtbl.replace seen (doc_key doc) true;
  let imported = docs_for_lookup ws doc |> docs_except_seen seen in
  let workspace = docs_for_rename ws doc |> docs_except_seen seen in
  (current, imported, workspace)

let def_keys_for_defs (defs : def list) : (string, bool) Hashtbl.t =
  let keys = Hashtbl.create 32 in
  List.iter
    (fun d ->
      if not (is_ref_import_def d) then
        Hashtbl.replace keys (loc_key ~uri:d.uri d.loc) true)
    defs;
  keys

let filter_declarations ~(include_decl : bool) ~(def_keys : (string, bool) Hashtbl.t)
    (occs : (T.DocumentUri.t * Ast.Loc.t) list) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  if include_decl then occs
  else
    List.filter
      (fun (u, loc) -> not (Hashtbl.mem def_keys (loc_key ~uri:u loc)))
      occs

let emit_locations_stage (budget : nav_budget)
    (seen : (string, bool) Hashtbl.t) ~(emit : T.Location.t list -> unit)
    (occs : (T.DocumentUri.t * Ast.Loc.t) list) : T.Location.t list =
  let out = ref [] in
  List.iter
    (fun (u, loc) ->
      if not (nav_budget_check budget) then
        let key = loc_key ~uri:u loc in
        if not (Hashtbl.mem seen key) then (
          Hashtbl.replace seen key true;
          out := Lsp_conv.location_of_loc ~uri:u loc :: !out))
    occs;
  let batch = List.rev !out in
  if batch <> [] then emit batch;
  batch

let emit_reference_doc_stages (budget : nav_budget)
    ~(emit : T.Location.t list -> unit)
    ~(include_decl : bool) ~(def_keys : (string, bool) Hashtbl.t)
    ~(key : string) (current : Document.t list) (imported : Document.t list)
    (workspace : Document.t list) : T.Location.t list =
  let seen = Hashtbl.create 256 in
  let acc = ref [] in
  let emit_docs docs =
    let occs =
      docs |> List.concat_map (fun d -> occurrences_in_doc d ~key)
      |> filter_declarations ~include_decl ~def_keys
    in
    let batch = emit_locations_stage budget seen ~emit occs in
    acc := !acc @ batch
  in
  emit_docs current;
  emit_docs imported;
  emit_docs workspace;
  !acc

let definition_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
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
                let word = nav_word_at_position doc pos in
                let startup_quick_hits =
                  match word with
                  | None -> []
                  | Some (nm, _) ->
                      let key = normalize_name nm in
                      if key = "" then [] else proc_real_defs_by_key ws doc ~key
                in
                if startup_quick_hits <> [] then
                  List.map location_of_def startup_quick_hits
                else
                  let allow_fallback = allow_fallback_for_ws ws doc in
                  match define_under_cursor doc pos with
                  | Some (dm, _) ->
                      [ location_of_def (def_of_preprocess_define doc dm) ]
                  | None -> (
                      match word with
                      | None -> []
                      | Some _ ->
                          let cache : (string, doc_nav) Hashtbl.t =
                            Hashtbl.create 32
                          in
                          let nav = nav_for_doc_cached ws cache doc in
                          let docs_cache : Document.t list option ref =
                            ref None
                          in
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
                                match
                                  Hashtbl.find_opt nav.defs_by_id sym_id
                                with
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
                              if nav_budget_check budget then []
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
                                  (not allow_fallback)
                                  || nav_budget_check budget
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
          | Some _ -> definition_locations_for ws ~uri ~pos
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

let references_locations_for (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) ~(include_decl : bool) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
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
              let word = nav_word_at_position doc pos in
              match word with
              | None -> []
              | Some _ ->
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
                      ( Semantic_store.defs_for_sym_id ws.semantic_store sym_id
                        |> List.map def_of_snapshot_def
                        |> uniq_defs,
                        Semantic_store.refs_for_sym_id ws.semantic_store sym_id
                      )
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
                        let docs = docs_for_rename ws doc in
                        let proc_defs =
                          if allow_fallback_for_ws ws doc then
                            proc_defs_by_key ws doc ~key
                          else []
                        in
                        let defs =
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
                            occurrences_for_docs_with_budget budget docs ~key
                          in
                          let occs =
                            if include_decl then occs
                            else
                              List.filter
                                (fun (u, loc) ->
                                  not
                                    (Hashtbl.mem def_keys (loc_key ~uri:u loc)))
                                occs
                          in
                          locations_with_budget budget occs))
      in
      let result = nav_compute_with_budget_value budget compute in
      schedule_nav_miss_for_result ws doc pos ~empty:(result = []);
      result

let references_locations_stream (ws : t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) ~(include_decl : bool)
    ~(emit : T.Location.t list -> unit) : T.Location.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then []
        else
          let current, imported, workspace = reference_doc_stages ws doc in
          match import_under_cursor doc pos with
          | Some imp ->
              let key = normalize_name imp.name in
              if key = "" then []
              else
                let all_docs = current @ imported @ workspace in
                let defs =
                  all_docs |> List.concat_map collect_doc_defs
                  |> List.filter (fun d -> d.key = key)
                in
                emit_reference_doc_stages budget ~emit ~include_decl
                  ~def_keys:(def_keys_for_defs defs) ~key current imported
                  workspace
          | None -> (
              match nav_word_at_position doc pos with
              | None -> []
              | Some (nm, _) ->
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
                          let imported_keys =
                            let keys = Hashtbl.create 32 in
                            List.iter
                              (fun d ->
                                Hashtbl.replace keys
                                  (uri_key d.Document.uri)
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
                                match
                                  Hashtbl.find_opt dnav.occs_by_id sym_id
                                with
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
                            imported workspace)
      in
      let result = nav_compute_with_budget_value budget compute in
      schedule_nav_miss_for_result ws doc pos ~empty:(result = []);
      result

let background_queue_length (ws : t) : int =
  Queue.length ws.bg_high_small_queue
  + Queue.length ws.bg_norm_small_queue
  + Queue.length ws.bg_root_small_queue
  + Queue.length ws.bg_high_large_queue
  + Queue.length ws.bg_root_large_queue
  + Queue.length ws.bg_norm_large_queue
  + Queue.length ws.parse_worker_jobs

let schedule_open_doc_parse_fallback (ws : t) (doc : Document.t)
    ~(reason_group : string) : unit =
  if enqueue_open_doc_parse_if_pending ~reason_group ws doc then (
    Perf_stats.tick ("sched." ^ reason_group);
    Perf_log.log_event ("background_scheduled_" ^ reason_group)
      ~uri:(Uri_path.docuri_to_string doc.Document.uri)
      ~bytes:(String.length doc.Document.text) ~rev:doc.Document.rev
      ~queue:(background_queue_length ws))

let skeleton_kind_label = function
  | Syntax_cache.SkModule -> "module"
  | Syntax_cache.SkCompool -> "compool"
  | Syntax_cache.SkProcedure -> "procedure"
  | Syntax_cache.SkFunction -> "function"
  | Syntax_cache.SkItem -> "item"
  | Syntax_cache.SkTable -> "table"
  | Syntax_cache.SkBlock -> "block"
  | Syntax_cache.SkType -> "type"
  | Syntax_cache.SkLabel -> "label"
  | Syntax_cache.SkDefineMacro -> "define"

let skeleton_symbol_at_position (doc : Document.t) (pos : T.Position.t) :
    Syntax_cache.skeleton_symbol option =
  match Document.current_parse doc with
  | Some { Document.parsed_syntax = Some syntax; _ } ->
      let symbols = syntax.Syntax_cache.skeleton.symbols in
      let by_position =
        symbols
        |> List.find_opt (fun (sym : Syntax_cache.skeleton_symbol) ->
               position_in_loc pos sym.sk_loc)
      in
      (match by_position with
      | Some _ as hit -> hit
      | None -> (
          match word_at_position doc pos with
          | None -> None
          | Some (word, _) ->
              let key = normalize_name word in
              symbols
              |> List.find_opt (fun (sym : Syntax_cache.skeleton_symbol) ->
                     normalize_name sym.sk_name = key)))
  | _ -> None

let fast_hover_fallback (ws : t) (doc : Document.t) (pos : T.Position.t) :
    T.Hover.t option =
  schedule_open_doc_parse_fallback ws doc ~reason_group:"hover_fallback";
  match skeleton_symbol_at_position doc pos with
  | Some sym ->
      let body =
        let kind = skeleton_kind_label sym.sk_kind in
        let role =
          if sym.sk_imported then "external REF import"
          else if sym.sk_exported then "external DEF"
          else "local"
        in
        let scope =
          match sym.sk_container with None -> "<global>" | Some c -> c
        in
        hover_panel ~name:sym.sk_name
          ~summary:(Printf.sprintf "JOVIAL %s - syntax snapshot" kind)
          ~facts:
            [
              Printf.sprintf "Classification: %s" kind;
              Printf.sprintf "Declaration role: %s" role;
              Printf.sprintf "Exported: %s"
                (if sym.sk_exported then "yes" else "no");
              Printf.sprintf "Imported: %s"
                (if sym.sk_imported then "yes" else "no");
              Printf.sprintf "Scope: `%s`" scope;
              Printf.sprintf "Location: line %d, column %d"
                sym.sk_loc.Ast.Loc.start_pos.line
                (sym.sk_loc.Ast.Loc.start_pos.col + 1);
              "Status: semantic analysis pending";
            ]
          ~sections:
            [
              hover_inline_section "Details"
                "The open document has changed and semantic analysis is still \
                 catching up. Full declaration text, implementation preview, \
                 references, and cross-file details will appear when workspace \
                 indexing catches up.";
            ]
      in
      Some (hover_markdown ~range:(Lsp_conv.range_of_loc sym.sk_loc) body)
  | None -> (
      match word_at_position doc pos with
      | None -> None
      | Some (name, loc) ->
          let body =
            hover_panel ~name ~summary:"Unresolved JOVIAL symbol"
              ~facts:
                [
                  "Status: semantic analysis pending";
                  "Status: no visible declaration was found for this reference.";
                  hover_current_file_fact doc;
                  Printf.sprintf "Position: line %d, column %d"
                    loc.Ast.Loc.start_pos.line
                    (loc.Ast.Loc.start_pos.col + 1);
                ]
              ~sections:
                [
                  hover_inline_section "Next check"
                    "Confirm the symbol is declared in scope, imported from \
                     the expected COMPOOL, or available through the expected \
                     external DEF/REF pair.";
                ]
          in
          Some (hover_markdown ~range:(Lsp_conv.range_of_loc loc) body))

let hover_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.Hover.t option =
  match doc_of_uri ws uri with
  | None -> None
  | Some doc ->
      let hover_t0 = Perf_log.now_ms () in
      Perf_log.log_event "hover_received"
        ~uri:(Uri_path.docuri_to_string uri)
        ~bytes:(String.length doc.Document.text) ~rev:doc.Document.rev;
      let finish result =
        Perf_log.log_event "hover_responded"
          ~uri:(Uri_path.docuri_to_string uri)
          ~bytes:(String.length doc.Document.text) ~rev:doc.Document.rev
          ~ms:(max 0.0 (Perf_log.now_ms () -. hover_t0))
          ~queue:(background_queue_length ws);
        result
      in
      if doc.Document.parse_rev <> doc.Document.rev then
        finish (fast_hover_fallback ws doc pos)
      else finish (hover_semantic_for ws doc ~pos)

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
      if budget.exceeded then None else computed

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
                              if not (nav_budget_check budget) then
                                add_edit u loc)
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
                      if nav_budget_check budget then None else apply_changes ())
                )
        in
        let computed = nav_compute_with_budget_value budget compute in
        if budget.exceeded then None else computed

let completion_item_kind_of_lsp_int (kind : int) : T.CompletionItemKind.t =
  if kind = 3 then T.CompletionItemKind.Function
  else if kind = 7 then T.CompletionItemKind.Class
  else if kind = 9 then T.CompletionItemKind.Module
  else if kind = 10 then T.CompletionItemKind.Property
  else if kind = 14 then T.CompletionItemKind.Keyword
  else if kind = 15 then T.CompletionItemKind.Snippet
  else if kind = 21 then T.CompletionItemKind.Constant
  else T.CompletionItemKind.Variable

let completion_item_t ~(label : string) ~(kind : T.CompletionItemKind.t) ?detail
    ?insert_text ?sort_text () : T.CompletionItem.t =
  T.CompletionItem.create ~label ~kind ?detail ?insertText:insert_text
    ?sortText:sort_text ()

let completion_items_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    : T.CompletionItem.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        let prefix =
          match word_at_position doc pos with None -> "" | Some (nm, _) -> nm
        in
        let seen = Hashtbl.create 512 in
        let out = ref [] in
        let count = ref 0 in
        let max_items = 500 in
        let add_item ~(uniq_key : string) (item : T.CompletionItem.t) : unit =
          if nav_budget_check budget then ()
          else if !count < max_items && not (Hashtbl.mem seen uniq_key) then (
            Hashtbl.replace seen uniq_key true;
            out := item :: !out;
            incr count)
        in
        let add_symbol_item (d : def) : unit =
          if starts_with_ci ~prefix d.name then
            let detail =
              match d.container with
              | None -> Some (kind_name_of_def d)
              | Some c ->
                  Some (Printf.sprintf "%s in %s" (kind_name_of_def d) c)
            in
            let kind =
              completion_item_kind_of_lsp_int
                (completion_item_kind_of_metadata d)
            in
            let uniq_key =
              Printf.sprintf "sym|%s|%d" (normalize_name d.name) d.kind
            in
            let sort_text =
              if same_uri d.uri doc.Document.uri then
                Some ("0_" ^ normalize_name d.name)
              else Some ("1_" ^ normalize_name d.name)
            in
            add_item ~uniq_key
              (completion_item_t ~label:d.name ~kind ?detail ?sort_text ())
        in
        let add_keyword (label : string) (kind : int) (detail : string option) :
            unit =
          if starts_with_ci ~prefix label then
            let uniq_key = "kw|" ^ normalize_name label in
            add_item ~uniq_key
              (completion_item_t ~label
                 ~kind:(completion_item_kind_of_lsp_int kind)
                 ?detail
                 ~sort_text:("2_" ^ normalize_name label)
                 ())
        in
        let add_builtin_function (label : string) (kind : int)
            (detail : string option) : unit =
          if starts_with_ci ~prefix label then
            let uniq_key = "fn|" ^ normalize_name label in
            add_item ~uniq_key
              (completion_item_t ~label
                 ~kind:(completion_item_kind_of_lsp_int kind)
                 ?detail
                 ~sort_text:("3_" ^ normalize_name label)
                 ())
        in
        let add_snippet (label : string) (insert_text : string) (kind : int)
            (detail : string option) : unit =
          if starts_with_ci ~prefix label || starts_with_ci ~prefix insert_text
          then
            let uniq_key = "snip|" ^ normalize_name label in
            add_item ~uniq_key
              (completion_item_t ~label
                 ~kind:(completion_item_kind_of_lsp_int kind)
                 ?detail ~insert_text
                 ~sort_text:("4_" ^ normalize_name label)
                 ())
        in
        docs_for_lookup ws doc
        |> List.iter (fun d ->
            if not (nav_budget_check budget) then
              collect_doc_defs d
              |> List.iter (fun defn ->
                  if not (nav_budget_check budget) then add_symbol_item defn));
        List.iter
          (fun (label, kind, detail) ->
            if not (nav_budget_check budget) then add_keyword label kind detail)
          completion_keywords;
        List.iter
          (fun (label, kind, detail) ->
            if not (nav_budget_check budget) then add_keyword label kind detail)
          completion_types_builtin;
        List.iter
          (fun (label, kind, detail) ->
            if not (nav_budget_check budget) then
              add_builtin_function label kind detail)
          completion_functions_builtin;
        List.iter
          (fun (label, insert_text, kind, detail) ->
            if not (nav_budget_check budget) then
              add_snippet label insert_text kind detail)
          completion_snippets;
        List.rev !out
      in
      nav_compute_with_budget_value budget compute

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
              let defs =
                docs_for_lookup ws doc
                |> List.concat_map collect_doc_defs
                |> List.filter (fun d -> d.kind = sym_kind_type && d.key = key)
                |> uniq_defs
              in
              List.map location_of_def defs
      in
      nav_compute_with_budget_value budget compute

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

let signature_help_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.SignatureHelp.t option =
  match doc_of_uri ws uri with
  | None -> None
  | Some doc ->
      let budget = nav_budget_start ws in
      let compute () =
        if nav_budget_check budget then None
        else
          match call_context_at_position doc pos with
          | None -> None
          | Some (key, active_param) ->
              let defs =
                if nav_budget_check budget then []
                else
                  let docs = docs_for_lookup ws doc in
                  let from_docs =
                    docs
                    |> List.concat_map collect_doc_defs
                    |> List.filter (fun d ->
                        d.kind = sym_kind_func && d.key = key)
                    |> uniq_defs
                  in
                  if from_docs <> [] then from_docs
                  else if allow_fallback_for_ws ws doc then
                    proc_defs_by_key ws doc ~key
                  else []
              in
              if defs = [] then None
              else
                let signatures =
                  defs
                  |> List.filter_map (fun d ->
                      if nav_budget_check budget then None
                      else
                        let parameters_of_label label =
                          split_signature_params label
                          |> List.map (fun p ->
                              T.ParameterInformation.create ~label:(`String p)
                                ())
                        in
                        match proc_signature_for_def ws d with
                        | Some label ->
                            Some
                              (T.SignatureInformation.create ~label
                                 ~parameters:(parameters_of_label label)
                                 ())
                        | None ->
                            Some
                              (T.SignatureInformation.create ~label:d.name
                                 ~parameters:[] ()))
                in
                if signatures = [] then None
                else
                  Some
                    (T.SignatureHelp.create ~signatures ~activeSignature:0
                       ~activeParameter:(Some active_param) ())
      in
      nav_compute_with_budget_value budget compute

let workspace_single_edit ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    ~(new_text : string) : T.WorkspaceEdit.t =
  let range = { T.Range.start = pos; end_ = pos } in
  let edit = T.TextEdit.create ~range ~newText:new_text in
  T.WorkspaceEdit.create ~changes:[ (uri, [ edit ]) ] ()

let quickfix_add_import_code_action ~(doc : Document.t) ~(diag : T.Diagnostic.t)
    ~(compool : string) : T.CodeAction.t option =
  let key = normalize_name compool in
  if key = "" || has_import_for_compool doc key then None
  else
    let pos, append_after_line = import_insert_position doc in
    let text =
      if append_after_line then Printf.sprintf "\n!COMPOOL(\"%s\");" key
      else Printf.sprintf "!COMPOOL(\"%s\");\n" key
    in
    Some
      (T.CodeAction.create
         ~title:(Printf.sprintf "Import COMPOOL %s" key)
         ~kind:T.CodeActionKind.QuickFix ~isPreferred:true ~diagnostics:[ diag ]
         ~edit:(workspace_single_edit ~uri:doc.uri ~pos ~new_text:text)
         ())

let code_actions_for (ws : t) ~(uri : T.DocumentUri.t) ~(range : T.Range.t) :
    T.CodeAction.t list =
  match doc_of_uri ws uri with
  | None -> []
  | Some doc ->
      let diag_in_range (d : T.Diagnostic.t) : bool =
        range_intersects d.range range
      in
      let seen = Hashtbl.create 32 in
      let actions = ref [] in
      let add_action (key : string) (action : T.CodeAction.t option) =
        if not (Hashtbl.mem seen key) then
          match action with
          | None -> ()
          | Some a ->
              Hashtbl.replace seen key true;
              actions := a :: !actions
      in
      Document.diagnostics doc |> List.filter diag_in_range
      |> List.iter (fun (d : T.Diagnostic.t) ->
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
                (quickfix_add_import_code_action ~doc ~diag:d ~compool:c));
      List.rev !actions
