module T = Lsp.Types
open Workspace_state
open Workspace_imports
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_hover_markdown
open Workspace_tuning
module Metadata = Workspace_symbol_metadata

module Perf_stats = Workspace_foundation.Perf_stats

type nav_budget = Workspace_budget.t

let nav_budget_start (ws : t) : nav_budget =
  Workspace_budget.start ~ws ~soft_budget_ms:nav_soft_budget_ms

let nav_budget_check (budget : nav_budget) : bool =
  Workspace_budget.should_stop ~phase:"hover" budget

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
      let file =
        match loc.Ast.Loc.file with
        | Some p -> Filename.basename p
        | None -> "<unknown>"
      in
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
          | target0 :: _ ->
              let target =
                match
                  List.find_opt
                    (fun d -> d.metadata.Metadata.type_info <> None)
                    type_defs
                with
                | Some d -> d
                | None -> target0
              in
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

let field_owner_context ws doc d : string option =
  match d.container with
  | None -> None
  | Some owner ->
      let owner_key = normalize_name owner in
      if owner_key = "" then None
      else
        let docs = doc :: docs_for_lookup ws doc in
        docs
        |> List.concat_map collect_doc_defs
        |> List.find_map (fun candidate ->
               if candidate.key <> owner_key then None
               else
                 match candidate.metadata.Metadata.jovial_kind with
                 | Metadata.JovialTable | Metadata.JovialConstantTable ->
                     Some "table"
                 | Metadata.JovialBlock -> Some "block"
                 | Metadata.JovialType -> Some "type"
                 | _ when candidate.kind = sym_kind_type -> Some "type"
                 | _ -> None)

let same_loc_span (a : Ast.Loc.t) (b : Ast.Loc.t) : bool =
  a.Ast.Loc.start_pos.offset = b.Ast.Loc.start_pos.offset
  && a.Ast.Loc.end_pos.offset = b.Ast.Loc.end_pos.offset

let rec take_first n xs =
  if n <= 0 then []
  else match xs with [] -> [] | x :: rest -> x :: take_first (n - 1) rest

let specified_table_facts_of_type (ty : Ast.type_expr Ast.node) : string list =
  match ty.v with
  | Ast.TSpecifiedTable { elem; dims; kind } ->
      let kind_name, entry_size =
        match kind with
        | Ast.SpecTableW entry_size -> ("W", Some entry_size)
        | Ast.SpecTableV None -> ("V", None)
        | Ast.SpecTableV (Some entry_size) -> ("V", Some entry_size)
      in
      let dim_fact =
        match dims with
        | [] -> []
        | _ ->
            [
              Printf.sprintf "Dimensions: `%s`"
                (dims |> List.map Metadata.expr_display |> String.concat ", ");
            ]
      in
      let entry_fact =
        match entry_size with
        | None -> []
        | Some entry_size ->
            [
              Printf.sprintf "Entry size: `%s`"
                (Metadata.expr_display entry_size);
            ]
      in
      let field_position_facts =
        match elem.v with
        | Ast.TRecord fields ->
            let positions =
              fields
              |> List.filter_map (fun (field : Ast.field_decl Ast.node) ->
                     match field.v.fpos with
                     | None -> None
                     | Some pos ->
                         Some
                           (Printf.sprintf "%s POS(%s,%s)" field.v.fname.v
                              (Metadata.expr_display pos.pos_start_bit)
                              (Metadata.expr_display pos.pos_start_word)))
            in
            if positions = [] then []
            else
              [
                Printf.sprintf "Field positions: `%s`"
                  (positions |> take_first 8 |> String.concat "`, `");
              ]
        | _ -> []
      in
      [
        "Specified table: yes";
        Printf.sprintf "Table kind: %s" kind_name;
      ]
      @ entry_fact @ dim_fact @ field_position_facts
  | _ -> []

let specified_table_facts_for_def doc (d : def) : string list =
  let type_for_decl (decl : Ast.decl Ast.node) : Ast.type_expr Ast.node option =
    match decl.v with
    | Ast.DVar { name; dtype; _ }
    | Ast.DConst { name; dtype = Some dtype; _ }
      when normalize_name name.v = d.key && same_loc_span name.loc d.loc ->
        Some dtype
    | Ast.DType { name; defn; _ }
      when normalize_name name.v = d.key && same_loc_span name.loc d.loc ->
        Some defn
    | _ -> None
  in
  let rec find_decl = function
    | [] -> None
    | Ast.TopDecl decl :: rest -> (
        match type_for_decl decl with
        | Some _ as found -> found
        | None -> (
            match decl.v with
            | Ast.DProc p -> find_decl (List.map (fun d -> Ast.TopDecl d) p.v.locals @ rest)
            | _ -> find_decl rest))
    | Ast.TopStmt _ :: rest -> find_decl rest
  in
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> (
      match find_decl prog with
      | Some ty -> specified_table_facts_of_type ty
      | None -> [])
  | _ -> []

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
    match metadata.jovial_kind with
    | Metadata.JovialField ->
        let owner_fact =
          match d.container with
          | None -> []
          | Some owner -> [ Printf.sprintf "Field owner: `%s`" owner ]
        in
        let context_fact =
          match field_owner_context ws doc d with
          | None -> [ "Field context: table/block/type field" ]
          | Some context ->
              [
                Printf.sprintf "Field context: %s field" context;
              ]
        in
        facts @ owner_fact @ context_fact
    | _ -> facts
  in
  let facts = facts @ specified_table_facts_for_def doc d in
  let facts =
    facts
    @ [
        Printf.sprintf "Constant: %s"
          (if metadata.is_constant then "yes" else "no");
        Printf.sprintf "Readonly: %s"
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

let hover_body_for_def ws doc d =
  let sig_line = proc_signature_for_def ws d in
  let src_line = source_line_for_def ws d in
  let navigation_section =
    if d.kind = sym_kind_func then
      let targets = proc_real_defs_by_key ws doc ~key:d.key in
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
    hover_inline_section "Change impact"
      (Workspace_change_impact.change_impact_for_def ws d)
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

let hover_macro_expansion_for ws doc (exp : Macro_graph.expansion) :
    T.Hover.t =
  let d = exp.Macro_graph.define_def in
  let dm = exp.Macro_graph.define in
  let primary_decl =
    match source_line_for_def ws d with
    | Some line when String.trim line <> "" -> line
    | _ ->
        if dm.Preprocess.requires_call then
          Printf.sprintf "DEFINE %s(%s) \"%s\";" dm.name
            (String.concat "," dm.formals)
            dm.body
        else Printf.sprintf "DEFINE %s \"%s\";" dm.name dm.body
  in
  let decl_block =
    let line = truncate_text 280 primary_decl in
    if String.trim line = "" then "" else hover_code_section "Declaration" line
  in
  let actuals_block =
    match exp.Macro_graph.actuals with
    | [] -> ""
    | actuals ->
        let actual_text =
          actuals
          |> List.map (fun (actual : Macro_graph.macro_actual) ->
                 let value = truncate_text 120 actual.text in
                 match actual.formal with
                 | None -> Printf.sprintf "`%s`" value
                 | Some formal -> Printf.sprintf "`%s` = `%s`" formal value)
          |> String.concat ", "
        in
        hover_inline_section "Actuals" actual_text
  in
  let mapping_fact =
    if exp.Macro_graph.provisional then
      match exp.reason with
      | None -> "Source mapping: provisional"
      | Some reason ->
          Printf.sprintf "Source mapping: provisional (%s)"
            (Workspace_readiness.reason_label reason)
    else "Source mapping: macro expansion source map"
  in
  let sections =
    (if decl_block = "" then [] else [ decl_block ])
    @
    (match source_line_for_def ws d with
    | Some line when String.trim line <> "" ->
        [ hover_code_section "Source declaration" line ]
    | _ -> [])
    @
    (if actuals_block = "" then [] else [ actuals_block ])
    @
    (if dm.Preprocess.formals = [] then
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
    @ [
        hover_inline_section "Change impact"
          (Workspace_change_impact.change_impact_for_def ws d);
      ]
  in
  let facts =
    hover_def_facts ws doc d
    @ [
        Printf.sprintf "Macro call: line %d, column %d"
          exp.call_site_loc.Ast.Loc.start_pos.line
          (exp.call_site_loc.Ast.Loc.start_pos.col + 1);
        mapping_fact;
      ]
  in
  hover_markdown ~range:(Lsp_conv.range_of_loc exp.call_name_loc)
    (hover_panel ~name:d.name ~summary:"JOVIAL define expansion" ~facts
       ~sections)

let hover_semantic_for (ws : t) (doc : Document.t) ~(pos : T.Position.t) :
    T.Hover.t option =
  let budget = nav_budget_start ws in
  let macro_graph = lazy (Macro_graph.of_document doc) in
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
          match
            Macro_graph.macro_use_at_position (Lazy.force macro_graph)
              ~uri:doc.Document.uri ~pos
          with
          | Some exp -> Some (hover_macro_expansion_for ws doc exp)
          | None -> (
          match define_under_cursor doc pos with
          | Some (dm, word_loc) when position_in_loc pos dm.Preprocess.loc ->
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
                @ [
                    hover_inline_section "Change impact"
                      (Workspace_change_impact.change_impact_for_def ws d);
                  ]
              in
              Some
                (hover_markdown ~range:(Lsp_conv.range_of_loc word_loc)
                   (hover_panel ~name:d.name ~summary:"JOVIAL define"
                      ~facts:(hover_def_facts ws doc d) ~sections))
          | Some _ | None ->
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
                  match import_text with
                  | None -> top_lines
                  | Some imp -> imp :: top_lines
                in
                let body = String.concat "\n\n---\n\n" lines in
                let range = Option.map Lsp_conv.range_of_loc hover_loc in
                Some (hover_markdown ?range body)))
  in
  let out = compute () in
  ignore (nav_budget_check budget);
  out

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
