(* Module overview: Hover provider that combines syntax, semantic, type, and navigation metadata. *)

module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_imports
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_hover_markdown
open Workspace_tuning
module Metadata = Workspace_symbol_metadata

module Perf_stats = Workspace_foundation.Perf_stats

type nav_budget = Workspace_budget.t

let nav_soft_budget_for_ws (ws : t) : int =
  let profile_budget =
    match Workspace_runtime.workspace_profile_for_budget ws with
    | ProfileSmall -> nav_soft_budget_ms
    | ProfileMedium -> nav_soft_budget_medium_ms
    | ProfileLarge -> nav_soft_budget_large_ms
  in
  if ws.startup_fully_nav_ready_ms = None then
    min profile_budget nav_startup_soft_budget_ms
  else profile_budget

let nav_budget_start (ws : t) : nav_budget =
  let soft_budget_ms = nav_soft_budget_for_ws ws in
  Perf_stats.observe_ms "nav.budget_ms" (float_of_int soft_budget_ms);
  Workspace_budget.start ~ws ~soft_budget_ms

let nav_budget_check (budget : nav_budget) : bool =
  Workspace_budget.should_stop ~phase:"hover" budget

let allow_fallback_for_ws (ws : t) (doc : Document.t) : bool =
  let startup_allows_fallback =
    match Workspace_runtime.workspace_profile_for_budget ws with
    | ProfileLarge ->
        ws.allow_slow_query_fallback
        || Workspace_runtime.quick_nav_index_complete ws
    | ProfileSmall | ProfileMedium -> true
  in
  let allowed = startup_allows_fallback && allow_unscoped_fallback doc in
  if (not allowed) && allow_unscoped_fallback doc then
    Perf_stats.tick "query.slow_fallback_disabled";
  allowed

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

let is_fast_scoped_hover_target (d : def) : bool =
  d.kind <> sym_kind_field && d.kind <> sym_kind_module

let is_fast_scoped_hover_kind (kind : int) : bool =
  kind <> sym_kind_field && kind <> sym_kind_module

let fast_local_defs_before_position (ws : t) (doc : Document.t) (pos : T.Position.t)
    ~(key : string) : def list =
  if key = "" then []
  else
    let matching defs =
      defs
      |> List.filter (fun d ->
             same_uri d.uri doc.Document.uri && d.key = key
             && is_fast_scoped_hover_target d)
    in
    let parsed_authoritative =
      match Document.current_parse doc with
      | Some { Document.parsed_ast = Some _; parsed_diags = []; _ } -> true
      | _ -> false
    in
    let prefer_doc_defs = String.length doc.Document.text <= 262_144 in
    let doc_defs = if prefer_doc_defs then matching (collect_doc_defs doc) else [] in
    let semantic_defs =
      if doc_defs <> [] || prefer_doc_defs || not ws.sem_store_enabled then []
      else
        let cursor_off =
          Text_index.offset_of_line_col doc.Document.index
            ~line:pos.T.Position.line ~col:pos.T.Position.character
        in
        let best_before : Semantic_store.Snapshot.nav_def option ref =
          ref None
        in
        let fallback = ref [] in
        Semantic_store.defs_for_key_in_uri ws.semantic_store
          ~uri:doc.Document.uri ~key
        |> List.iter (fun (d : Semantic_store.Snapshot.nav_def) ->
               if
                 same_uri d.uri doc.Document.uri && d.key = key
                 && is_fast_scoped_hover_kind d.kind
               then
                 match cursor_off with
                 | Some off when d.loc.Ast.Loc.start_pos.offset <= off -> (
                     match !best_before with
                     | None -> best_before := Some d
                     | Some prev
                       when d.loc.Ast.Loc.start_pos.offset
                            > prev.loc.Ast.Loc.start_pos.offset ->
                         best_before := Some d
                     | Some _ -> ())
                 | _ -> fallback := d :: !fallback);
        (match !best_before with
        | Some d -> [ def_of_snapshot_def d ]
        | None -> !fallback |> List.rev_map def_of_snapshot_def)
    in
    let defs =
      if doc_defs <> [] then doc_defs
      else if semantic_defs <> [] then semantic_defs
      else if prefer_doc_defs then []
      else matching (collect_doc_defs doc)
    in
    let defs =
      if defs <> [] || parsed_authoritative then defs
      else matching (fallback_line_defs doc)
    in
    defs |> prefer_local_defs_before_position doc pos

let fast_imported_defs_for_key (ws : t) (doc : Document.t) ~(key : string) :
    def list =
  if key = "" then []
  else
    let filter = List.filter is_fast_scoped_hover_target in
    let rec take n acc = function
      | [] -> List.rev acc
      | _ when n <= 0 -> List.rev acc
      | x :: tl -> take (n - 1) (x :: acc) tl
    in
    let semantic_hits =
      semantic_defs_for_imported_compools ~max_defs:4 ws doc ~key |> filter
    in
    if semantic_hits <> [] then semantic_hits
    else
      let summary_hits =
        summary_defs_for_imported_compools ~max_defs:4 ws doc ~key |> filter
      in
      if summary_hits <> [] then summary_hits
      else prefix_defs_for_imported_compools ws doc ~key |> filter |> take 4 []

let fast_global_proc_defs_for_key (ws : t) ~(key : string) : def list =
  if key = "" then []
  else (
    Perf_stats.tick "query.fast_global_proc_lookup";
    let line_hits =
      Perf_stats.time "query.fast_global_proc_line_ms" (fun () ->
          Hashtbl.fold
            (fun _ doc acc ->
              if String.length doc.Document.text > 262_144 then acc
              else
                fallback_line_defs doc
                |> List.filter (fun d -> d.key = key && d.kind = sym_kind_func)
                |> List.rev_append acc)
            ws.files [])
    in
    let semantic_hits =
      if line_hits <> [] || not ws.sem_store_enabled then []
      else
        Perf_stats.time "query.fast_global_proc_semantic_ms" (fun () ->
            Semantic_store.defs_for_key_kind ws.semantic_store ~key
              ~kind:sym_kind_func
            |> List.map def_of_snapshot_def
            |> List.filter (fun d -> d.key = key && d.kind = sym_kind_func))
    in
    let hits =
      Perf_stats.time "query.fast_global_proc_rank_ms" (fun () ->
          if line_hits <> [] then line_hits |> prefer_non_ref_targets |> uniq_defs
          else
            semantic_hits
            |> prefer_real_definition_targets ws
            |> prefer_non_ref_targets
            |> uniq_defs)
    in
    if hits <> [] then Perf_stats.tick "query.fast_global_proc_hit";
    hits)

let fast_type_origin_label = function
  | Metadata.BuiltinType -> "built-in type"
  | Metadata.UserDefinedType _ -> "user-defined type"
  | Metadata.InferredType -> "inferred type"
  | Metadata.UnknownType -> "unknown type"

let fast_local_hover_body (doc : Document.t) (d : def) : string =
  let metadata = d.metadata in
  let kind =
    match metadata.Metadata.jovial_kind with
    | Metadata.JovialUnknownSymbol -> Workspace_symbol_kinds.role_of_def_kind d.kind
    | kind -> Metadata.symbol_kind_label kind
  in
  let type_facts =
    match metadata.type_info with
    | None -> []
    | Some ti ->
        [ Printf.sprintf "Type: `%s`" ti.Metadata.display ]
        @ [ Printf.sprintf "Type origin: %s" (fast_type_origin_label ti.origin) ]
        @
        (match ti.explanation with
        | None -> []
        | Some e ->
            [ Printf.sprintf "Resolved type: `%s` - %s" ti.display e ])
  in
  let facts =
    [
      Printf.sprintf "Classification: %s" kind;
      Printf.sprintf "Declaration role: %s"
        (Metadata.decl_role_label metadata.decl_role);
    ]
    @ type_facts
    @ [
        Printf.sprintf "Constant: %s"
          (if metadata.is_constant then "yes" else "no");
        Printf.sprintf "Readonly: %s"
          (if metadata.is_constant || metadata.is_readonly then "yes" else "no");
      ]
    @
    (match d.container with
    | None -> []
    | Some c -> [ Printf.sprintf "Scope: `%s`" c ])
    @ [
        source_path_fact_for_def d;
        declaration_location_fact_for_def d;
        Printf.sprintf "Symbol key: `%s`" d.key;
      ]
  in
  let decl_block =
    match line_text_in_doc doc ~line1:d.loc.Ast.Loc.start_pos.line with
    | None -> ""
    | Some line ->
        let line = truncate_text 600 line in
        if String.trim line = "" then ""
        else hover_code_section "Declaration" line
  in
  let sections =
    if String.trim decl_block = "" then [] else [ decl_block ]
  in
  hover_panel ~name:d.name ~summary:(Metadata.metadata_summary metadata) ~facts
    ~sections

let hover_current_file_fact (doc : Document.t) : string =
  match doc.Document.file with
  | Some p -> Printf.sprintf "Current file: `%s`" p
  | None ->
      Printf.sprintf "Current file: `%s`"
        (Uri_path.docuri_to_string doc.Document.uri)

let builtin_type_hover_at ?implementation_config (doc : Document.t)
    (pos : T.Position.t) : T.Hover.t option =
  let implementation_config : Implementation_config.t option =
    implementation_config
  in
  match word_at_position doc pos with
  | None -> None
  | Some (name, loc) ->
      if
        (not (Metadata.is_builtin_type_name name))
        || is_navigation_literal_like doc name loc
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
        let key = normalize_name name in
        let display, dims =
          match (key, dims, implementation_config) with
          | "F", [], Some { Implementation_config.float_precision = Some n; _ }
            ->
              let dim = string_of_int n in
              (name ^ " " ^ dim, [ dim ])
          | "A", [], Some { Implementation_config.fixed_precision = Some n; _ }
            ->
              let fraction = string_of_int n in
              (name ^ " ?," ^ fraction, [ "?"; fraction ])
          | _ -> (display, dims)
        in
        let cls, meaning =
          match key with
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
          match (key, dims) with
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

let system_subroutine_hover_at (ws : t) (doc : Document.t)
    (pos : T.Position.t) : T.Hover.t option =
  match nav_word_at_position doc pos with
  | None -> None
  | Some (name, loc) ->
      if
        not
          (Implementation_config.is_system_subroutine ws.implementation_config
             name)
      then None
      else
        let metadata = Metadata.system_subroutine_metadata in
        let facts =
          [
            Printf.sprintf "Classification: %s"
              (Metadata.symbol_kind_label metadata.jovial_kind);
            Printf.sprintf "Declaration role: %s"
              (Metadata.decl_role_label metadata.decl_role);
            Printf.sprintf "External kind: %s"
              (Metadata.external_label metadata.external_kind);
            hover_current_file_fact doc;
          ]
          @
          match ws.implementation_config.Implementation_config.dialect with
          | None -> []
          | Some dialect -> [ Printf.sprintf "Dialect/profile: `%s`" dialect ]
        in
        let body =
          hover_panel ~name ~summary:"JOVIAL system procedure" ~facts
            ~sections:
              [
                hover_inline_section "Configuration"
                  "This routine is supplied by the active implementation \
                   profile, so unresolved-reference diagnostics are suppressed.";
              ]
        in
        Some (hover_markdown ~range:(Lsp_conv.range_of_loc loc) body)

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
            if scoped <> [] then scoped
            else if allow_fallback_for_ws ws doc then
              find_type_defs (docs_for_rename ws doc)
            else []
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

let layout_facts_of_type ws (ty : Ast.type_expr Ast.node) : string list =
  let env = Jovial_compile_time.empty_env () in
  Jovial_compile_time.add_implementation_config env ws.implementation_config;
  match
    Jovial_layout.table_layout_of_type
      ~config:
        (Jovial_layout.config_of_implementation_config
           ws.implementation_config)
      env ty
  with
  | None -> []
  | Some layout ->
      let entry_fact =
        [
          Printf.sprintf "Layout entry size: `%s`"
            (Jovial_layout.size_display layout.entry_size_bits);
        ]
      in
      let total_fact =
        match layout.total_size_bits with
        | Jovial_layout.UnknownBits _ -> []
        | size ->
            [
              Printf.sprintf "Layout total size: `%s`"
                (Jovial_layout.size_display size);
            ]
      in
      let field_fact =
        let field_text =
          layout.fields
          |> List.filter_map (fun (field : Jovial_layout.field_layout) ->
                 match field.absolute_bit_offset with
                 | Some offset -> (
                     match field.size_bits with
                     | Jovial_layout.KnownBits bits ->
                         Some
                           (Printf.sprintf "%s @ bit %Ld + %Ld bits"
                              field.field_name offset bits)
                     | Jovial_layout.UnknownBits reason ->
                         Some
                           (Printf.sprintf "%s @ bit %Ld + unknown (%s)"
                              field.field_name offset reason))
                 | None -> None)
          |> take_first 8
        in
        match field_text with
        | [] -> []
        | fields ->
            [
              Printf.sprintf "Field layout: `%s`"
                (String.concat "`, `" fields);
            ]
      in
      entry_fact @ total_fact @ field_fact

let specified_table_facts_for_def ws doc (d : def) : string list =
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
      | Some ty -> specified_table_facts_of_type ty @ layout_facts_of_type ws ty
      | None -> [])
  | _ -> []

let overlay_facts_for_def doc (d : def) : string list =
  let rec target_names acc (item : Ast.overlay_item Ast.node) =
    match item.v with
    | Ast.OverlayTarget id -> id.v :: acc
    | Ast.OverlaySpacer _ -> acc
    | Ast.OverlayGroup items -> List.fold_left target_names acc items
  in
  let facts_of_overlay (overlay : Ast.overlay_decl) =
    let targets =
      List.rev (List.fold_left target_names [] overlay.overlay_items)
    in
    let target_fact =
      match targets with
      | [] -> []
      | _ ->
          [
            Printf.sprintf "Overlay targets: `%s`"
              (targets |> take_first 12 |> String.concat "`, `");
          ]
    in
    let pos_fact =
      match overlay.overlay_pos with
      | None -> []
      | Some pos ->
          [
            Printf.sprintf "Overlay POS: `%s`" (Metadata.expr_display pos);
          ]
    in
    target_fact @ pos_fact
  in
  let rec find_decl = function
    | [] -> None
    | Ast.TopDecl { v = Ast.DOverlay overlay; _ } :: _rest
      when normalize_name overlay.overlay_name.v = d.key
           && same_loc_span overlay.overlay_name.loc d.loc ->
        Some overlay
    | Ast.TopDecl { v = Ast.DProc p; _ } :: rest ->
        find_decl (List.map (fun d -> Ast.TopDecl d) p.v.locals @ rest)
    | _ :: rest -> find_decl rest
  in
  match Document.current_parse doc with
  | Some { Document.parsed_ast = Some prog; _ } -> (
      match find_decl prog with Some overlay -> facts_of_overlay overlay | None -> [])
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
  let facts = facts @ specified_table_facts_for_def ws doc d in
  let facts =
    match metadata.jovial_kind with
    | Metadata.JovialOverlay -> facts @ overlay_facts_for_def doc d
    | _ -> facts
  in
  let facts =
    facts
    @ [
        Printf.sprintf "Constant: %s"
          (if metadata.is_constant then "yes" else "no");
        Printf.sprintf "Readonly: %s"
          (if metadata.is_constant || metadata.is_readonly then "yes" else "no");
      ]
  in
  let facts =
    if metadata.is_inline then facts @ [ "Inline: yes" ] else facts
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

let hover_cache_key (doc : Document.t) (d : def) : string =
  Printf.sprintf "%s|%d|%s|%s|%d:%d-%d:%d|%d"
    (Uri_path.docuri_to_string d.uri)
    doc.Document.parse_rev d.key d.name d.loc.Ast.Loc.start_pos.line
    d.loc.Ast.Loc.start_pos.col d.loc.Ast.Loc.end_pos.line
    d.loc.Ast.Loc.end_pos.col d.kind

let hover_body_for_def_uncached ws doc d =
  let sig_line = proc_signature_for_def ws d in
  let src_line = source_line_for_def ws d in
  let navigation_section =
    if d.kind = sym_kind_func then
      let targets =
        if allow_fallback_for_ws ws doc then proc_real_defs_by_key ws doc ~key:d.key
        else proc_index_real_defs_by_key ws doc ~key:d.key
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

let hover_body_for_def ?budget ws doc d =
  let key = hover_cache_key doc d in
  match Hashtbl.find_opt ws.hover_body_cache key with
  | Some body ->
      Perf_stats.tick "hover.body_cache_hit";
      body
  | None ->
      Perf_stats.tick "hover.body_cache_miss";
      let budget_exhausted =
        match budget with None -> false | Some budget -> nav_budget_check budget
      in
      if budget_exhausted then fast_local_hover_body doc d
      else
        let body =
          Perf_stats.time "hover.body_for_def_ms" (fun () ->
              hover_body_for_def_uncached ws doc d)
        in
        if Hashtbl.length ws.hover_body_cache > 4096 then
          Hashtbl.clear ws.hover_body_cache;
        Hashtbl.replace ws.hover_body_cache key body;
        body

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
  let fast_local_hover () =
    match nav_word_at_position doc pos with
    | None -> None
    | Some (nm, loc) ->
        let key = normalize_name nm in
        let defs = fast_local_defs_before_position ws doc pos ~key in
        if defs = [] then None
        else
          let lines =
            defs
            |> List.map (hover_body_for_def ~budget ws doc)
            |> List.filter (fun line -> String.trim line <> "")
          in
          let body = String.concat "\n\n---\n\n" lines in
          Some (hover_markdown ~range:(Lsp_conv.range_of_loc loc) body)
  in
  let compute () =
    if nav_budget_check budget then None
    else
      match
        Macro_graph.macro_use_at_position (Lazy.force macro_graph)
          ~uri:doc.Document.uri ~pos
      with
      | Some exp -> Some (hover_macro_expansion_for ws doc exp)
      | None ->
      match fast_local_hover () with
      | Some _ as hover -> hover
      | None ->
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
      match builtin_type_hover_at ~implementation_config:ws.implementation_config doc pos with
      | Some hover -> Some hover
      | None -> (
          match system_subroutine_hover_at ws doc pos with
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
              let word = nav_word_at_position doc pos in
              let fast_defs =
                match word with
                | None -> []
                | Some (nm, _) ->
                    let key = normalize_name nm in
                    let local_defs =
                      fast_local_defs_before_position ws doc pos ~key
                    in
                    if local_defs <> [] then local_defs
                    else
                      let global_proc_defs = fast_global_proc_defs_for_key ws ~key in
                      if global_proc_defs <> [] then global_proc_defs
                      else fast_imported_defs_for_key ws doc ~key
              in
              if fast_defs <> [] then
                let lines =
                  fast_defs
                  |> List.map (fun d -> hover_body_for_def ~budget ws doc d)
                  |> List.filter (fun line -> String.trim line <> "")
                in
                let body = String.concat "\n\n---\n\n" lines in
                let range =
                  match word with
                  | Some (_, loc) -> Some (Lsp_conv.range_of_loc loc)
                  | None -> None
                in
                Some (hover_markdown ?range body)
              else if word = None then (
                match import_text with
                | Some txt -> Some (hover_markdown txt)
                | None -> None)
              else
              let cache : (string, doc_nav) Hashtbl.t = Hashtbl.create 32 in
              let nav = nav_for_doc_cached ws cache doc in
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
                      let real_defs =
                        if
                          ws.startup_fully_nav_ready_ms = None
                          || not (allow_fallback_for_ws ws doc)
                        then
                          proc_index_real_defs_by_key ws doc ~key:d.key
                        else proc_real_defs_by_key ws doc ~key:d.key
                      in
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
                        else if
                          ws.startup_fully_nav_ready_ms = None
                          || not (allow_fallback_for_ws ws doc)
                        then
                          proc_index_real_defs_by_key ws doc ~key
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
                let top_lines =
                  match defs with
                  | [] -> []
                  | d :: tl ->
                      let first = hover_body_for_def ~budget ws doc d in
                      let rec rest acc = function
                        | [] -> List.rev acc
                        | _ when nav_budget_check budget -> List.rev acc
                        | d :: tl ->
                            rest (hover_body_for_def ~budget ws doc d :: acc) tl
                      in
                      rest [ first ] tl
                in
                let lines =
                  match import_text with
                  | None -> top_lines
                  | Some imp -> imp :: top_lines
                in
                let body = String.concat "\n\n---\n\n" lines in
                let range = Option.map Lsp_conv.range_of_loc hover_loc in
                Some (hover_markdown ?range body))))
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
          match nav_word_at_position doc pos with
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
      match nav_word_at_position doc pos with
      | None -> None
      | Some (name, loc) ->
          if
            Implementation_config.is_system_subroutine ws.implementation_config
              name
          then system_subroutine_hover_at ws doc pos
          else
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

let hover_skeleton_fallback_for (ws : t) (doc : Document.t)
    ~(pos : T.Position.t) : T.Hover.t option =
  fast_hover_fallback ws doc pos

let hover_for (ws : t) ~(uri : T.DocumentUri.t) ~(pos : T.Position.t) :
    T.Hover.t option =
  match doc_of_uri ws uri with
  | None -> None
  | Some doc ->
      let hover_t0 = Perf_log.now_ms () in
      Perf_stats.tick "hover.received";
      let finish result =
        Perf_stats.observe_ms "hover.responded_ms"
          (max 0.0 (Perf_log.now_ms () -. hover_t0));
        ignore (background_queue_length ws);
        result
      in
      if doc.Document.parse_rev <> doc.Document.rev then
        finish (fast_hover_fallback ws doc pos)
      else finish (hover_semantic_for ws doc ~pos)
