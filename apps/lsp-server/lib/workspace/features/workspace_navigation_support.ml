(* Module overview: Lower-level helpers for resolving symbol locations and import-aware navigation. *)

module T = Lsp.Types
open Workspace_state
open Workspace_index_graph
open Workspace_nav_model
open Workspace_nav_lookup
open Workspace_tuning

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

type nav_budget = Workspace_budget.t

let nav_soft_budget_for_ws (ws : t) : int =
  let profile_budget =
    match Workspace_runtime.workspace_profile_for_budget ws with
    | Workspace_foundation.ProfileSmall -> nav_soft_budget_ms
    | Workspace_foundation.ProfileMedium -> nav_soft_budget_medium_ms
    | Workspace_foundation.ProfileLarge -> nav_soft_budget_large_ms
  in
  if ws.startup_fully_nav_ready_ms = None then
    min profile_budget nav_startup_soft_budget_ms
  else profile_budget

let nav_budget_start (ws : t) : nav_budget =
  let soft_budget_ms = nav_soft_budget_for_ws ws in
  Workspace_foundation.Perf_stats.observe_ms "nav.budget_ms"
    (float_of_int soft_budget_ms);
  Workspace_budget.start ~ws ~soft_budget_ms

let nav_budget_check ?(phase = "nav") (budget : nav_budget) : bool =
  Workspace_budget.should_stop ~phase budget

let allow_fallback_for_ws (ws : t) (doc : Document.t) : bool =
  let startup_allows_fallback =
    match Workspace_runtime.workspace_profile_for_budget ws with
    | Workspace_foundation.ProfileLarge ->
        ws.allow_slow_query_fallback
        || Workspace_runtime.quick_nav_index_complete ws
    | Workspace_foundation.ProfileSmall | Workspace_foundation.ProfileMedium ->
        true
  in
  let allowed = startup_allows_fallback && allow_unscoped_fallback doc in
  if (not allowed) && allow_unscoped_fallback doc then
    Workspace_foundation.Perf_stats.tick "query.slow_fallback_disabled";
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

let occurrences_in_doc_fallback ?budget (doc : Document.t) ~(key : string) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  let text = doc.Document.text in
  let n = String.length text in
  let rec loop i acc =
    if i >= n then List.rev acc
    else if
      i mod 4096 = 0
      &&
      (match budget with
      | Some budget -> nav_budget_check budget
      | None -> false)
    then List.rev acc
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

let occurrences_in_doc ?budget (doc : Document.t) ~(key : string) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  try
    Lexer.with_session_state (fun lexer ->
        let lexbuf = Lexing.from_string doc.Document.text in
        (match doc.Document.file with
        | None -> ()
        | Some f ->
            lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = f });
        let rec loop count acc =
          if
            count mod 512 = 0
            &&
            (match budget with
            | Some budget -> nav_budget_check budget
            | None -> false)
          then List.rev acc
          else
          let tok = Lexer.token lexer lexbuf in
          let sp = Lexing.lexeme_start_p lexbuf in
          let ep = Lexing.lexeme_end_p lexbuf in
          match tok with
          | Parser.EOF -> List.rev acc
          | Parser.ID s when normalize_name s = key ->
              let loc =
                Ast.Loc.of_lexing_positions sp ep ~file:doc.Document.file
              in
              loop (count + 1) ((doc.Document.uri, loc) :: acc)
          | _ -> loop (count + 1) acc
        in
        loop 0 [])
  with _ -> occurrences_in_doc_fallback ?budget doc ~key

let occurrences_for_docs_with_budget (budget : nav_budget)
    (docs : Document.t list) ~(key : string) :
    (T.DocumentUri.t * Ast.Loc.t) list =
  let acc = ref [] in
  List.iter
    (fun d ->
      if not (nav_budget_check budget) then
        acc := List.rev_append (occurrences_in_doc ~budget d ~key) !acc)
    docs;
  List.rev !acc

let starts_with_ci ~(prefix : string) (s : string) : bool =
  let p = normalize_name prefix in
  if p = "" then true
  else
    let u = normalize_name s in
    let lp = String.length p in
    String.length u >= lp && String.sub u 0 lp = p

let nav_compute_with_budget_value ?(phase = "nav") (budget : nav_budget)
    (f : unit -> 'a) : 'a =
  let out = f () in
  ignore (nav_budget_check ~phase budget);
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

let docs_except_seen (seen : (string, bool) Hashtbl.t) (docs : Document.t list) :
    Document.t list =
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

let filter_declarations ~(include_decl : bool)
    ~(def_keys : (string, bool) Hashtbl.t)
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
    List.iter
      (fun d ->
        if not (nav_budget_check budget) then
          let occs =
            occurrences_in_doc ~budget d ~key
            |> filter_declarations ~include_decl ~def_keys
          in
          let batch = emit_locations_stage budget seen ~emit occs in
          acc := !acc @ batch)
      docs
  in
  emit_docs current;
  emit_docs imported;
  emit_docs workspace;
  !acc

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
  let end_of_line line =
    let line = min (max 0 line) (max 0 (line_count - 1)) in
    let ch =
      match Text_index.line_length idx ~line with
      | Some n -> max 0 n
      | None -> 0
    in
    ({ line; character = ch } : T.Position.t)
  in
  let line_max =
    Document.imports doc
    |> List.fold_left
         (fun acc (imp : Preprocess.import) ->
           let line0 = max 0 (imp.loc.start_pos.line - 1) in
           if line0 > acc then line0 else acc)
         (-1)
  in
  if line_max >= 0 && line_count > 0 then (end_of_line line_max, true)
  else
    let rec find_start line =
      if line >= line_count then None
      else
        match line_text_in_doc doc ~line1:(line + 1) with
        | None -> find_start (line + 1)
        | Some text ->
            let trimmed = String.trim text |> String.uppercase_ascii in
            if
              String.length trimmed >= 5
              && String.sub trimmed 0 5 = "START"
              && (String.length trimmed = 5
                 ||
                 match trimmed.[5] with
                 | ' ' | '\t' | ';' | ',' -> true
                 | _ -> false)
            then Some line
            else find_start (line + 1)
    in
    match find_start 0 with
    | Some line -> (end_of_line line, true)
    | None -> (({ line = 0; character = 0 } : T.Position.t), false)

let has_import_for_compool (doc : Document.t) (name : string) : bool =
  let key = normalize_name name in
  Document.imports doc
  |> List.exists (fun (imp : Preprocess.import) -> normalize_name imp.name = key)

let workspace_single_edit ~(uri : T.DocumentUri.t) ~(pos : T.Position.t)
    ~(new_text : string) : T.WorkspaceEdit.t =
  let range = { T.Range.start = pos; end_ = pos } in
  let edit = T.TextEdit.create ~range ~newText:new_text in
  T.WorkspaceEdit.create ~changes:[ (uri, [ edit ]) ] ()
