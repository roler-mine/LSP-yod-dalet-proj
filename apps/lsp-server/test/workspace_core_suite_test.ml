module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let mk_temp_dir (prefix : string) : string =
  let root = Filename.get_temp_dir_name () in
  let rec pick attempts =
    if attempts <= 0 then failf "failed to create temp dir for %s" prefix;
    let name =
      Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ())
        (Random.bits () land 0xFFFFFF)
    in
    let path = Filename.concat root name in
    try
      Unix.mkdir path 0o755;
      path
    with _ -> pick (attempts - 1)
  in
  pick 32

let write_text (path : string) (text : string) : unit =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let read_text (path : string) : string =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let text = really_input_string ic len in
  close_in ic;
  text

let jovial_hover_nav_fixture (name : string) : string =
  let rec candidates_from dir depth acc =
    if depth < 0 then acc
    else
      let acc =
        Filename.concat dir
          (Filename.concat "test"
             (Filename.concat "fixtures"
                (Filename.concat "jovial_hover_nav" name)))
        :: Filename.concat dir
             (Filename.concat "apps"
                (Filename.concat "lsp-server"
                   (Filename.concat "test"
                      (Filename.concat "fixtures"
                         (Filename.concat "jovial_hover_nav" name)))))
        :: acc
      in
      let parent = Filename.dirname dir in
      if parent = dir then acc else candidates_from parent (depth - 1) acc
  in
  let candidates = candidates_from (Sys.getcwd ()) 8 [] in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failf "missing jovial_hover_nav fixture %s" name

let position_of_offset (text : string) (off : int) : T.Position.t =
  let rec loop i line character =
    if i >= off then ({ line; character } : T.Position.t)
    else if text.[i] = '\n' then loop (i + 1) (line + 1) 0
    else loop (i + 1) line (character + 1)
  in
  loop 0 0 0

let find_nth (text : string) ~(needle : string) ~(nth : int) : int =
  let n = String.length text in
  let m = String.length needle in
  let rec loop i seen =
    if i + m > n then failf "missing %S occurrence %d" needle nth
    else if String.sub text i m = needle then
      if seen = nth then i else loop (i + 1) (seen + 1)
    else loop (i + 1) seen
  in
  loop 0 0

let range_of_substring (text : string) ~(needle : string) ~(nth : int) :
    T.Range.t =
  let start_off = find_nth text ~needle ~nth in
  let end_off = start_off + String.length needle in
  {
    T.Range.start = position_of_offset text start_off;
    end_ = position_of_offset text end_off;
  }

let string_contains ~(needle : string) (text : string) : bool =
  let n = String.length text in
  let m = String.length needle in
  if m = 0 then true
  else if m > n then false
  else
    let rec at i j =
      j = m || (text.[i + j] = needle.[j] && at i (j + 1))
    in
    let rec loop i =
      i + m <= n && (at i 0 || loop (i + 1))
    in
    loop 0

let find_substring_from (text : string) ~(needle : string) ~(start : int) :
    int option =
  let n = String.length text in
  let m = String.length needle in
  if m = 0 then Some start
  else
    let rec at i j =
      j = m || (i + j < n && text.[i + j] = needle.[j] && at i (j + 1))
    in
    let rec loop i =
      if i + m > n then None else if at i 0 then Some i else loop (i + 1)
    in
    loop start

let parse_lsp_messages (raw : string) : Yojson.Safe.t list =
  let sep = "\r\n\r\n" in
  let content_length header =
    let rec find = function
    | [] -> failf "missing Content-Length header"
    | line :: rest ->
        let line = String.trim line in
        let prefix = "Content-Length:" in
        if String.starts_with ~prefix line then
          String.sub line (String.length prefix)
            (String.length line - String.length prefix)
          |> String.trim |> int_of_string
        else find rest
    in
    find (String.split_on_char '\n' header)
  in
  let rec loop off acc =
    if off >= String.length raw then List.rev acc
    else
      match find_substring_from raw ~needle:sep ~start:off with
      | None -> failf "missing LSP header separator"
      | Some sep_at ->
          let header = String.sub raw off (sep_at - off) in
          let len = content_length header in
          let payload_start = sep_at + String.length sep in
          let payload = String.sub raw payload_start len in
          loop (payload_start + len) (Yojson.Safe.from_string payload :: acc)
  in
  loop 0 []

let normalize_path (path : string) : string = Lib.Uri_path.normalize_path_key path
let normalize_name (name : string) : string = String.uppercase_ascii (String.trim name)

let path_in (paths : string list) (path : string) : bool =
  let want = normalize_path path in
  List.exists (fun got -> normalize_path got = want) paths

let expect_true name got = if not got then failf "%s: expected true" name
let expect_false name got = if got then failf "%s: expected false" name

let expect_not_contains name ~(needle : string) (text : string) =
  if string_contains ~needle text then
    failf "%s: did not expect hover text to contain %S" name needle

let expect_no_hover_metadata name (text : string) =
  List.iter
    (fun needle -> expect_not_contains name ~needle text)
    [
      "cached definitions";
      "Cached definitions";
      "place of origin";
      "workspace index";
      "workspace index hit";
      "candidate count";
      "fast index fallback";
      "indexed symbol";
      "immutable snapshot";
    ]

let expect_int name got want =
  if got <> want then failf "%s: expected %d, got %d" name want got

let expect_some name = function
  | Some value -> value
  | None -> failf "%s: expected Some _" name

let expect_none name = function
  | None -> ()
  | Some _ -> failf "%s: expected None" name

let expect_path name paths path =
  if not (path_in paths path) then
    failf "%s: expected %s in [%s]" name path (String.concat "; " paths)

let main_text proc_name =
  String.concat "\n"
    [
      "START";
      "!COMPOOL ('TARGET');";
      Printf.sprintf "DEF PROC %s RENT;" proc_name;
      "BEGIN";
      "END";
      "TERM";
      "";
    ]

let target_text =
  String.concat "\n"
    [ "START"; "COMPOOL TARGET;"; "DEF BEGIN"; "  ITEM TARGET'VAL U 1;"; "END"; "TERM"; "" ]

let hidden_text =
  String.concat "\n" [ "START"; "COMPOOL HIDDEN;"; "TERM"; "" ]

let test_source_set_index () =
  let root = mk_temp_dir "jovial-source-set-index" in
  let nested = Filename.concat root "nested" in
  Unix.mkdir nested 0o755;
  let main_path = Filename.concat root "MAIN.j73" in
  let target_path = Filename.concat root "TARGET.j73" in
  let hidden_path = Filename.concat nested "HIDDEN.j73" in
  let note_path = Filename.concat root "notes.txt" in
  write_text main_path (main_text "MAIN");
  write_text target_path target_text;
  write_text hidden_path hidden_text;
  write_text note_path "COMPOOL NOTE;\n";

  let idx =
    Lib.Workspace_index.of_source_files ~source_extensions:[ ".j73" ] ~root
      ~paths:[ main_path; target_path; note_path ]
  in
  expect_int "source count ignores non-Jovial and unlisted files"
    (Lib.Workspace_index.source_count idx) 2;
  expect_path "MAIN import hint"
    (Lib.Workspace_index.source_import_hints idx ~path:main_path)
    "TARGET";
  ignore (expect_some "TARGET compool" (Lib.Workspace_index.find_compool idx ~name:"TARGET"));
  expect_none "HIDDEN compool is not discovered by crawl"
    (Lib.Workspace_index.find_compool idx ~name:"HIDDEN");
  expect_path "MAIN proc hint"
    (Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"MAIN")
    main_path;

  write_text main_path (main_text "NEXT");
  ignore
    (Lib.Workspace_index.apply_file_change idx ~path:main_path ~kind:Changed);
  expect_false "old proc hint removed"
    (path_in (Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"MAIN") main_path);
  expect_path "new proc hint added"
    (Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"NEXT")
    main_path;

  ignore
    (Lib.Workspace_index.apply_file_change idx ~path:target_path ~kind:Deleted);
  expect_none "deleted compool removed"
    (Lib.Workspace_index.find_compool idx ~name:"TARGET");

  let created_path = Filename.concat root "CREATED.j73" in
  write_text created_path
    (String.concat "\n" [ "START"; "COMPOOL CREATED;"; "TERM"; "" ]);
  ignore
    (Lib.Workspace_index.apply_file_change idx ~path:created_path ~kind:Created);
  ignore (expect_some "created compool added" (Lib.Workspace_index.find_compool idx ~name:"CREATED"));
  expect_int "created source counted" (Lib.Workspace_index.source_count idx) 2

let test_workspace_index_health_watchdog_repairs_index () =
  let root = mk_temp_dir "jovial-index-health" in
  let main_path = Filename.concat root "MAIN.j73" in
  let target_path = Filename.concat root "TARGET.j73" in
  write_text main_path (main_text "MAIN");
  write_text target_path target_text;
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ main_path; target_path ]);
  expect_true "initial index health creates index"
    (Lib.Workspace_index_graph.ensure_index_health ws);
  let idx =
    expect_some "initial healthy index" ws.Lib.Workspace_foundation.index
  in
  expect_int "initial healthy source count" (Lib.Workspace_index.source_count idx)
    2;
  ws.Lib.Workspace_foundation.index <- None;
  expect_true "missing index is restored"
    (Lib.Workspace_index_graph.ensure_index_health ws);
  let restored =
    expect_some "restored index" ws.Lib.Workspace_foundation.index
  in
  expect_int "restored source count" (Lib.Workspace_index.source_count restored)
    2;
  let stale =
    Lib.Workspace_index.of_source_files ~source_extensions:[ ".j73" ] ~root
      ~paths:[ main_path ]
  in
  ws.Lib.Workspace_foundation.index <- Some stale;
  expect_true "stale index source set is repaired"
    (Lib.Workspace_index_graph.ensure_index_health ws);
  let repaired =
    expect_some "repaired index" ws.Lib.Workspace_foundation.index
  in
  expect_int "repaired source count" (Lib.Workspace_index.source_count repaired)
    2

let syntax_text =
  String.concat "\n"
    [
      "START";
      "DEFINE TWELVE \"12\";";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  ITEM NUM U 6;";
      "  NUM = TWELVE;";
      "END";
      "TERM";
      "";
    ]

let test_document_syntax_cache () =
  let uri =
    expect_some "syntax URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-core-suite.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text:syntax_text in
  let cache = expect_some "document syntax cache" doc.Lib.Document.syntax in
  expect_true "raw token stream retained"
    (match cache.raw_tokens with Some toks -> Array.length toks > 0 | None -> false);
  expect_true "syntax units retained" (cache.units <> []);
  ignore (expect_some "document AST retained" doc.Lib.Document.ast)

let test_document_stale_parse_state () =
  let uri =
    expect_some "stale parse URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-stale-parse.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text:syntax_text in
  ignore (expect_some "initial AST" doc.Lib.Document.ast);
  ignore (expect_some "initial syntax" doc.Lib.Document.syntax);
  let value_off = find_nth syntax_text ~needle:"TWELVE" ~nth:1 in
  let value_end = value_off + String.length "TWELVE" in
  let range =
    {
      T.Range.start = position_of_offset syntax_text value_off;
      end_ = position_of_offset syntax_text value_end;
    }
  in
  let change =
    T.TextDocumentContentChangeEvent.create ~range ~text:"ELEVEN" ()
  in
  let stale = Lib.Document.apply_changes_no_reparse ~changes:[ change ] doc in
  expect_int "edit advances document revision" stale.Lib.Document.rev 2;
  expect_int "parse revision remains stale" stale.Lib.Document.parse_rev 1;
  expect_none "current parse accessor rejects stale parse"
    (Lib.Document.current_parse stale);
  expect_none "stale AST cleared" stale.Lib.Document.ast;
  expect_none "stale syntax cache cleared" stale.Lib.Document.syntax;
  expect_int "stale diagnostics cleared" (List.length stale.Lib.Document.diags) 0;
  let reparsed = Lib.Document.ensure_parsed stale in
  expect_int "ensure_parsed catches up parse revision"
    reparsed.Lib.Document.parse_rev reparsed.Lib.Document.rev;
  ignore (expect_some "current parse accessor accepts current parse"
    (Lib.Document.current_parse reparsed));
  ignore (expect_some "reparsed syntax" reparsed.Lib.Document.syntax);
  let skipped_diag =
    Lib.Workspace_state.diag_parse_guard ~file:None ~max_bytes:1 ~actual_bytes:2
  in
  let skipped =
    Lib.Document.make_parse_skipped ~uri ~file:None ~text:syntax_text
      ~parse_diags:[ skipped_diag ]
  in
  let ensured = Lib.Document.ensure_parsed skipped in
  expect_int "parse-skipped revision stays current"
    ensured.Lib.Document.parse_rev ensured.Lib.Document.rev;
  expect_none "parse-skipped syntax is not reparsed" ensured.Lib.Document.syntax;
  expect_int "parse-skipped diagnostic retained"
    (List.length ensured.Lib.Document.parse_diags) 1

let test_incremental_syntax_metadata () =
  let uri =
    expect_some "incremental syntax URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-incremental-suite.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM NUM U 6;";
        "  NUM = TWELVE;";
        "END";
        "TERM";
        "";
      ]
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let value_off = find_nth text ~needle:"TWELVE" ~nth:0 in
  let value_end = value_off + String.length "TWELVE" in
  let range =
    {
      T.Range.start = position_of_offset text value_off;
      end_ = position_of_offset text value_end;
    }
  in
  let change =
    T.TextDocumentContentChangeEvent.create ~range ~text:"ELEVEN" ()
  in
  let updated = Lib.Document.apply_changes_and_reparse ~changes:[ change ] doc in
  let cache =
    expect_some "updated document syntax cache" updated.Lib.Document.syntax
  in
  expect_true "token reuse attempted after ranged edit"
    cache.Lib.Syntax_cache.token_reuse.attempted;
  expect_true "token window reused a stable prefix"
    (cache.Lib.Syntax_cache.token_reuse.reused_prefix_tokens > 0);
  expect_true "token window rejoined a stable suffix"
    (cache.Lib.Syntax_cache.token_reuse.reused_suffix_tokens > 0);
  expect_none "token window avoided whole-file fallback"
    cache.Lib.Syntax_cache.token_reuse.fallback_reason;
  expect_true "checkpoint cache populated"
    (cache.Lib.Syntax_cache.checkpoint_stats.checkpoint_count > 0);
  expect_true "checkpoint resume used after token edit"
    cache.Lib.Syntax_cache.checkpoint_stats.checkpoint_reused;
  let metrics = Lib.Syntax_cache.metrics cache in
  expect_int "syntax metrics token count"
    metrics.Lib.Syntax_cache.lexed_token_count
    cache.Lib.Syntax_cache.token_reuse.new_token_count;
  expect_true "syntax metrics expose suffix reuse"
    (metrics.Lib.Syntax_cache.reused_suffix_tokens > 0);
  expect_true "syntax metrics expose checkpoint reuse"
    metrics.Lib.Syntax_cache.checkpoint_reused;
  expect_true "syntax metrics expose parse duration"
    (metrics.Lib.Syntax_cache.parse_duration_ms >= 0.0);
  expect_true "skeleton proc names populated"
    (List.exists
       (fun (name, _) -> String.uppercase_ascii name = "MAIN")
       cache.Lib.Syntax_cache.skeleton.proc_names)

let assert_shifted_suffix_reuse name text ~(start_off : int) ~(old_len : int)
    ~(replacement : string) : unit =
  let uri =
    expect_some (name ^ " URI")
      (Lib.Uri_path.docuri_of_string
         ("file:///workspace-shifted-" ^ name ^ ".j73"))
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let range =
    {
      T.Range.start = position_of_offset text start_off;
      end_ = position_of_offset text (start_off + old_len);
    }
  in
  let change =
    T.TextDocumentContentChangeEvent.create ~range ~text:replacement ()
  in
  let updated =
    Lib.Document.apply_changes_and_reparse ~changes:[ change ] doc
  in
  let cache =
    expect_some (name ^ " syntax cache") updated.Lib.Document.syntax
  in
  expect_true (name ^ " attempted token reuse")
    cache.Lib.Syntax_cache.token_reuse.attempted;
  expect_true (name ^ " rejoined shifted suffix")
    (cache.Lib.Syntax_cache.token_reuse.reused_suffix_tokens > 0);
  expect_none (name ^ " no token fallback")
    cache.Lib.Syntax_cache.token_reuse.fallback_reason

let test_shifted_suffix_token_reuse () =
  let proc_text item_line =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        item_line;
        "  NUM = 12;";
        "END";
        "TERM";
        "";
      ]
  in
  let insert_body = proc_text "  ITEM NUM U 6;" in
  let insert_off =
    find_nth insert_body ~needle:"NUM = 12" ~nth:0 + String.length "NUM"
  in
  assert_shifted_suffix_reuse "body-insert" insert_body ~start_off:insert_off
    ~old_len:0 ~replacement:"BER";
  let delete_body = proc_text "  ITEM NUMBER U 6;" in
  let delete_off = find_nth delete_body ~needle:"NUMBER" ~nth:0 + 3 in
  assert_shifted_suffix_reuse "body-delete" delete_body ~start_off:delete_off
    ~old_len:3 ~replacement:"";
  let before_decl = proc_text "  ITEM NUM U 6;" in
  let before_decl_off = find_nth before_decl ~needle:"ITEM NUM" ~nth:0 in
  assert_shifted_suffix_reuse "before-decl-insert" before_decl
    ~start_off:before_decl_off ~old_len:0 ~replacement:"STATIC ";
  let decl_with_static = proc_text "  STATIC ITEM NUM U 6;" in
  let static_off = find_nth decl_with_static ~needle:"STATIC " ~nth:0 in
  assert_shifted_suffix_reuse "before-decl-delete" decl_with_static
    ~start_off:static_off ~old_len:(String.length "STATIC ") ~replacement:"";
  let comment_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  % comment %";
        "  ITEM NUM U 6;";
        "END";
        "TERM";
        "";
      ]
  in
  let comment_off =
    find_nth comment_text ~needle:"comment" ~nth:0 + String.length "com"
  in
  assert_shifted_suffix_reuse "comment-insert" comment_text
    ~start_off:comment_off ~old_len:0 ~replacement:"x"

let test_checkpoint_reuse_after_failed_parse () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM NUM U 6;";
        "  ITEM OTHER U 6;";
        "  NUM = ;";
        "";
      ]
  in
  let uri =
    expect_some "failed-checkpoint URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-failed-checkpoint.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let previous =
    expect_some "failed parse syntax cache" doc.Lib.Document.syntax
  in
  expect_none "broken parse has no full AST"
    previous.Lib.Syntax_cache.parse.ast;
  expect_true "broken parse recorded checkpoints"
    (previous.Lib.Syntax_cache.checkpoint_stats.checkpoint_count > 0);
  let edit_off =
    find_nth text ~needle:"OTHER" ~nth:0 + String.length "OTH"
  in
  let range =
    {
      T.Range.start = position_of_offset text edit_off;
      end_ = position_of_offset text edit_off;
    }
  in
  let change = T.TextDocumentContentChangeEvent.create ~range ~text:"ER" () in
  let updated =
    Lib.Document.apply_changes_and_reparse ~changes:[ change ] doc
  in
  let cache =
    expect_some "updated failed parse syntax cache" updated.Lib.Document.syntax
  in
  expect_true "checkpoint reused after failed parse"
    cache.Lib.Syntax_cache.checkpoint_stats.checkpoint_reused;
  expect_none "failed parse checkpoint reuse did not fall back"
    cache.Lib.Syntax_cache.checkpoint_stats.fallback_reason;
  expect_true "diagnostics retained or recomputed after failed reuse"
    (List.length cache.Lib.Syntax_cache.parse.diags > 0)

let has_skeleton_symbol cache name kind =
  cache.Lib.Syntax_cache.skeleton.symbols
  |> List.exists (fun (symbol : Lib.Syntax_cache.skeleton_symbol) ->
         normalize_name symbol.sk_name = normalize_name name
         && symbol.sk_kind = kind)

let name_of_symbol_response = function
  | `DocumentSymbol symbol -> (
      match T.DocumentSymbol.yojson_of_t symbol with
      | `Assoc fields -> (
          match List.assoc_opt "name" fields with
          | Some (`String name) -> Some name
          | _ -> None)
      | _ -> None)
  | `SymbolInformation symbol -> (
      match T.SymbolInformation.yojson_of_t symbol with
      | `Assoc fields -> (
          match List.assoc_opt "name" fields with
          | Some (`String name) -> Some name
          | _ -> None)
      | _ -> None)

let test_token_skeleton_symbols_for_broken_file () =
  let text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEFINE MAC \"1\";";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM NUM U 6;";
        "  TABLE TAB (1) U 6;";
        "  TYPE REC TABLE U 1;";
        "  BLOCK BLK;";
        "  BEGIN";
        "    ITEM INSIDE U 1;";
        "  END";
        "LBL: NUM = ;";
        "";
      ]
  in
  let uri =
    expect_some "skeleton broken URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-skeleton-broken.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let cache =
    expect_some "broken skeleton syntax cache" doc.Lib.Document.syntax
  in
  expect_none "skeleton fixture has no full AST" cache.Lib.Syntax_cache.parse.ast;
  expect_true "skeleton captures COMPOOL"
    (has_skeleton_symbol cache "TARGET" Lib.Syntax_cache.SkCompool);
  expect_true "skeleton captures DEFINE"
    (has_skeleton_symbol cache "MAC" Lib.Syntax_cache.SkDefineMacro);
  expect_true "skeleton captures PROC"
    (has_skeleton_symbol cache "MAIN" Lib.Syntax_cache.SkProcedure);
  expect_true "skeleton captures ITEM"
    (has_skeleton_symbol cache "NUM" Lib.Syntax_cache.SkItem);
  expect_true "skeleton captures TABLE"
    (has_skeleton_symbol cache "TAB" Lib.Syntax_cache.SkTable);
  expect_true "skeleton captures TYPE"
    (has_skeleton_symbol cache "REC" Lib.Syntax_cache.SkType);
  expect_true "skeleton captures BLOCK"
    (has_skeleton_symbol cache "BLK" Lib.Syntax_cache.SkBlock);
  expect_true "skeleton captures LABEL"
    (has_skeleton_symbol cache "LBL" Lib.Syntax_cache.SkLabel);
  let ws = Lib.Workspace.create () in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let names =
    Lib.Workspace.document_symbols_for ws ~uri
    |> List.filter_map name_of_symbol_response
    |> List.map normalize_name
  in
  List.iter
    (fun name ->
      expect_true ("document symbols include skeleton " ^ name)
        (List.mem (normalize_name name) names))
    [ "MAIN"; "NUM"; "TAB"; "REC"; "BLK"; "MAC"; "LBL" ]

let test_quick_nav_uses_token_skeleton () =
  let root = mk_temp_dir "jovial-quick-nav-skeleton" in
  let path = Filename.concat root "BROKEN.j73" in
  let text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEFINE MAC \"1\";";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM NUM U 6;";
        "  TABLE TAB (1) U 6;";
        "  TYPE REC TABLE U 1;";
        "LBL: NUM = ;";
        "";
      ]
  in
  write_text path text;
  let entries =
    Lib.Workspace_background.quick_nav_entries_of_path_prefix path
      ~max_bytes:(String.length text)
  in
  let names =
    entries
    |> List.map (fun (entry : Lib.Workspace_foundation.quick_nav_entry) ->
           normalize_name entry.qn_name)
  in
  List.iter
    (fun name ->
      expect_true ("quick nav skeleton includes " ^ name)
        (List.mem (normalize_name name) names))
    [ "TARGET"; "MAC"; "MAIN"; "NUM"; "TAB"; "REC"; "LBL" ]

let token_names text =
  Lib.Preprocess.lex_all_tokens ~file:None ~text
  |> Array.to_list
  |> List.map (fun (span : Lib.Preprocess.lex_tok) ->
         Lib.Parser.Debug.string_of_token span.Lib.Parser.tok)

let test_lexer_regressions () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEFINE FOO \"A\" \"comment\";";
        "COMPOOL \"UTIL\";";
        "ITEM NUMBER'1' U 6;";
        "ITEM MASK U 1B'1010';";
        "X = (* U 6 *) 1;";
        "% ignored";
        "comment %";
        "TERM";
        "";
      ]
  in
  let names = token_names text in
  let string_count =
    List.fold_left
      (fun acc name -> if name = "STRINGLIT" then acc + 1 else acc)
      0 names
  in
  expect_int "DEFINE and COMPOOL first strings retained" string_count 2;
  expect_true "conversion left token retained" (List.mem "(*" names);
  expect_true "conversion right token retained" (List.mem "*)" names);
  expect_true "based integer retained" (List.mem "INTLIT" names);
  expect_true "quoted identifier retained" (List.mem "ID" names);
  let lexbuf = Lexing.from_string "DEFINE BAD \"unterminated" in
  Lib.Lexer.with_session_state (fun lexer ->
      match Lib.Lexer.token lexer lexbuf with
      | Lib.Parser.DEFINE -> (
          match Lib.Lexer.token lexer lexbuf with
          | Lib.Parser.ID _ -> (
              try
                ignore (Lib.Lexer.token lexer lexbuf);
                failf "unterminated DEFINE string did not raise"
              with Lib.Lexer.Lex_error _ -> ())
          | _ -> failf "expected ID after DEFINE")
      | _ -> failf "expected DEFINE token")

let diagnostic_texts (diags : T.Diagnostic.t list) : string list =
  List.map
    (fun (d : T.Diagnostic.t) ->
      match d.message with
      | `String s -> s
      | `MarkupContent mc -> mc.value)
    diags

let diagnostics_contain (diags : T.Diagnostic.t list) ~(needle : string) : bool
    =
  List.exists (string_contains ~needle) (diagnostic_texts diags)

let expect_diagnostic_containing label ~(needle : string)
    (diags : T.Diagnostic.t list) : unit =
  if not (diagnostics_contain diags ~needle) then
    failf "%s: expected diagnostic containing %S, got [%s]" label needle
      (String.concat "; " (diagnostic_texts diags))

let expect_no_diagnostic_containing label ~(needle : string)
    (diags : T.Diagnostic.t list) : unit =
  if diagnostics_contain diags ~needle then
    failf "%s: unexpected diagnostic containing %S in [%s]" label needle
      (String.concat "; " (diagnostic_texts diags))

let test_macro_expansion_source_mapping () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEFINE FOO \"BAR\";";
        "% FOO in comment %";
        "ITEM VALUE U 6;";
        "VALUE = 'FOO';";
        "VALUE = FOOD;";
        "VALUE = FOO;";
        "TERM";
        "";
      ]
  in
  let pre = Lib.Preprocess.run ~file:None ~text in
  expect_true "macro use expands outside protected spans"
    (string_contains ~needle:"VALUE = BAR;" pre.text);
  expect_true "macro does not expand inside percent comments"
    (string_contains ~needle:"% FOO in comment %" pre.text);
  expect_true "macro does not expand inside string literals"
    (string_contains ~needle:"'FOO'" pre.text);
  expect_true "macro does not expand inside longer identifiers"
    (string_contains ~needle:"VALUE = FOOD;" pre.text);
  expect_true "macro expansion records source-map segment"
    (List.exists
       (fun (seg : Lib.Preprocess.expansion_segment) ->
         match seg.origin with
         | Lib.Preprocess.MacroExpansion { macro_name; call_site; _ } ->
             normalize_name macro_name = "FOO"
             && call_site.Lib.Ast.Loc.start_pos.line = 7
         | _ -> false)
       pre.source_map);
  let arity_text =
    String.concat "\n"
      [
        "START";
        "DEFINE ADD(A,B) \"$A+$B\";";
        "ITEM VALUE U 6;";
        "VALUE = ADD(1);";
        "TERM";
        "";
      ]
  in
  let arity = Lib.Preprocess.run ~file:None ~text:arity_text in
  expect_true "macro arity mismatch is diagnosed"
    (List.exists
       (string_contains ~needle:"expects 2 argument")
       (diagnostic_texts arity.diags));
  let recursive_text =
    String.concat "\n"
      [
        "START";
        "DEFINE A \"B\";";
        "DEFINE B \"A\";";
        "ITEM VALUE U 6;";
        "VALUE = A;";
        "TERM";
        "";
      ]
  in
  let recursive = Lib.Preprocess.run ~file:None ~text:recursive_text in
  expect_true "recursive macro expansion is bounded and diagnosed"
    (List.exists
       (fun msg ->
         string_contains ~needle:"possible recursive macro" msg
         || string_contains ~needle:"exceeded" msg)
       (diagnostic_texts recursive.diags))

let test_parser_profiles () =
  let text =
    String.concat "\n"
      [ "START"; "DEF PROC MAIN RENT;"; "BEGIN"; "  X = ;"; "END"; "TERM"; "" ]
  in
  let toks = Lib.Preprocess.lex_all_tokens ~file:None ~text in
  let interactive =
    Lib.Parser.parse_tokens ~file:None ~dump_ast:false
      ~profile:Lib.Parser.Interactive ~tokens:toks
  in
  let background =
    Lib.Parser.parse_tokens ~file:None ~dump_ast:false
      ~profile:Lib.Parser.Background ~tokens:toks
  in
  let messages out =
    List.map
      (fun (d : T.Diagnostic.t) ->
        match d.message with
        | `String s -> s
        | `MarkupContent mc -> mc.value)
      out.Lib.Parser.diags
  in
  expect_true "interactive expected-token hint"
    (List.exists
       (fun msg ->
         String.starts_with ~prefix:"Parse error" msg
         && string_contains ~needle:"Expected:" msg)
       (messages interactive));
  expect_true "background suppresses expected-token hint"
    (not
       (List.exists (string_contains ~needle:"Expected:") (messages background)));
  expect_true "background diagnostics capped"
    (List.length background.Lib.Parser.diags <= List.length interactive.diags)

let test_cache_shedding_and_cst_bounds () =
  let uri =
    expect_some "CST URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-cst-suite.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text:syntax_text in
  let dropped = Lib.Document.drop_ast doc in
  (match dropped.Lib.Document.syntax with
  | Some syntax ->
      expect_true "drop_ast drops raw tokens" (syntax.raw_tokens = None);
      expect_true "drop_ast drops expanded tokens" (syntax.expanded_tokens = None)
  | None -> failf "expected syntax cache after drop_ast");
  let ws = Lib.Workspace.create () in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:syntax_text;
  let cst = expect_some "CST dump" (Lib.Workspace.cst_dump_for ws ~uri) in
  expect_true "CST header retained" (String.starts_with ~prefix:"CST" cst)

let test_semantic_tokens_delta () =
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "semantic delta URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-semantic-delta.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:syntax_text;
  let full = expect_some "semantic tokens full" (Lib.Workspace.semantic_tokens_full_for ws ~uri) in
  let previous_result_id =
    match full.T.SemanticTokens.resultId with
    | Some id -> id
    | None -> failf "semantic tokens full response missing resultId"
  in
  (match
     Lib.Workspace.semantic_tokens_delta_for ws ~uri
       ~previous_result_id
   with
  | Some (`SemanticTokensDelta delta) -> (
      match delta.T.SemanticTokensDelta.edits with
      | [] ->
          expect_true "semantic delta resultId retained"
            (delta.resultId = Some previous_result_id)
      | edits ->
          failf "semantic delta expected no edits, got %d" (List.length edits))
  | Some (`SemanticTokens _) ->
      failf "semantic delta fell back despite cached previous resultId"
  | None -> failf "semantic delta returned None");
  (match
     Lib.Workspace.semantic_tokens_delta_for ws ~uri
       ~previous_result_id:"missing-result-id"
   with
  | Some (`SemanticTokens _) -> ()
  | Some (`SemanticTokensDelta _) ->
      failf "semantic delta should fall back on stale previousResultId"
  | None -> failf "semantic delta stale fallback returned None")

let large_deferred_text () =
  let b = Buffer.create (280 * 1024) in
  Buffer.add_string b "START\n";
  Buffer.add_string b "DEF PROC MAIN RENT;\n";
  Buffer.add_string b "BEGIN\n";
  Buffer.add_string b "  ITEM TARGETTOKEN U 6;\n";
  Buffer.add_string b "  TARGETTOKEN = 1;\n";
  let i = ref 0 in
  while Buffer.length b < (270 * 1024) do
    Buffer.add_string b "  ITEM FILL";
    Buffer.add_string b (string_of_int !i);
    Buffer.add_string b " U 6;\n";
    incr i
  done;
  Buffer.add_string b "END\nTERM\n";
  Buffer.contents b

let open_deferred_large_doc name text =
  let root = mk_temp_dir name in
  let path = Filename.concat root "LARGE.j73" in
  write_text path text;
  let uri = expect_some (name ^ " URI") (Lib.Uri_path.docuri_of_path path) in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_doc_lifecycle.open_doc ws ~inline_catch_up:false ~uri
    ~file:(Some path) ~text;
  let doc =
    expect_some (name ^ " open doc")
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs uri)
  in
  expect_int (name ^ " starts with stale parse") doc.Lib.Document.parse_rev 0;
  (ws, uri, path)

let test_specific_open_doc_catchup_handles_multiple_pending () =
  let root = mk_temp_dir "jovial-specific-open-catchup" in
  let path_a = Filename.concat root "A.j73" in
  let path_b = Filename.concat root "B.j73" in
  write_text path_a syntax_text;
  write_text path_b syntax_text;
  let uri_a = expect_some "catchup A URI" (Lib.Uri_path.docuri_of_path path_a) in
  let uri_b = expect_some "catchup B URI" (Lib.Uri_path.docuri_of_path path_b) in
  let ws = Lib.Workspace.create () in
  Lib.Workspace.open_doc ~force_provisional:true ~inline_catch_up:false ws
    ~uri:uri_a ~file:(Some path_a) ~text:syntax_text;
  Lib.Workspace.open_doc ~force_provisional:true ~inline_catch_up:false ws
    ~uri:uri_b ~file:(Some path_b) ~text:syntax_text;
  expect_false "catchup A starts provisional"
    (Lib.Workspace.open_doc_converged ws ~uri:uri_a);
  expect_false "catchup B starts provisional"
    (Lib.Workspace.open_doc_converged ws ~uri:uri_b);
  expect_true "specific open-doc catchup runs"
    (Lib.Workspace.finish_open_doc_now_if_needed ws ~uri:uri_a);
  expect_true "catchup A converged"
    (Lib.Workspace.open_doc_converged ws ~uri:uri_a);
  expect_false "catchup B remains pending"
    (Lib.Workspace.open_doc_converged ws ~uri:uri_b);
  expect_false "specific open-doc catchup is idempotent"
    (Lib.Workspace.finish_open_doc_now_if_needed ws ~uri:uri_a)

let hover_markdown_text (hover : T.Hover.t) : string =
  match T.Hover.yojson_of_t hover with
  | `Assoc fields -> (
      match List.assoc_opt "contents" fields with
      | Some (`Assoc cfields) -> (
          match List.assoc_opt "value" cfields with
          | Some (`String value) -> value
          | _ -> "")
      | _ -> "")
  | _ -> ""

let test_hover_large_stale_does_not_sync_parse () =
  let text = large_deferred_text () in
  let ws, uri, _path = open_deferred_large_doc "jovial-hover-large" text in
  let use_off = find_nth text ~needle:"TARGETTOKEN = 1" ~nth:0 in
  let pos = position_of_offset text (use_off + 1) in
  Lib.Perf_log.reset ();
  let hover =
    expect_some "large stale hover fallback"
      (Lib.Workspace_navigation.hover_for ws ~uri ~pos)
  in
  let body = hover_markdown_text hover in
  expect_true "hover reports pending semantic analysis"
    (string_contains ~needle:"semantic analysis pending" body);
  expect_no_hover_metadata "large stale hover fallback" body;
  let doc =
    expect_some "hover doc still open"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs uri)
  in
  expect_int "hover did not update parse rev" doc.Lib.Document.parse_rev 0;
  expect_int "hover sync parse counter remains zero"
    (Lib.Perf_log.counter_value "sync_full_parse_from_hover")
    0;
  expect_int "hover nav rebuild counter remains zero"
    (Lib.Perf_log.counter_value "sync_workspace_nav_rebuild_from_hover")
    0

let test_semantic_range_large_stale_does_not_sync_parse () =
  let text = large_deferred_text () in
  let ws, uri, _path =
    open_deferred_large_doc "jovial-semantic-range-large" text
  in
  let token_range = range_of_substring text ~needle:"TARGETTOKEN = 1" ~nth:0 in
  Lib.Perf_log.reset ();
  let tokens =
    expect_some "large stale semantic range"
      (Lib.Workspace_reporting.semantic_tokens_range_for ws ~uri
         ~range:token_range)
  in
  expect_true "semantic range returns lexical tokens"
    (Array.length tokens.T.SemanticTokens.data > 0);
  let doc =
    expect_some "semantic range doc still open"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs uri)
  in
  expect_int "semantic range did not update parse rev" doc.Lib.Document.parse_rev
    0;
  expect_int "semantic range sync parse counter remains zero"
    (Lib.Perf_log.counter_value
       "sync_full_parse_from_semantic_tokens_range")
    0

let test_open_doc_dequeue_preempts_unopened_high () =
  let root = mk_temp_dir "jovial-open-first" in
  let unopened_path = Filename.concat root "TARGET.j73" in
  let open_path = Filename.concat root "OPEN.j73" in
  write_text unopened_path target_text;
  write_text open_path syntax_text;
  let ws = Lib.Workspace_state.create () in
  ignore (Lib.Workspace_state.set_source_files ws [ unopened_path; open_path ]);
  Lib.Workspace_state.enqueue_bg_path ws
    ~lane:Lib.Workspace_foundation.LaneOpen ~reason_group:"test_unopened"
    ~high:true unopened_path;
  let uri =
    expect_some "open-first URI" (Lib.Uri_path.docuri_of_path open_path)
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~force_provisional:true
    ~inline_catch_up:false ~uri ~file:(Some open_path) ~text:syntax_text;
  expect_true "open parse is pending"
    (Lib.Workspace_state.has_pending_open_parse_work ws);
  match
    Lib.Workspace_state.dequeue_bg_path ws
      ~mode:Lib.Workspace_foundation.BgTickInteractive
      ~allow_normal_large:true ~allow_root_large:true ~open_only:true
      ~prefer_open:true
  with
  | Some (path, _) ->
      expect_true "pending open path wins over unopened high-priority work"
        (normalize_path path = normalize_path open_path)
  | None -> failf "open-only dequeue returned no pending open document"

let test_parse_worker_stale_open_job_detected () =
  let root = mk_temp_dir "jovial-stale-open-job" in
  let path = Filename.concat root "OPEN.j73" in
  write_text path syntax_text;
  let uri = expect_some "stale-open URI" (Lib.Uri_path.docuri_of_path path) in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ws
    ~force_provisional:true ~inline_catch_up:false ~uri ~file:(Some path)
    ~text:syntax_text;
  let doc =
    expect_some "open doc before change"
      (Lib.Workspace_state.find_open_doc_for_path ws ~path)
  in
  let path_key = normalize_path path in
  let uri_key = Lib.Uri_path.docuri_to_string uri in
  let generation =
    let open Lib.Workspace_foundation in
    expect_some "open parse generation"
      (Hashtbl.find_opt ws.open_parse_generation uri_key)
  in
  let job =
    let open Lib.Workspace_foundation in
    {
      pj_kind = ParseJobHighLarge;
      pj_epoch = ws.parse_epoch;
      pj_payload = ParseJobOpen { path_key; uri; generation; doc };
    }
  in
  let change =
    T.TextDocumentContentChangeEvent.create
      ~text:(syntax_text ^ "! typed after queued parse\n")
      ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri
    ~changes:[ change ];
  expect_true "newer edit makes queued open parse stale"
    (Lib.Workspace_background.parse_job_is_stale ws job)

let test_parse_worker_path_result_does_not_replace_open_doc () =
  let root = mk_temp_dir "jovial-path-result-open" in
  let path = Filename.concat root "OPEN.j73" in
  write_text path syntax_text;
  let uri = expect_some "path-result URI" (Lib.Uri_path.docuri_of_path path) in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ws
    ~force_provisional:true ~inline_catch_up:false ~uri ~file:(Some path)
    ~text:syntax_text;
  let path_key = normalize_path path in
  let stale_doc =
    Lib.Document.make_versioned ~lsp_version:(Some 0) ~uri ~file:(Some path)
      ~text:target_text
  in
  let pr_epoch =
    let open Lib.Workspace_foundation in
    ws.parse_epoch
  in
  Lib.Workspace_background.apply_parse_result_path ws
    ~pr_kind:Lib.Workspace_foundation.ParseJobNormalLarge ~pr_epoch ~path_key
    ~doc_opt:(Some stale_doc);
  let file_doc =
    let open Lib.Workspace_foundation in
    expect_some "file map keeps open doc" (Hashtbl.find_opt ws.files path_key)
  in
  expect_true "stale path result did not replace open text"
    (file_doc.Lib.Document.text = syntax_text)

let has_capability (name : string) (json : Yojson.Safe.t) : bool =
  match json with
  | `Assoc root -> (
      match List.assoc_opt "capabilities" root with
      | Some (`Assoc caps) -> List.mem_assoc name caps
      | _ -> false)
  | _ -> false

let params_assoc name (json : Yojson.Safe.t) : (string * Yojson.Safe.t) list =
  match json with
  | `Assoc root -> (
      match List.assoc_opt "params" root with
      | Some (`Assoc params) -> params
      | _ -> failf "%s: missing params object" name)
  | _ -> failf "%s: expected JSON object" name

let expect_json_version name json want =
  let params = params_assoc name json in
  match (List.assoc_opt "version" params, want) with
  | Some (`Int got), Some want -> expect_int name got want
  | None, None -> ()
  | Some got, None ->
      failf "%s: expected no version, got %s" name (Yojson.Safe.to_string got)
  | got, Some want ->
      failf "%s: expected version %d, got %s" name want
        (match got with None -> "<none>" | Some j -> Yojson.Safe.to_string j)

let expect_json_empty_diagnostics name json =
  let params = params_assoc name json in
  match List.assoc_opt "diagnostics" params with
  | Some (`List []) -> ()
  | Some got ->
      failf "%s: expected empty diagnostics, got %s" name
        (Yojson.Safe.to_string got)
  | None -> failf "%s: missing diagnostics" name

let test_lsp_capabilities_diagnostic_pull () =
  let settings = Lib.Workspace_settings.from_env () in
  let flags =
    { settings.Lib.Workspace_settings.feature_flags with diagnostics = true }
  in
  let without_pull =
    Lib.Lsp_response.initialize_result_json ~feature_flags:flags
      ~diagnostic_pull:false
  in
  let with_pull =
    Lib.Lsp_response.initialize_result_json ~feature_flags:flags
      ~diagnostic_pull:true
  in
  expect_false "diagnostic pull omitted without client support"
    (has_capability "diagnosticProvider" without_pull);
  expect_false "diagnostic pull not advertised by default"
    (has_capability "diagnosticProvider" with_pull)

let test_versioned_diagnostic_publish () =
  let uri =
    expect_some "diagnostic URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-diagnostics.j73")
  in
  let path = Filename.temp_file "jovial-diags" ".lsp" in
  let oc = open_out_bin path in
  let published = Hashtbl.create 4 in
  expect_false "stale diagnostic snapshot is suppressed"
    (Lib.Lsp_response.publish_diagnostics_if_current published oc ~uri
       ~computed_version:(Some 1) ~current_version:(Some 2) ~diags:[]);
  expect_true "current diagnostic snapshot is published"
    (Lib.Lsp_response.publish_diagnostics_if_current published oc ~uri
       ~computed_version:(Some 2) ~current_version:(Some 2) ~diags:[]);
  expect_false "same version and diagnostics are deduplicated"
    (Lib.Lsp_response.publish_diagnostics_if_changed published oc ~uri
       ~version:(Some 2) ~diags:[]);
  expect_true "empty diagnostics republish for a newer version"
    (Lib.Lsp_response.publish_diagnostics_if_changed published oc ~uri
       ~version:(Some 3) ~diags:[]);
  close_out oc;
  let messages = parse_lsp_messages (read_text path) in
  expect_int "versioned diagnostic publish count" (List.length messages) 2;
  (match messages with
  | [ first; second ] ->
      expect_json_version "first diagnostic version" first (Some 2);
      expect_json_empty_diagnostics "first diagnostic payload" first;
      expect_json_version "second diagnostic version" second (Some 3);
      expect_json_empty_diagnostics "second diagnostic payload" second
  | _ -> failf "unexpected diagnostic message count")

let test_unversioned_closed_diagnostic_publish () =
  let uri =
    expect_some "closed diagnostic publish URI"
      (Lib.Uri_path.docuri_of_string "file:///closed-publish.j73")
  in
  let path = Filename.concat (mk_temp_dir "jovial-closed-publish") "out.jsonl" in
  let oc = open_out_bin path in
  let published = Hashtbl.create 4 in
  expect_true "closed diagnostics publish without version"
    (Lib.Lsp_response.publish_diagnostics_if_changed published oc ~uri
       ~version:None ~diags:[]);
  close_out oc;
  match parse_lsp_messages (read_text path) with
  | [ msg ] ->
      expect_json_version "closed diagnostic publish version" msg None;
      expect_json_empty_diagnostics "closed diagnostic publish payload" msg
  | msgs ->
      failf "unexpected closed diagnostic publish message count %d"
        (List.length msgs)

let open_workspace_text ws root name text =
  let path = Filename.concat root name in
  write_text path text;
  let uri = expect_some (name ^ " URI") (Lib.Uri_path.docuri_of_path path) in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:(Some path) ~text;
  (uri, path)

let revalidated_diagnostics ws uri =
  ignore (Lib.Workspace_doc_lifecycle.revalidate_all ws);
  Lib.Workspace_state.diagnostics_for ws ~uri

let test_closed_document_diagnostics_survive_close () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "closed diagnostic URI"
      (Lib.Uri_path.docuri_of_string "file:///closed-diagnostics.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  MISSING = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let before = Lib.Workspace_state.diagnostics_for ws ~uri in
  expect_true "diagnostics before close" (before <> []);
  Lib.Workspace_doc_lifecycle.close_doc ws ~uri;
  let after = Lib.Workspace_state.diagnostics_for ws ~uri in
  expect_true "diagnostics after close retained" (after <> [])

let test_closed_document_diagnostics_refresh_after_file_change () =
  let root = mk_temp_dir "jovial-closed-diag-refresh" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let initial_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  MISSING_OLD = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let uri, path = open_workspace_text ws root "CLOSED.j73" initial_text in
  let before = Lib.Workspace_state.diagnostics_for ws ~uri in
  expect_diagnostic_containing "initial closed-file diagnostic"
    ~needle:"MISSING_OLD" before;
  Lib.Workspace_doc_lifecycle.close_doc ws ~uri;
  let updated_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  MISSING_NEW = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  write_text path updated_text;
  Lib.Workspace_doc_lifecycle.apply_watched_file_changes ws
    ~changes:[ (path, `Changed) ];
  expect_true "closed file diagnostic request refreshes from disk"
    (Lib.Workspace_background.refresh_closed_doc_diagnostics_now ws ~uri);
  let after = Lib.Workspace_state.diagnostics_for ws ~uri in
  expect_diagnostic_containing "updated closed-file diagnostic"
    ~needle:"MISSING_NEW" after;
  expect_no_diagnostic_containing "stale closed-file diagnostic removed"
    ~needle:"MISSING_OLD" after

let test_selected_imported_item_helper_type_no_false_positive () =
  let root = mk_temp_dir "jovial-selected-import-type" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text =
    String.concat "\n"
      [
        "START COMPOOL DATA;";
        "TYPE COUNTER U 10;";
        "DEF ITEM CLOCK COUNTER;";
        "TERM";
        "";
      ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('DATA') CLOCK;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  CLOCK = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  ignore (open_workspace_text ws root "DATA.j73" compool_text);
  let importer_uri, _ = open_workspace_text ws root "MAIN.j73" importer_text in
  let diags = revalidated_diagnostics ws importer_uri in
  expect_no_diagnostic_containing
    "selected imported helper type false positive removed"
    ~needle:"requires explicit import of type" diags;
  expect_no_diagnostic_containing "selected imported item visible"
    ~needle:"Undefined item \"CLOCK\"" diags;
  expect_no_diagnostic_containing "selected imported identifier visible"
    ~needle:"Undefined identifier \"CLOCK\"" diags

let test_direct_missing_custom_type_still_hints () =
  let root = mk_temp_dir "jovial-missing-type-hint" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text =
    String.concat "\n"
      [ "START COMPOOL DATA;"; "TYPE COUNTER U 10;"; "TERM"; "" ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM LOCAL COUNTER;";
        "END";
        "TERM";
        "";
      ]
  in
  ignore (open_workspace_text ws root "DATA.j73" compool_text);
  ignore (Lib.Workspace_semantics.symbol_hint_index ws);
  let importer_uri, _ = open_workspace_text ws root "MAIN.j73" importer_text in
  ignore (Lib.Workspace_semantics.symbol_hint_index ws);
  let diags = revalidated_diagnostics ws importer_uri in
  expect_diagnostic_containing "direct missing custom type still hints"
    ~needle:"COUNTER" diags

let test_ref_proc_visibility_suppresses_undefined () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "ref proc URI"
      (Lib.Uri_path.docuri_of_string "file:///ref-proc-visible.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "REF PROC HELPER RENT;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "REF PROC makes helper callable"
    ~needle:"Undefined procedure \"HELPER\"" diags

let test_compool_helper_proc_visibility_suppresses_undefined () =
  let root = mk_temp_dir "jovial-compool-helper-proc" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text =
    String.concat "\n"
      [ "START COMPOOL DATA;"; "REF PROC HELPER RENT;"; "TERM"; "" ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('DATA');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  ignore (open_workspace_text ws root "DATA.j73" compool_text);
  let importer_uri, _ = open_workspace_text ws root "MAIN.j73" importer_text in
  let diags = revalidated_diagnostics ws importer_uri in
  expect_no_diagnostic_containing "COMPOOL helper proc visible"
    ~needle:"Undefined procedure \"HELPER\"" diags

let test_imported_ref_item_and_proc_visibility_suppresses_undefined () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "ref item/proc URI"
      (Lib.Uri_path.docuri_of_string "file:///ref-item-proc-visible.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "REF ITEM CLOCK U 10;";
        "REF PROC HELPER RENT;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  CLOCK = 1;";
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "REF ITEM makes imported item visible"
    ~needle:"Undefined item \"CLOCK\"" diags;
  expect_no_diagnostic_containing "REF PROC makes imported proc visible"
    ~needle:"Undefined procedure \"HELPER\"" diags

let test_hover_nav_fixture_import_diagnostics_are_not_undefined () =
  let root = mk_temp_dir "jovial-hover-nav-diags" in
  let copy_fixture name =
    let src = jovial_hover_nav_fixture name in
    let text = read_text src in
    let dst = Filename.concat root name in
    write_text dst text;
    (dst, text)
  in
  let compool_path, compool_text = copy_fixture "compool_data.j73" in
  let find_path, find_text = copy_fixture "find_impl.j73" in
  let main_path, main_text = copy_fixture "main.j73" in
  let types_path, types_text = copy_fixture "types.j73" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore
    (Lib.Workspace_state.set_source_files ws
       [ compool_path; find_path; main_path; types_path ]);
  ignore (open_workspace_text ws root "compool_data.j73" compool_text);
  ignore (open_workspace_text ws root "find_impl.j73" find_text);
  ignore (open_workspace_text ws root "types.j73" types_text);
  let main_uri, _ = open_workspace_text ws root "main.j73" main_text in
  let diags = revalidated_diagnostics ws main_uri in
  List.iter
    (fun name ->
      expect_no_diagnostic_containing
        ("imported fixture symbol is not undefined: " ^ name)
        ~needle:("Undefined identifier \"" ^ name ^ "\"") diags;
      expect_no_diagnostic_containing
        ("imported fixture item is not undefined: " ^ name)
        ~needle:("Undefined item \"" ^ name ^ "\"") diags;
      expect_no_diagnostic_containing
        ("imported fixture procedure is not undefined: " ^ name)
        ~needle:("Undefined procedure \"" ^ name ^ "\"") diags)
    [ "CLOCK"; "LIMIT"; "FIND"; "PRIVILEGE"; "COUNTER" ]

let test_symbol_hints_include_skeleton_imported_helpers () =
  let root = mk_temp_dir "jovial-skeleton-import-hints" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text =
    String.concat "\n"
      [
        "START COMPOOL DATA;";
        "REF ITEM CLOCK U 10;";
        "REF PROC HELPER RENT;";
        "BEGIN";
        "";
      ]
  in
  ignore (open_workspace_text ws root "DATA.j73" compool_text);
  let values, _types = Lib.Workspace_semantics.symbol_hint_index ws in
  let has_hint symbol =
    match Hashtbl.find_opt values symbol with
    | None -> false
    | Some compools -> List.exists (( = ) "DATA") compools
  in
  expect_true "skeleton hints include imported item" (has_hint "CLOCK");
  expect_true "skeleton hints include imported proc" (has_hint "HELPER")

let test_function_return_expression_type_mismatch () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "function return mismatch URI"
      (Lib.Uri_path.docuri_of_string "file:///function-return-mismatch.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "PROC BAD() U 10;";
        "BEGIN";
        "  ITEM TEXT C 4;";
        "  RETURN TEXT;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "RETURN expression checked against function type"
    ~needle:
      "Function \"BAD\" result type mismatch: expected integer, provided \
       character"
    diags

let test_function_result_assignment_type_mismatch () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "function result assignment mismatch URI"
      (Lib.Uri_path.docuri_of_string
         "file:///function-result-assignment-mismatch.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "PROC BAD() U 10;";
        "BEGIN";
        "  ITEM TEXT C 4;";
        "  BAD = TEXT;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing
    "function-name assignment checked against function type"
    ~needle:
      "Function \"BAD\" result type mismatch: expected integer, provided \
       character"
    diags

let test_function_return_matching_type_has_no_mismatch () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "function return match URI"
      (Lib.Uri_path.docuri_of_string "file:///function-return-match.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "PROC GOOD() U 10;";
        "BEGIN";
        "  ITEM VALUE U 10;";
        "  RETURN VALUE;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "matching function return has no mismatch"
    ~needle:"Function \"GOOD\" result type mismatch" diags

let test_procedure_call_used_as_function_value_errors () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "procedure as value URI"
      (Lib.Uri_path.docuri_of_string "file:///procedure-as-value.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "PROC HELPER();";
        "BEGIN";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 10;";
        "  VALUE = HELPER();";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing
    "procedure call used as function value is diagnosed"
    ~needle:"\"HELPER\" is a procedure and cannot be used as a value" diags

let test_procedure_call_statement_is_not_value_error () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "procedure call statement URI"
      (Lib.Uri_path.docuri_of_string "file:///procedure-call-statement.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "PROC HELPER();";
        "BEGIN";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  HELPER();";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing
    "procedure call statement does not require a function result"
    ~needle:"\"HELPER\" is a procedure and cannot be used as a value" diags

let test_truly_undefined_still_errors () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "undefined URI"
      (Lib.Uri_path.docuri_of_string "file:///undefined-still-errors.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  REALLY_MISSING = 1;";
        "  REALLY_MISSING_PROC;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "undefined item still errors"
    ~needle:"REALLY_MISSING" diags;
  expect_diagnostic_containing "undefined proc still errors"
    ~needle:"REALLY_MISSING_PROC" diags

let test_open_doc_owner_survives_source_set_replacement () =
  let root = mk_temp_dir "jovial-mixed-workspace" in
  let system = Filename.concat root "system" in
  let source = Filename.concat system "source" in
  Unix.mkdir system 0o755;
  Unix.mkdir source 0o755;
  let main_path = Filename.concat source "MAIN.j73" in
  write_text main_path syntax_text;
  expect_true "open document owner survives source-root replacement"
    (Lib.Lsp_server.Private_for_tests
     .open_document_workspace_survives_source_set_replacement
       ~workspace_root:root ~source_root:source ~source_file:main_path)

let lsp_request_json ~(id : int) ~(method_ : string) : string =
  Yojson.Safe.to_string
    (`Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String method_);
        ("params", `Assoc []);
      ])

let lsp_notification_json ~(method_ : string) : string =
  Yojson.Safe.to_string
    (`Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String method_);
        ("params", `Assoc []);
      ])

let method_of_lsp_message msg =
  match Yojson.Safe.from_string msg with
  | `Assoc fields -> (
      match List.assoc_opt "method" fields with
      | Some (`String method_) -> method_
      | _ -> "<missing>")
  | _ -> "<invalid>"

let test_request_priority_dispatch_order () =
  let items =
    [
      lsp_request_json ~id:1 ~method_:"workspace/symbol";
      lsp_request_json ~id:2 ~method_:"textDocument/hover";
      lsp_notification_json ~method_:"textDocument/didChange";
      lsp_request_json ~id:3 ~method_:"textDocument/completion";
      lsp_request_json ~id:4 ~method_:"shutdown";
    ]
  in
  let got =
    Lib.Lsp_server.Private_for_tests.reorder_raw_messages_for_dispatch
      items
    |> List.map method_of_lsp_message
  in
  let want =
    [
      "textDocument/hover";
      "workspace/symbol";
      "textDocument/didChange";
      "shutdown";
      "textDocument/completion";
    ]
  in
  if got <> want then
    failf "request priority dispatch: expected [%s], got [%s]"
      (String.concat "; " want) (String.concat "; " got)
  else
    let queue_got =
      Lib.Lsp_server.Private_for_tests.priority_queue_order items
      |> List.map method_of_lsp_message
    in
    let queue_want =
      [
        "shutdown";
        "textDocument/hover";
        "textDocument/completion";
        "workspace/symbol";
      ]
    in
    if queue_got <> queue_want then
      failf "request priority queue: expected [%s], got [%s]"
        (String.concat "; " queue_want) (String.concat "; " queue_got)
    else (
      expect_true "interactive request preempts workspace symbol"
        (Lib.Lsp_server.Private_for_tests.incoming_preempts_active_method
           ~active:"workspace/symbol" ~incoming:"textDocument/hover");
      expect_false "workspace symbol does not preempt hover"
        (Lib.Lsp_server.Private_for_tests.incoming_preempts_active_method
           ~active:"textDocument/hover" ~incoming:"workspace/symbol"))

let expect_same_json_set name to_json got want =
  let keys xs =
    xs |> List.map (fun x -> Yojson.Safe.to_string (to_json x))
    |> List.sort_uniq String.compare
  in
  let got = keys got in
  let want = keys want in
  if got <> want then
    failf "%s: expected [%s], got [%s]" name (String.concat "; " want)
      (String.concat "; " got)

let test_partial_references_stream () =
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "partial reference URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-partial-ref.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:syntax_text;
  let use_off = find_nth syntax_text ~needle:"TWELVE" ~nth:1 in
  let pos = position_of_offset syntax_text (use_off + 1) in
  let batches = ref [] in
  let streamed =
    Lib.Workspace.references_locations_stream ws ~uri ~pos
      ~include_decl:true
      ~emit:(fun xs -> batches := xs :: !batches)
  in
  let full =
    Lib.Workspace.references_locations_for ws ~uri ~pos ~include_decl:true
  in
  expect_true "reference stream emitted at least one batch" (!batches <> []);
  expect_same_json_set "reference stream matches full result"
    T.Location.yojson_of_t streamed full;
  expect_same_json_set "reference emitted batches match full result"
    T.Location.yojson_of_t (List.concat (List.rev !batches)) full

let test_partial_workspace_symbols_stream () =
  let ws = Lib.Workspace.create () in
  let uri_a =
    expect_some "partial symbol URI A"
      (Lib.Uri_path.docuri_of_string "file:///workspace-partial-symbol-a.j73")
  in
  let uri_b =
    expect_some "partial symbol URI B"
      (Lib.Uri_path.docuri_of_string "file:///workspace-partial-symbol-b.j73")
  in
  let other_text =
    String.concat "\n"
      [ "START"; "DEF PROC SECOND RENT;"; "BEGIN"; "END"; "TERM"; "" ]
  in
  Lib.Workspace.open_doc ws ~uri:uri_a ~file:None ~text:syntax_text;
  Lib.Workspace.open_doc ws ~uri:uri_b ~file:None ~text:other_text;
  let batches = ref [] in
  let streamed =
    Lib.Workspace.workspace_symbols_stream ws ~query:"MAIN"
      ~emit:(fun xs -> batches := xs :: !batches)
  in
  let full = Lib.Workspace.workspace_symbols_for ws ~query:"MAIN" in
  expect_true "workspace symbol stream emitted at least one batch"
    (!batches <> []);
  expect_same_json_set "workspace symbol stream matches full result"
    T.SymbolInformation.yojson_of_t streamed full;
  expect_same_json_set "workspace symbol emitted batches match full result"
    T.SymbolInformation.yojson_of_t (List.concat (List.rev !batches)) full

let test_basic_navigation () =
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "navigation URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-navigation-suite.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:syntax_text;
  let use_off = find_nth syntax_text ~needle:"TWELVE" ~nth:1 in
  let pos = position_of_offset syntax_text (use_off + 1) in
  let defs = Lib.Workspace.definition_locations_for ws ~uri ~pos in
  match defs with
  | loc :: _ -> expect_int "DEFINE navigation line" loc.range.start.line 1
  | [] -> failf "DEFINE navigation returned no locations"

let test_scope_shadowing_prefers_innermost () =
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM VALUE U 6;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 6;";
        "  VALUE = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "shadow URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-shadowing.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"VALUE = 1" ~nth:0 in
  let pos = position_of_offset text (use_off + 1) in
  match Lib.Workspace.definition_locations_for ws ~uri ~pos with
  | loc :: _ -> expect_int "shadowed local VALUE wins" loc.range.start.line 4
  | [] -> failf "shadowed VALUE navigation returned no locations"

let test_hover_shadowing_prefers_innermost () =
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM VALUE U 6;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE F 20;";
        "  VALUE = 1.0;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "shadow hover URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-shadow-hover.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"VALUE = 1.0" ~nth:0 in
  let pos = position_of_offset text (use_off + 1) in
  let hover =
    expect_some "shadowed VALUE hover" (Lib.Workspace.hover_for ws ~uri ~pos)
  in
  let body = hover_markdown_text hover in
  expect_true "hover title is symbol name"
    (string_contains ~needle:"### `VALUE`" body);
  expect_true "hover shows declaration label"
    (string_contains ~needle:"Declaration:" body);
  expect_true "hover shows declared file"
    (string_contains ~needle:"workspace-shadow-hover.j73" body);
  expect_true "hover shows innermost declaration"
    (string_contains ~needle:"ITEM VALUE F 20" body);
  expect_false "hover does not show outer declaration"
    (string_contains ~needle:"ITEM VALUE U 6" body);
  expect_no_hover_metadata "shadowed VALUE hover" body

let test_hover_unresolved_is_semantic_message () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  MISSING = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "unresolved hover URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-unresolved-hover.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"MISSING = 1" ~nth:0 in
  let pos = position_of_offset text (use_off + 1) in
  let hover =
    expect_some "unresolved MISSING hover" (Lib.Workspace.hover_for ws ~uri ~pos)
  in
  let body = hover_markdown_text hover in
  expect_true "hover reports unresolved symbol"
    (string_contains ~needle:"Unresolved JOVIAL symbol" body);
  expect_no_hover_metadata "unresolved MISSING hover" body

let test_compool_import_definition () =
  let ws = Lib.Workspace.create () in
  let compool_uri =
    expect_some "compool URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-target-compool.j73")
  in
  let main_uri =
    expect_some "compool user URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-compool-user.j73")
  in
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF BEGIN";
        "  ITEM SHARED U 6;";
        "END";
        "TERM";
        "";
      ]
  in
  let main_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('TARGET');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  SHARED = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace.open_doc ws ~uri:compool_uri ~file:None ~text:compool_text;
  Lib.Workspace.open_doc ws ~uri:main_uri ~file:None ~text:main_text;
  let use_off = find_nth main_text ~needle:"SHARED = 1" ~nth:0 in
  let pos = position_of_offset main_text (use_off + 1) in
  match Lib.Workspace.definition_locations_for ws ~uri:main_uri ~pos with
  | loc :: _ -> expect_int "compool SHARED definition line" loc.range.start.line 3
  | [] -> failf "compool-imported SHARED returned no locations"

let test_ref_proc_links_to_def_proc () =
  let ws = Lib.Workspace.create () in
  let def_uri =
    expect_some "def proc URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-def-proc.j73")
  in
  let ref_uri =
    expect_some "ref proc URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-ref-proc.j73")
  in
  let def_text =
    String.concat "\n"
      [ "START"; "DEF PROC CALLEE RENT;"; "BEGIN"; "END"; "TERM"; "" ]
  in
  let ref_text =
    String.concat "\n"
      [
        "START";
        "REF PROC CALLEE RENT;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  CALLEE;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace.open_doc ws ~uri:def_uri ~file:None ~text:def_text;
  Lib.Workspace.open_doc ws ~uri:ref_uri ~file:None ~text:ref_text;
  let call_off = find_nth ref_text ~needle:"CALLEE;" ~nth:0 in
  let pos = position_of_offset ref_text (call_off + 1) in
  (match Lib.Workspace.definition_locations_for ws ~uri:ref_uri ~pos with
  | loc :: _ -> expect_int "REF PROC definition jumps to DEF PROC" loc.range.start.line 1
  | [] -> failf "REF PROC definition returned no implementation");
  match Lib.Workspace.declaration_locations_for ws ~uri:ref_uri ~pos with
  | loc :: _ -> expect_int "REF PROC declaration stays on REF line" loc.range.start.line 1
  | [] -> failf "REF PROC declaration returned no declaration"

let test_jovial_hover_nav_fixture () =
  let root = mk_temp_dir "jovial-hover-nav-fixture" in
  let copy_fixture name =
    let src = jovial_hover_nav_fixture name in
    let text = read_text src in
    let dst = Filename.concat root name in
    write_text dst text;
    (dst, text)
  in
  let compool_path, compool_text = copy_fixture "compool_data.j73" in
  let find_path, find_text = copy_fixture "find_impl.j73" in
  let main_path, main_text = copy_fixture "main.j73" in
  let types_path, types_text = copy_fixture "types.j73" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore
    (Lib.Workspace_state.set_source_files ws
       [ compool_path; find_path; main_path; types_path ]);
  Lib.Workspace_index_graph.pump_index_background ws;
  Lib.Workspace_index_graph.ensure_graph_fresh ws;
  let open_doc label path text =
    let uri = expect_some (label ^ " URI") (Lib.Uri_path.docuri_of_path path) in
    Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:(Some path) ~text;
    uri
  in
  let compool_uri = open_doc "fixture compool" compool_path compool_text in
  let find_uri = open_doc "fixture find" find_path find_text in
  let types_uri = open_doc "fixture types" types_path types_text in
  let main_uri = open_doc "fixture main" main_path main_text in
  ignore (Lib.Workspace_doc_lifecycle.revalidate_all ws);
  Lib.Workspace_index_graph.ensure_graph_fresh ws;
  let expect_contains label needle text =
    if not (string_contains ~needle text) then
      failf "%s: missing %S in hover:\n%s" label needle text
  in
  let hover_text label uri text needle nth =
    let off = find_nth text ~needle ~nth in
    let pos = position_of_offset text (off + 1) in
    let hover =
      expect_some label (Lib.Workspace_navigation.hover_for ws ~uri ~pos)
    in
    hover_markdown_text hover
  in
  let find_call_pos =
    let off = find_nth main_text ~needle:"FIND(CLOCK" ~nth:0 in
    position_of_offset main_text (off + 1)
  in
  (match
     Lib.Workspace_navigation.definition_locations_for ws ~uri:main_uri
       ~pos:find_call_pos
   with
  | loc :: _ ->
      expect_int "FIND call definition goes to DEF PROC" loc.range.start.line 1
  | [] -> failf "FIND call definition returned no locations");
  (match
     Lib.Workspace_navigation.implementation_locations_for ws ~uri:main_uri
       ~pos:find_call_pos
   with
  | loc :: _ ->
      expect_int "FIND call implementation goes to real body"
        loc.range.start.line 1
  | [] -> failf "FIND call implementation returned no locations");
  let ref_hover =
    hover_text "REF PROC FIND hover" compool_uri compool_text "FIND(CODE" 0
  in
  expect_contains "REF PROC hover classification"
    "JOVIAL external REF function import" ref_hover;
  expect_contains "REF PROC hover meaning"
    "not the real definition or implementation" ref_hover;
  let def_hover =
    hover_text "DEF PROC FIND hover" find_uri find_text "FIND(CODE" 0
  in
  expect_contains "DEF PROC hover classification"
    "JOVIAL external DEF function" def_hover;
  let clock_hover =
    hover_text "CLOCK hover" main_uri main_text "CLOCK COUNTER" 0
  in
  expect_contains "CLOCK is item" "JOVIAL item" clock_hover;
  expect_contains "CLOCK type display" "`COUNTER`" clock_hover;
  expect_contains "CLOCK resolved type" "`U 10`" clock_hover;
  let limit_hover =
    hover_text "LIMIT hover" main_uri main_text "LIMIT" 0
  in
  expect_contains "LIMIT is external DEF item" "external DEF item" limit_hover;
  expect_contains "LIMIT type display" "`U 10`" limit_hover;
  let privilege_hover =
    hover_text "PRIVILEGE hover" main_uri main_text "PRIVILEGE" 0
  in
  expect_contains "PRIVILEGE is table" "table" privilege_hover;
  expect_false "PRIVILEGE is not generic item"
    (string_contains ~needle:"Classification: item" privilege_hover);
  let data_hover =
    hover_text "ICOMPOOL DATA hover" main_uri main_text "DATA" 0
  in
  expect_contains "DATA is COMPOOL import" "JOVIAL COMPOOL import" data_hover;
  let u_hover =
    hover_text "U 10 built-in hover" types_uri types_text "U 10" 0
  in
  expect_contains "U 10 built-in unsigned"
    "JOVIAL built-in unsigned integer type" u_hover;
  let f_hover =
    hover_text "F 30 built-in hover" types_uri types_text "F 30" 0
  in
  expect_contains "F 30 built-in floating"
    "JOVIAL built-in floating type" f_hover

let test_ambiguous_table_field_does_not_resolve () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  TYPE REC1 TABLE;";
        "  BEGIN";
        "    ITEM FIELD U 1;";
        "  END";
        "  TYPE REC2 TABLE;";
        "  BEGIN";
        "    ITEM FIELD U 1;";
        "  END";
        "  FIELD = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "ambiguous field URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-ambiguous-field.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"FIELD = 1" ~nth:0 in
  let pos = position_of_offset text (use_off + 1) in
  let defs = Lib.Workspace.definition_locations_for ws ~uri ~pos in
  match defs with
  | [] -> ()
  | _ -> failf "ambiguous table field resolved to an arbitrary definition"

let dependency_change_text =
  String.concat "\n"
    [
      "START";
      "ITEM VALUE U 6;";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  VALUE = 1;";
      "END";
      "TERM";
      "";
    ]

let dependency_change_workspace () =
  let root = mk_temp_dir "jovial-dep-change" in
  let path = Filename.concat root "MAIN.j73" in
  write_text path dependency_change_text;
  let uri = expect_some "dependency change URI" (Lib.Uri_path.docuri_of_path path) in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1
    ~inline_catch_up:false ws ~uri ~file:(Some path)
    ~text:dependency_change_text;
  let open Lib.Workspace_foundation in
  ws.graph_needs_refresh <- false;
  (ws, uri)

let test_procedure_body_change_keeps_graph_clean () =
  let ws, uri = dependency_change_workspace () in
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:(range_of_substring dependency_change_text ~needle:"1;" ~nth:0)
      ~text:"2;" ()
  in
  let open Lib.Workspace_foundation in
  let epoch_before = ws.graph_epoch in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri
    ~changes:[ change ];
  expect_false "procedure body edit does not dirty dependency graph"
    ws.graph_needs_refresh;
  expect_int "procedure body edit keeps graph epoch" ws.graph_epoch epoch_before

let test_declaration_change_dirties_graph () =
  let ws, uri = dependency_change_workspace () in
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:(range_of_substring dependency_change_text ~needle:"U 6" ~nth:0)
      ~text:"U 12" ()
  in
  let open Lib.Workspace_foundation in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri
    ~changes:[ change ];
  expect_true "declaration edit dirties dependency graph"
    ws.graph_needs_refresh

let test_compool_export_change_revalidates_importers () =
  let ws = Lib.Workspace_state.create () in
  let compool_uri =
    expect_some "dependency compool URI"
      (Lib.Uri_path.docuri_of_string "file:///dep-target-compool.j73")
  in
  let importer_uri =
    expect_some "dependency importer URI"
      (Lib.Uri_path.docuri_of_string "file:///dep-compool-user.j73")
  in
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF BEGIN";
        "  ITEM SHARED U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('TARGET');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  SHARED = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:compool_uri ~file:None
    ~text:compool_text;
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:importer_uri ~file:None
    ~text:importer_text;
  let open Lib.Workspace_foundation in
  ws.graph_needs_refresh <- false;
  Hashtbl.clear ws.open_diag_revalidate_set;
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:(range_of_substring compool_text ~needle:"U 1" ~nth:0)
      ~text:"U 2" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri:compool_uri
    ~changes:[ change ];
  expect_true "compool export edit dirties graph" ws.graph_needs_refresh;
  expect_true "compool export edit revalidates importer"
    (Hashtbl.mem ws.open_diag_revalidate_set
       (Lib.Uri_path.docuri_to_string importer_uri))

let test_icopy_include_reverse_dependency () =
  let root = mk_temp_dir "jovial-icopy-dep" in
  let include_path = Filename.concat root "INC.j73" in
  let user_path = Filename.concat root "USER.j73" in
  write_text include_path
    (String.concat "\n"
       [ "START"; "DEF PROC HELPER RENT;"; "BEGIN"; "END"; "TERM"; "" ]);
  write_text user_path
    (String.concat "\n"
       [
         "START";
         "ICOPY ('INC.j73');";
         "DEF PROC MAIN RENT;";
         "BEGIN";
         "END";
         "TERM";
         "";
       ]);
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ include_path; user_path ]);
  Lib.Workspace_index_graph.pump_index_background ws;
  Lib.Workspace_index_graph.ensure_graph_fresh ws;
  let open Lib.Workspace_foundation in
  let include_key = normalize_path include_path in
  let user_key = normalize_path user_path in
  let include_node =
    expect_some "include graph node" (Hashtbl.find_opt ws.graph_nodes include_key)
  in
  expect_true "ICOPY reverse importer recorded"
    (List.mem user_key include_node.gn_rev_importers);
  let user_node =
    expect_some "user graph node" (Hashtbl.find_opt ws.graph_nodes user_key)
  in
  expect_true "ICOPY dependency edge recorded"
    (List.exists
       (fun edge ->
         edge.de_kind = ICopyInclude && edge.de_path_key = Some include_key)
       user_node.gn_dependency_edges);
  Lib.Workspace_doc_lifecycle.apply_watched_file_changes ws
    ~changes:[ (include_path, `Changed) ];
  expect_true "ICOPY target change enqueues dependent"
    (Hashtbl.mem ws.bg_enqueued user_key)

let quoted_identifier_text =
  String.concat "\n"
    [
      "START";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  ITEM NUMBER'1' U 6;";
      "  NUMBER'1' = 3;";
      "END";
      "TERM";
      "";
    ]

let expect_quoted_identifier_definition ws uri use_off label rel =
  let pos = position_of_offset quoted_identifier_text (use_off + rel) in
  let defs = Lib.Workspace.definition_locations_for ws ~uri ~pos in
  match defs with
  | loc :: _ -> expect_int label loc.range.start.line 3
  | [] -> failf "%s: quoted identifier navigation returned no locations" label

let test_quoted_identifier_navigation () =
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "quoted navigation URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-quoted-navigation.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:quoted_identifier_text;
  let use_off = find_nth quoted_identifier_text ~needle:"NUMBER'1'" ~nth:1 in
  expect_quoted_identifier_definition ws uri use_off "quoted identifier letters"
    1;
  expect_quoted_identifier_definition ws uri use_off "quoted identifier digit"
    7;
  expect_quoted_identifier_definition ws uri use_off "quoted identifier quote"
    8;
  let pos = position_of_offset quoted_identifier_text (use_off + 7) in
  match Lib.Workspace.prepare_rename_for ws ~uri ~pos with
  | Some (`RangeWithPlaceholder (range, placeholder)) ->
      if placeholder <> "NUMBER'1'" then
        failf "quoted identifier rename placeholder: expected NUMBER'1', got %S"
          placeholder;
      expect_int "quoted identifier rename start line" range.start.line 4;
      expect_int "quoted identifier rename start col" range.start.character 2;
      expect_int "quoted identifier rename end line" range.end_.line 4;
      expect_int "quoted identifier rename end col" range.end_.character 11
  | Some (`Range _) ->
      failf "quoted identifier rename returned a range without placeholder"
  | None -> failf "quoted identifier prepareRename returned None"

let test_file_modes_and_huge_policy () =
  let settings = Lib.Workspace_settings.from_env () in
  let ws = Lib.Workspace_state.create ~settings () in
  let mode_name = function
    | Lib.Workspace_tuning.Small -> "small"
    | Lib.Workspace_tuning.Normal -> "normal"
    | Lib.Workspace_tuning.Large -> "large"
    | Lib.Workspace_tuning.Huge -> "huge"
  in
  let expect_mode label bytes want =
    let got = Lib.Workspace_tuning.file_mode_of_size ws ~bytes in
    if got <> want then
      failf "%s: expected %s, got %s" label (mode_name want) (mode_name got)
  in
  expect_mode "small mode" 1024 Lib.Workspace_tuning.Small;
  expect_mode "normal mode" settings.large_file_threshold_bytes
    Lib.Workspace_tuning.Normal;
  expect_mode "large mode" settings.full_semantic_tokens_max_bytes
    Lib.Workspace_tuning.Large;
  expect_mode "huge mode" settings.huge_file_threshold_bytes
    Lib.Workspace_tuning.Huge;
  expect_false "huge full parse disabled by default"
    (Lib.Workspace_tuning.full_parse_allowed_for_size ws
       ~bytes:settings.huge_file_threshold_bytes)

let test_persistent_workspace_index_roundtrip () =
  let root = mk_temp_dir "jovial-persistent-index" in
  let main_path = Filename.concat root "MAIN.j73" in
  let compool_path = Filename.concat root "TARGET.j73" in
  write_text main_path (main_text "MAIN");
  write_text compool_path target_text;
  let idx =
    Lib.Workspace_index.of_source_files ~source_extensions:[ ".j73" ] ~root
      ~paths:[ main_path; compool_path ]
  in
  Lib.Workspace_persistent_index.save_workspace_index ~root idx;
  let loaded =
    expect_some "persistent workspace index"
      (Lib.Workspace_persistent_index.load_workspace_index
         ~source_extensions:[ ".j73" ] ~root)
  in
  expect_int "persistent source count" (Lib.Workspace_index.source_count loaded)
    2;
  ignore
    (expect_some "persistent compool"
       (Lib.Workspace_index.find_compool loaded ~name:"TARGET"));
  write_text compool_path "";
  ignore (Lib.Workspace_index.replace_source_files loaded ~paths:[ main_path ]);
  expect_none "persistent deleted file pruned"
    (Lib.Workspace_index.find_compool loaded ~name:"TARGET");
  let ws = Lib.Workspace.create () in
  let main_uri = expect_some "persistent main URI" (Lib.Uri_path.docuri_of_path main_path) in
  let compool_uri =
    expect_some "persistent compool URI" (Lib.Uri_path.docuri_of_path compool_path)
  in
  Lib.Workspace.open_doc ws ~uri:main_uri ~file:(Some main_path)
    ~text:(main_text "MAIN");
  Lib.Workspace.open_doc ws ~uri:compool_uri ~file:(Some compool_path)
    ~text:target_text;
  Lib.Workspace.snapshot ws
  |> Lib.Workspace_persistent_index.save_snapshot_index ~root;
  let expect_cache_file name path =
    expect_true name (Sys.file_exists path)
  in
  expect_cache_file "persistent symbols.json"
    (Lib.Workspace_persistent_index.symbols_json_path ~root);
  expect_cache_file "persistent refs.json"
    (Lib.Workspace_persistent_index.refs_json_path ~root);
  expect_cache_file "persistent scopes.json"
    (Lib.Workspace_persistent_index.scopes_json_path ~root);
  expect_cache_file "persistent deps.json"
    (Lib.Workspace_persistent_index.deps_json_path ~root);
  expect_cache_file "persistent macros.json"
    (Lib.Workspace_persistent_index.macros_json_path ~root);
  expect_cache_file "persistent diagnostics.json"
    (Lib.Workspace_persistent_index.diagnostics_json_path ~root)

let test_skeleton_snapshot_ide_query () =
  let root = mk_temp_dir "jovial-snapshot-query" in
  let path = Filename.concat root "MAIN.j73" in
  let text = main_text "MAIN" in
  write_text path text;
  let uri =
    expect_some "snapshot uri" (Lib.Uri_path.docuri_of_path path)
  in
  let ws = Lib.Workspace.create () in
  Lib.Workspace.open_doc ws ~uri ~file:(Some path) ~text;
  let snap = Lib.Workspace.snapshot ws in
  let uri_s = Lib.Uri_path.docuri_to_string uri in
  let symbols = Lib.Ide_query.workspace_symbols snap ~query:"MAIN" in
  expect_true "snapshot workspace symbols"
    (List.exists
       (fun (s : T.SymbolInformation.t) ->
         match T.SymbolInformation.yojson_of_t s with
         | `Assoc fields -> (
             match List.assoc_opt "name" fields with
             | Some (`String "MAIN") -> true
             | _ -> false)
         | _ -> false)
       symbols);
  let doc_symbols = Lib.Ide_query.document_symbols snap ~uri:uri_s in
  expect_true "snapshot document symbols" (doc_symbols <> []);
  let body_scope =
    Lib.Scope_graph.scopes snap.Lib.Workspace_snapshot.scopes
    |> List.find_opt (fun (scope : Lib.Scope_graph.scope) ->
           scope.kind = Lib.Scope_graph.ModuleBodyScope)
  in
  let body_scope = expect_some "snapshot module body scope" body_scope in
  ignore
    (expect_some "snapshot scope lookup by symbol name"
       (Lib.Scope_graph.lookup_symbol_id snap.Lib.Workspace_snapshot.scopes
          ~scope_id:body_scope.id ~normalized_name:"MAIN"))

let run name f =
  try
    f ();
    Printf.printf "ok %s\n%!" name
  with exn ->
    failf "%s failed: %s" name (Printexc.to_string exn)

let () =
  Random.self_init ();
  let tests =
    [
      ("source_set_index", test_source_set_index);
      ( "workspace_index_health_watchdog_repairs_index",
        test_workspace_index_health_watchdog_repairs_index );
      ("document_syntax_cache", test_document_syntax_cache);
      ("document_stale_parse_state", test_document_stale_parse_state);
      ("incremental_syntax_metadata", test_incremental_syntax_metadata);
      ("shifted_suffix_token_reuse", test_shifted_suffix_token_reuse);
      ( "checkpoint_reuse_after_failed_parse",
        test_checkpoint_reuse_after_failed_parse );
      ( "token_skeleton_symbols_for_broken_file",
        test_token_skeleton_symbols_for_broken_file );
      ("quick_nav_uses_token_skeleton", test_quick_nav_uses_token_skeleton);
      ("lexer_regressions", test_lexer_regressions);
      ("macro_expansion_source_mapping", test_macro_expansion_source_mapping);
      ("parser_profiles", test_parser_profiles);
      ("cache_shedding_and_cst_bounds", test_cache_shedding_and_cst_bounds);
      ("semantic_tokens_delta", test_semantic_tokens_delta);
      ( "hover_large_stale_does_not_sync_parse",
        test_hover_large_stale_does_not_sync_parse );
      ( "semantic_range_large_stale_does_not_sync_parse",
        test_semantic_range_large_stale_does_not_sync_parse );
      ( "open_doc_dequeue_preempts_unopened_high",
        test_open_doc_dequeue_preempts_unopened_high );
      ( "specific_open_doc_catchup_handles_multiple_pending",
        test_specific_open_doc_catchup_handles_multiple_pending );
      ( "parse_worker_stale_open_job_detected",
        test_parse_worker_stale_open_job_detected );
      ( "parse_worker_path_result_does_not_replace_open_doc",
        test_parse_worker_path_result_does_not_replace_open_doc );
      ("lsp_capabilities_diagnostic_pull", test_lsp_capabilities_diagnostic_pull);
      ("versioned_diagnostic_publish", test_versioned_diagnostic_publish);
      ( "unversioned_closed_diagnostic_publish",
        test_unversioned_closed_diagnostic_publish );
      ( "closed_document_diagnostics_survive_close",
        test_closed_document_diagnostics_survive_close );
      ( "closed_document_diagnostics_refresh_after_file_change",
        test_closed_document_diagnostics_refresh_after_file_change );
      ( "selected_imported_item_helper_type_no_false_positive",
        test_selected_imported_item_helper_type_no_false_positive );
      ( "direct_missing_custom_type_still_hints",
        test_direct_missing_custom_type_still_hints );
      ( "ref_proc_visibility_suppresses_undefined",
        test_ref_proc_visibility_suppresses_undefined );
      ( "compool_helper_proc_visibility_suppresses_undefined",
        test_compool_helper_proc_visibility_suppresses_undefined );
      ( "imported_ref_item_and_proc_visibility_suppresses_undefined",
        test_imported_ref_item_and_proc_visibility_suppresses_undefined );
      ( "hover_nav_fixture_import_diagnostics_are_not_undefined",
        test_hover_nav_fixture_import_diagnostics_are_not_undefined );
      ( "symbol_hints_include_skeleton_imported_helpers",
        test_symbol_hints_include_skeleton_imported_helpers );
      ("truly_undefined_still_errors", test_truly_undefined_still_errors);
      ( "open_doc_owner_survives_source_set_replacement",
        test_open_doc_owner_survives_source_set_replacement );
      ("request_priority_dispatch_order", test_request_priority_dispatch_order);
      ("partial_references_stream", test_partial_references_stream);
      ("partial_workspace_symbols_stream", test_partial_workspace_symbols_stream);
      ("basic_navigation", test_basic_navigation);
      ("scope_shadowing_prefers_innermost", test_scope_shadowing_prefers_innermost);
      ( "hover_shadowing_prefers_innermost",
        test_hover_shadowing_prefers_innermost );
      ( "hover_unresolved_is_semantic_message",
        test_hover_unresolved_is_semantic_message );
      ("compool_import_definition", test_compool_import_definition);
      ("ref_proc_links_to_def_proc", test_ref_proc_links_to_def_proc);
      ("jovial_hover_nav_fixture", test_jovial_hover_nav_fixture);
      ( "function_return_expression_type_mismatch",
        test_function_return_expression_type_mismatch );
      ( "function_result_assignment_type_mismatch",
        test_function_result_assignment_type_mismatch );
      ( "function_return_matching_type_has_no_mismatch",
        test_function_return_matching_type_has_no_mismatch );
      ( "procedure_call_used_as_function_value_errors",
        test_procedure_call_used_as_function_value_errors );
      ( "procedure_call_statement_is_not_value_error",
        test_procedure_call_statement_is_not_value_error );
      ( "ambiguous_table_field_does_not_resolve",
        test_ambiguous_table_field_does_not_resolve );
      ( "procedure_body_change_keeps_graph_clean",
        test_procedure_body_change_keeps_graph_clean );
      ("declaration_change_dirties_graph", test_declaration_change_dirties_graph);
      ( "compool_export_change_revalidates_importers",
        test_compool_export_change_revalidates_importers );
      ( "icopy_include_reverse_dependency",
        test_icopy_include_reverse_dependency );
      ("quoted_identifier_navigation", test_quoted_identifier_navigation);
      ("file_modes_and_huge_policy", test_file_modes_and_huge_policy);
      ( "persistent_workspace_index_roundtrip",
        test_persistent_workspace_index_roundtrip );
      ("skeleton_snapshot_ide_query", test_skeleton_snapshot_ide_query);
    ]
  in
  let selected = List.tl (Array.to_list Sys.argv) in
  List.iter
    (fun (name, f) ->
      if selected = [] || List.mem name selected then run name f)
    tests
