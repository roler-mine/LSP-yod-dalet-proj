(* Module overview: Test support and regression coverage for workspace core suite test. *)

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

let find_existing_relative_path (rel_parts : string list) : string =
  let rec candidates_from dir depth acc =
    if depth < 0 then acc
    else
      let candidate = List.fold_left Filename.concat dir rel_parts in
      let parent = Filename.dirname dir in
      let acc = candidate :: acc in
      if parent = dir then acc else candidates_from parent (depth - 1) acc
  in
  let candidates = candidates_from (Sys.getcwd ()) 8 [] in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failf "missing repository path %s" (String.concat "/" rel_parts)

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
    let rec at i j = j = m || (text.[i + j] = needle.[j] && at i (j + 1)) in
    let rec loop i = i + m <= n && (at i 0 || loop (i + 1)) in
    loop 0

let is_ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let has_identifier_token ~(token : string) (text : string) : bool =
  let n = String.length text in
  let m = String.length token in
  let token_at i =
    let rec loop j = j = m || (text.[i + j] = token.[j] && loop (j + 1)) in
    loop 0
  in
  let before_ok i = i = 0 || not (is_ident_char text.[i - 1]) in
  let after_ok i = i + m >= n || not (is_ident_char text.[i + m]) in
  let rec loop i =
    i + m <= n && ((before_ok i && after_ok i && token_at i) || loop (i + 1))
  in
  m > 0 && loop 0

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

let normalize_path (path : string) : string =
  Lib.Uri_path.normalize_path_key path

let normalize_name (name : string) : string =
  String.uppercase_ascii (String.trim name)

let path_in (paths : string list) (path : string) : bool =
  let want = normalize_path path in
  List.exists (fun got -> normalize_path got = want) paths

let expect_true name got = if not got then failf "%s: expected true" name
let expect_false name got = if got then failf "%s: expected false" name

let rec source_files_under dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
      let path = Filename.concat dir name in
      let is_dir = try Sys.is_directory path with _ -> false in
      if is_dir then source_files_under path
      else if
        Filename.check_suffix path ".ml" || Filename.check_suffix path ".mli"
      then [ path ]
      else [])

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

let expect_string name got want =
  if got <> want then failf "%s: expected %S, got %S" name want got

let expect_some name = function
  | Some value -> value
  | None -> failf "%s: expected Some _" name

let expect_none name = function
  | None -> ()
  | Some _ -> failf "%s: expected None" name

let expect_path name paths path =
  if not (path_in paths path) then
    failf "%s: expected %s in [%s]" name path (String.concat "; " paths)

let expect_json_field name key = function
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some value -> value
      | None -> failf "%s: expected JSON field %S" name key)
  | _ -> failf "%s: expected JSON object" name

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
    [
      "START";
      "COMPOOL TARGET;";
      "DEF BEGIN";
      "  ITEM TARGET'VAL U 1;";
      "END";
      "TERM";
      "";
    ]

let hidden_text = String.concat "\n" [ "START"; "COMPOOL HIDDEN;"; "TERM"; "" ]

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
    (Lib.Workspace_index.source_count idx)
    2;
  expect_path "MAIN import hint"
    (Lib.Workspace_index.source_import_hints idx ~path:main_path)
    "TARGET";
  ignore
    (expect_some "TARGET compool"
       (Lib.Workspace_index.find_compool idx ~name:"TARGET"));
  expect_none "HIDDEN compool is not discovered by crawl"
    (Lib.Workspace_index.find_compool idx ~name:"HIDDEN");
  expect_path "MAIN proc hint"
    (Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"MAIN")
    main_path;

  write_text main_path (main_text "NEXT");
  ignore
    (Lib.Workspace_index.apply_file_change idx ~path:main_path ~kind:Changed);
  expect_false "old proc hint removed"
    (path_in
       (Lib.Workspace_index.source_paths_for_proc_hint idx ~name:"MAIN")
       main_path);
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
  ignore
    (expect_some "created compool added"
       (Lib.Workspace_index.find_compool idx ~name:"CREATED"));
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
  expect_int "initial healthy source count"
    (Lib.Workspace_index.source_count idx)
    2;
  ws.Lib.Workspace_foundation.index <- None;
  expect_true "missing index is restored"
    (Lib.Workspace_index_graph.ensure_index_health ws);
  let restored =
    expect_some "restored index" ws.Lib.Workspace_foundation.index
  in
  expect_int "restored source count"
    (Lib.Workspace_index.source_count restored)
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
  expect_int "repaired source count"
    (Lib.Workspace_index.source_count repaired)
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
    (match cache.raw_tokens with
    | Some toks -> Array.length toks > 0
    | None -> false);
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
  expect_int "stale diagnostics cleared"
    (List.length stale.Lib.Document.diags)
    0;
  let reparsed = Lib.Document.ensure_parsed stale in
  expect_int "ensure_parsed catches up parse revision"
    reparsed.Lib.Document.parse_rev reparsed.Lib.Document.rev;
  ignore
    (expect_some "current parse accessor accepts current parse"
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
    (List.length ensured.Lib.Document.parse_diags)
    1

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
  let updated =
    Lib.Document.apply_changes_and_reparse ~changes:[ change ] doc
  in
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
  expect_true
    (name ^ " attempted token reuse")
    cache.Lib.Syntax_cache.token_reuse.attempted;
  expect_true
    (name ^ " rejoined shifted suffix")
    (cache.Lib.Syntax_cache.token_reuse.reused_suffix_tokens > 0);
  expect_none
    (name ^ " no token fallback")
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
  expect_none "broken parse has no full AST" previous.Lib.Syntax_cache.parse.ast;
  expect_true "broken parse recorded checkpoints"
    (previous.Lib.Syntax_cache.checkpoint_stats.checkpoint_count > 0);
  let edit_off = find_nth text ~needle:"OTHER" ~nth:0 + String.length "OTH" in
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
  expect_none "skeleton fixture has no full AST"
    cache.Lib.Syntax_cache.parse.ast;
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
      expect_true
        ("document symbols include skeleton " ^ name)
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
      expect_true
        ("quick nav skeleton includes " ^ name)
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
      match d.message with `String s -> s | `MarkupContent mc -> mc.value)
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

let diagnostic_authority_text
    (diag : Lib.Workspace_diagnostic_authority.diagnostic) : string =
  match diag.lsp.message with `String s -> s | `MarkupContent mc -> mc.value

let expect_authority_diagnostic label
    ~(authority : Lib.Workspace_diagnostic_authority.t) ~(needle : string)
    (diags : Lib.Workspace_diagnostic_authority.diagnostic list) : unit =
  if
    not
      (List.exists
         (fun (diag : Lib.Workspace_diagnostic_authority.diagnostic) ->
           diag.authority = authority
           && string_contains ~needle (diagnostic_authority_text diag))
         diags)
  then
    failf "%s: expected %s diagnostic containing %S, got [%s]" label
      (Lib.Workspace_diagnostic_authority.label authority)
      needle
      (String.concat "; " (List.map diagnostic_authority_text diags))

let code_action_title (action : T.CodeAction.t) : string = action.title

let find_code_action_containing label ~(needle : string)
    (actions : T.CodeAction.t list) : T.CodeAction.t =
  match
    List.find_opt
      (fun action -> string_contains ~needle (code_action_title action))
      actions
  with
  | Some action -> action
  | None ->
      failf "%s: expected code action containing %S, got [%s]" label needle
        (String.concat "; " (List.map code_action_title actions))

let expect_no_code_action_containing label ~(needle : string)
    (actions : T.CodeAction.t list) : unit =
  if
    List.exists
      (fun action -> string_contains ~needle (code_action_title action))
      actions
  then
    failf "%s: unexpected code action containing %S in [%s]" label needle
      (String.concat "; " (List.map code_action_title actions))

let expect_single_workspace_text_edit label (action : T.CodeAction.t) :
    T.TextEdit.t =
  match action.edit with
  | None -> failf "%s: expected workspace edit" label
  | Some edit -> (
      match edit.changes with
      | Some [ (_, [ text_edit ]) ] -> text_edit
      | Some _ -> failf "%s: expected one URI with one text edit" label
      | None -> failf "%s: expected workspace edit changes" label)

let apply_text_edit label (text : string) (edit : T.TextEdit.t) : string =
  let start_pos = edit.range.T.Range.start in
  let end_pos = edit.range.T.Range.end_ in
  if
    start_pos.T.Position.line <> end_pos.T.Position.line
    || start_pos.character <> end_pos.character
  then failf "%s: expected insertion edit" label;
  let idx = Lib.Text_index.of_string text in
  let off =
    expect_some (label ^ " offset")
      (Lib.Text_index.offset_of_line_col idx ~line:start_pos.line
         ~col:start_pos.character)
  in
  String.sub text 0 off ^ edit.newText
  ^ String.sub text off (String.length text - off)

let apply_text_edits label (text : string) (edits : T.TextEdit.t list) : string
    =
  let offset_of_pos pos =
    let idx = Lib.Text_index.of_string text in
    expect_some (label ^ " offset")
      (Lib.Text_index.offset_of_line_col idx ~line:pos.T.Position.line
         ~col:pos.character)
  in
  let with_offsets =
    edits
    |> List.map (fun (edit : T.TextEdit.t) ->
        ( offset_of_pos edit.range.start,
          offset_of_pos edit.range.end_,
          edit.newText ))
    |> List.sort (fun (a, _, _) (b, _, _) -> compare b a)
  in
  List.fold_left
    (fun acc (start_off, end_off, new_text) ->
      if start_off > end_off || end_off > String.length acc then
        failf "%s: invalid edit offsets" label;
      String.sub acc 0 start_off ^ new_text
      ^ String.sub acc end_off (String.length acc - end_off))
    text with_offsets

let workspace_edit_edits_for_uri label (edit : T.WorkspaceEdit.t)
    (uri : T.DocumentUri.t) : T.TextEdit.t list =
  let uri_s = Lib.Uri_path.docuri_to_string uri in
  match edit.changes with
  | None -> failf "%s: expected workspace edit changes" label
  | Some changes -> (
      match
        List.find_opt
          (fun (u, _) -> Lib.Uri_path.docuri_to_string u = uri_s)
          changes
      with
      | Some (_, edits) -> edits
      | None -> failf "%s: expected edits for %s" label uri_s)

let apply_workspace_edit_for_uri label text edit uri =
  workspace_edit_edits_for_uri label edit uri |> apply_text_edits label text

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

let test_macro_graph_navigation_hover_references () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEFINE FOO \"BAR\";";
        "DEFINE ADD(A,B) \"$A+$B\";";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 6;";
        "  VALUE = FOO;";
        "  VALUE = ADD(1,2);";
        "  VALUE = 'FOO';";
        "% FOO in comment %";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "macro graph URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-macro-graph.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let doc =
    expect_some "macro graph doc"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs uri)
  in
  let graph = Lib.Macro_graph.of_document doc in
  expect_int "macro graph expansion count"
    (List.length (Lib.Macro_graph.expansions graph))
    2;
  let foo_use_off = find_nth text ~needle:"FOO" ~nth:1 in
  let foo_use_pos = position_of_offset text (foo_use_off + 1) in
  let add_use_off = find_nth text ~needle:"ADD" ~nth:1 in
  let add_use_pos = position_of_offset text (add_use_off + 1) in
  let add_exp =
    expect_some "ADD macro expansion"
      (Lib.Macro_graph.macro_use_at_position graph ~uri ~pos:add_use_pos)
  in
  expect_int "ADD macro actual count"
    (List.length add_exp.Lib.Macro_graph.actuals)
    2;
  let actual_a = List.nth add_exp.actuals 0 in
  let actual_b = List.nth add_exp.actuals 1 in
  expect_string "ADD actual A formal"
    (Option.value actual_a.Lib.Macro_graph.formal ~default:"")
    "A";
  expect_string "ADD actual A text" actual_a.text "1";
  expect_string "ADD actual B formal"
    (Option.value actual_b.Lib.Macro_graph.formal ~default:"")
    "B";
  expect_string "ADD actual B text" actual_b.text "2";
  let add_def =
    expect_some "ADD macro definition through graph"
      (Lib.Macro_graph.definition_of_macro_use graph ~uri ~pos:add_use_pos)
  in
  expect_int "ADD macro definition line"
    add_def.Lib.Workspace_nav_model.loc.Lib.Ast.Loc.start_pos.line 3;
  let defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri ~pos:foo_use_pos
  in
  (match defs with
  | loc :: _ ->
      expect_int "FOO macro goto definition line" loc.range.start.line 1
  | [] -> failf "FOO macro goto definition returned no locations");
  let decl_pos =
    position_of_offset text (find_nth text ~needle:"FOO" ~nth:0 + 1)
  in
  let expect_ref_lines label refs expected =
    let got =
      refs
      |> List.map (fun (loc : T.Location.t) -> loc.range.start.line)
      |> List.sort_uniq Int.compare
    in
    let expected = List.sort_uniq Int.compare expected in
    if got <> expected then
      failf "%s: expected lines [%s], got [%s]" label
        (String.concat "," (List.map string_of_int expected))
        (String.concat "," (List.map string_of_int got))
  in
  let refs_with_decl =
    Lib.Workspace_references.references_locations_for ws ~uri ~pos:decl_pos
      ~include_decl:true
  in
  expect_ref_lines "FOO macro refs include declaration and real use"
    refs_with_decl [ 1; 6 ];
  let refs_without_decl =
    Lib.Workspace_references.references_locations_for ws ~uri ~pos:decl_pos
      ~include_decl:false
  in
  expect_ref_lines "FOO macro refs exclude strings and comments"
    refs_without_decl [ 6 ];
  (match add_exp.expanded_loc with
  | None -> failf "ADD macro expansion had no generated location"
  | Some generated ->
      let source =
        expect_some "ADD generated loc maps to source call"
          (Lib.Macro_graph.source_loc_for_generated_loc graph generated)
      in
      expect_int "ADD source map line" source.Lib.Ast.Loc.start_pos.line
        add_exp.call_site_loc.start_pos.line);
  let hover =
    expect_some "ADD macro hover"
      (Lib.Workspace_hover.hover_for ws ~uri ~pos:add_use_pos)
  in
  let body =
    match T.Hover.yojson_of_t hover with
    | `Assoc fields -> (
        match List.assoc_opt "contents" fields with
        | Some (`Assoc cfields) -> (
            match List.assoc_opt "value" cfields with
            | Some (`String value) -> value
            | _ -> "")
        | _ -> "")
    | _ -> ""
  in
  expect_true "ADD macro hover summary"
    (string_contains ~needle:"JOVIAL define expansion" body);
  expect_true "ADD macro hover actual A"
    (string_contains ~needle:"`A` = `1`" body);
  expect_true "ADD macro hover actual B"
    (string_contains ~needle:"`B` = `2`" body)

let formatting_edits_for_text ?range text =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "formatting URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-formatting.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let options = Lib.Workspace_formatting.default_options in
  match range with
  | None -> Lib.Workspace_formatting.document_edits_for ws ~uri ~options
  | Some range ->
      Lib.Workspace_formatting.range_edits_for ws ~uri ~range ~options

let expect_formatting_snapshot label ?range input expected =
  let edits = formatting_edits_for_text ?range input in
  let actual = apply_text_edits label input edits in
  expect_string label actual expected

let test_formatting_normal_nested_macro_and_broken () =
  let normal =
    String.concat "\n"
      [
        "START";
        "DEFINE TWELVE \"12\";";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "ITEM VALUE U 6;";
        "VALUE = TWELVE;";
        "END";
        "TERM";
        "";
      ]
  in
  let normal_expected =
    String.concat "\n"
      [
        "START";
        "DEFINE TWELVE \"12\";";
        "  DEF PROC MAIN RENT;";
        "  BEGIN";
        "    ITEM VALUE U 6;";
        "    VALUE = TWELVE;";
        "  END";
        "TERM";
        "";
      ]
  in
  expect_formatting_snapshot "formatting normal" normal normal_expected;
  let nested =
    String.concat "\n"
      [ "START"; "BEGIN"; "BEGIN"; "VALUE = 1;"; "END"; "END"; "TERM"; "" ]
  in
  let nested_expected =
    String.concat "\n"
      [
        "START";
        "  BEGIN";
        "    BEGIN";
        "      VALUE = 1;";
        "    END";
        "  END";
        "TERM";
        "";
      ]
  in
  expect_formatting_snapshot "formatting nested" nested nested_expected;
  let macro_heavy =
    String.concat "\n"
      [
        "START";
        "DEFINE ADD(A,B) \"$A+$B\";";
        "BEGIN";
        "VALUE = ADD(1,2);";
        "% macro comment stays put %";
        "END";
        "TERM";
        "";
      ]
  in
  let macro_expected =
    String.concat "\n"
      [
        "START";
        "DEFINE ADD(A,B) \"$A+$B\";";
        "  BEGIN";
        "    VALUE = ADD(1,2);";
        "% macro comment stays put %";
        "  END";
        "TERM";
        "";
      ]
  in
  expect_formatting_snapshot "formatting macro-heavy" macro_heavy macro_expected;
  let range_input =
    String.concat "\n" [ "START"; "BEGIN"; "VALUE = 1;"; "END"; "TERM"; "" ]
  in
  let range =
    {
      T.Range.start = { T.Position.line = 2; character = 0 };
      end_ = { T.Position.line = 3; character = 0 };
    }
  in
  let range_expected =
    String.concat "\n" [ "START"; "BEGIN"; "    VALUE = 1;"; "END"; "TERM"; "" ]
  in
  expect_formatting_snapshot "formatting range" ~range range_input
    range_expected;
  let broken = String.concat "\n" [ "START"; "BEGIN"; "ITEM VALUE U 6;"; "" ] in
  expect_int "formatting broken returns no edits"
    (List.length (formatting_edits_for_text broken))
    0;
  let edits = formatting_edits_for_text normal in
  match Lib.Lsp_response.yojson_of_text_edits edits with
  | `List (_ :: _) -> ()
  | got ->
      failf "formatting JSON shape: expected non-empty TextEdit list, got %s"
        (Yojson.Safe.to_string got)

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
        match d.message with `String s -> s | `MarkupContent mc -> mc.value)
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
      expect_true "drop_ast drops expanded tokens"
        (syntax.expanded_tokens = None)
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
  let full =
    expect_some "semantic tokens full"
      (Lib.Workspace.semantic_tokens_full_for ws ~uri)
  in
  let previous_result_id =
    match full.T.SemanticTokens.resultId with
    | Some id -> id
    | None -> failf "semantic tokens full response missing resultId"
  in
  (match
     Lib.Workspace.semantic_tokens_delta_for ws ~uri ~previous_result_id
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
  match
    Lib.Workspace.semantic_tokens_delta_for ws ~uri
      ~previous_result_id:"missing-result-id"
  with
  | Some (`SemanticTokens _) -> ()
  | Some (`SemanticTokensDelta _) ->
      failf "semantic delta should fall back on stale previousResultId"
  | None -> failf "semantic delta stale fallback returned None"

let large_deferred_text () =
  let b = Buffer.create (280 * 1024) in
  Buffer.add_string b "START\n";
  Buffer.add_string b "DEF PROC MAIN RENT;\n";
  Buffer.add_string b "BEGIN\n";
  Buffer.add_string b "  ITEM TARGETTOKEN U 6;\n";
  Buffer.add_string b "  TARGETTOKEN = 1;\n";
  let i = ref 0 in
  while Buffer.length b < 270 * 1024 do
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
  let uri_a =
    expect_some "catchup A URI" (Lib.Uri_path.docuri_of_path path_a)
  in
  let uri_b =
    expect_some "catchup B URI" (Lib.Uri_path.docuri_of_path path_b)
  in
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
      (Lib.Workspace_hover.hover_for ws ~uri ~pos)
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
  expect_int "semantic range did not update parse rev"
    doc.Lib.Document.parse_rev 0;
  expect_int "semantic range sync parse counter remains zero"
    (Lib.Perf_log.counter_value "sync_full_parse_from_semantic_tokens_range")
    0

let test_open_doc_dequeue_preempts_unopened_high () =
  let root = mk_temp_dir "jovial-open-first" in
  let unopened_path = Filename.concat root "TARGET.j73" in
  let open_path = Filename.concat root "OPEN.j73" in
  write_text unopened_path target_text;
  write_text open_path syntax_text;
  let ws = Lib.Workspace_state.create () in
  ignore (Lib.Workspace_state.set_source_files ws [ unopened_path; open_path ]);
  Lib.Workspace_state.enqueue_bg_path ws ~lane:Lib.Workspace_foundation.LaneOpen
    ~reason_group:"test_unopened" ~high:true unopened_path;
  let uri =
    expect_some "open-first URI" (Lib.Uri_path.docuri_of_path open_path)
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~force_provisional:true
    ~inline_catch_up:false ~uri ~file:(Some open_path) ~text:syntax_text;
  expect_true "open parse is pending"
    (Lib.Workspace_state.has_pending_open_parse_work ws);
  match
    Lib.Workspace_state.dequeue_bg_path ws
      ~mode:Lib.Workspace_foundation.BgTickInteractive ~allow_high_large:true
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
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ws ~force_provisional:true
    ~inline_catch_up:false ~uri ~file:(Some path) ~text:syntax_text;
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
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ws ~force_provisional:true
    ~inline_catch_up:false ~uri ~file:(Some path) ~text:syntax_text;
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

let has_execute_command (command : string) (json : Yojson.Safe.t) : bool =
  match json with
  | `Assoc root -> (
      match List.assoc_opt "capabilities" root with
      | Some (`Assoc caps) -> (
          match List.assoc_opt "executeCommandProvider" caps with
          | Some (`Assoc provider) -> (
              match List.assoc_opt "commands" provider with
              | Some (`List commands) -> List.mem (`String command) commands
              | _ -> false)
          | _ -> false)
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
    (has_capability "diagnosticProvider" with_pull);
  expect_true "CodeLens capability advertised when enabled"
    (has_capability "codeLensProvider" with_pull);
  expect_true "inlay hint capability advertised when enabled"
    (has_capability "inlayHintProvider" with_pull);
  expect_true "formatting capability advertised when enabled"
    (has_capability "documentFormattingProvider" with_pull);
  expect_true "range formatting capability advertised when enabled"
    (has_capability "documentRangeFormattingProvider" with_pull);
  expect_true "debugScheduler command advertised"
    (has_execute_command "jovial.debugScheduler" with_pull);
  expect_true "debugMemory command advertised"
    (has_execute_command "jovial.debugMemory" with_pull);
  expect_true "explainSymbolResolution command advertised"
    (has_execute_command "jovial.explainSymbolResolution" with_pull);
  let without_code_lens =
    Lib.Lsp_response.initialize_result_json
      ~feature_flags:{ flags with code_lens = false }
      ~diagnostic_pull:true
  in
  expect_false "CodeLens capability omitted when disabled"
    (has_capability "codeLensProvider" without_code_lens);
  let without_inlay_hints =
    Lib.Lsp_response.initialize_result_json
      ~feature_flags:{ flags with inlay_hints = false }
      ~diagnostic_pull:true
  in
  expect_false "inlay hint capability omitted when disabled"
    (has_capability "inlayHintProvider" without_inlay_hints);
  let without_formatting =
    Lib.Lsp_response.initialize_result_json
      ~feature_flags:{ flags with formatting = false }
      ~diagnostic_pull:true
  in
  expect_false "formatting capability omitted when disabled"
    (has_capability "documentFormattingProvider" without_formatting);
  expect_false "range formatting capability omitted when disabled"
    (has_capability "documentRangeFormattingProvider" without_formatting)

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
  match messages with
  | [ first; second ] ->
      expect_json_version "first diagnostic version" first (Some 2);
      expect_json_empty_diagnostics "first diagnostic payload" first;
      expect_json_version "second diagnostic version" second (Some 3);
      expect_json_empty_diagnostics "second diagnostic payload" second
  | _ -> failf "unexpected diagnostic message count"

let test_unversioned_closed_diagnostic_publish () =
  let uri =
    expect_some "closed diagnostic publish URI"
      (Lib.Uri_path.docuri_of_string "file:///closed-publish.j73")
  in
  let path =
    Filename.concat (mk_temp_dir "jovial-closed-publish") "out.jsonl"
  in
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

let test_literals_keywords_and_strings_are_not_hover_targets () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "non-symbol hover URI"
      (Lib.Uri_path.docuri_of_string "file:///non-symbol-hover.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 6;";
        "  ITEM TEXT C 20;";
        "  VALUE = 4;";
        "  TEXT = 'NOT_A_SYMBOL';";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let expect_no_nav_surface label ~needle ~rel =
    let pos = position_of_offset text (find_nth text ~needle ~nth:0 + rel) in
    expect_none (label ^ " hover") (Lib.Workspace_hover.hover_for ws ~uri ~pos);
    expect_int (label ^ " definitions")
      (List.length
         (Lib.Workspace_definition.definition_locations_for ws ~uri ~pos))
      0;
    expect_int (label ^ " references")
      (List.length
         (Lib.Workspace_references.references_locations_for ws ~uri ~pos
            ~include_decl:true))
      0
  in
  expect_no_nav_surface "numeric literal" ~needle:"4" ~rel:0;
  expect_no_nav_surface "ordinary string literal" ~needle:"NOT_A_SYMBOL" ~rel:1;
  expect_no_nav_surface "keyword" ~needle:"BEGIN" ~rel:1;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing
    "string literal ignored by unresolved diagnostics"
    ~needle:"Undefined identifier \"NOT_A_SYMBOL\"" diags;
  expect_no_diagnostic_containing
    "numeric literal ignored by unresolved diagnostics"
    ~needle:"Undefined identifier \"4\"" diags

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

let test_multiline_selected_import_excludes_unselected_symbol () =
  let root = mk_temp_dir "jovial-selected-import-multiline" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text =
    String.concat "\n"
      [
        "START COMPOOL DATA;";
        "DEF ITEM CLOCK U 10;";
        "DEF ITEM EXTRA U 10;";
        "DEF ITEM HIDDEN U 10;";
        "TERM";
        "";
      ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL 'DATA'";
        "  CLOCK,";
        "  EXTRA;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  CLOCK = 1;";
        "  HIDDEN = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  ignore (open_workspace_text ws root "DATA.j73" compool_text);
  let importer_uri, _ = open_workspace_text ws root "MAIN.j73" importer_text in
  let diags = revalidated_diagnostics ws importer_uri in
  expect_no_diagnostic_containing
    "multiline selected import keeps CLOCK visible"
    ~needle:"Undefined item \"CLOCK\"" diags;
  expect_diagnostic_containing
    "multiline selected import excludes unselected HIDDEN"
    ~needle:"not in the selective import list" diags

let test_text_compool_import_scanner_preserves_selected_order () =
  let text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('TINYCOMP');";
        "!COMPOOL 'MAINCOMP'";
        "    ARRAY,";
        "    TYPED'ONE;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "text compool scanner URI"
      (Lib.Uri_path.docuri_of_string "file:///text-compool-scanner.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let dirs = Lib.Workspace_imports.extract_compool_import_dirs doc in
  let find_dir name =
    dirs
    |> List.find_opt (fun (d : Lib.Workspace_imports.compool_import_dir) ->
        normalize_name d.compool = name)
  in
  let selected_keys (d : Lib.Workspace_imports.compool_import_dir) =
    List.map fst d.selected
  in
  let expect_string_list name got want =
    if got <> want then
      failf "%s: expected [%s], got [%s]" name (String.concat "; " want)
        (String.concat "; " got)
  in
  let tiny = expect_some "TINYCOMP import directive" (find_dir "TINYCOMP") in
  let main = expect_some "MAINCOMP import directive" (find_dir "MAINCOMP") in
  expect_string_list "TINYCOMP all import has no selected list"
    (selected_keys tiny) [];
  expect_string_list "MAINCOMP selected import order" (selected_keys main)
    [ "ARRAY"; "TYPED'ONE" ]

let test_selected_import_hint_does_not_suppress_unselected_symbol () =
  let root = mk_temp_dir "jovial-selected-import-hint" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text =
    String.concat "\n"
      [
        "START COMPOOL DATA;";
        "DEF ITEM CLOCK U 10;";
        "DEF ITEM HIDDEN U 10;";
        "TERM";
        "";
      ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL 'DATA'";
        "  CLOCK;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  CLOCK = 1;";
        "  HIDDEN = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  ignore (open_workspace_text ws root "DATA.j73" compool_text);
  let importer_uri, _ = open_workspace_text ws root "MAIN.j73" importer_text in
  ignore (Lib.Workspace_semantics.symbol_hint_index ws);
  let diags = revalidated_diagnostics ws importer_uri in
  expect_no_diagnostic_containing "selected hint keeps CLOCK visible"
    ~needle:"Undefined item \"CLOCK\"" diags;
  expect_diagnostic_containing
    "selected import hint does not suppress unselected HIDDEN" ~needle:"HIDDEN"
    diags

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

let diagnostic_authority_doc text =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "diagnostic authority URI"
      (Lib.Uri_path.docuri_of_string "file:///diagnostic-authority.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  (ws, doc)

let test_diagnostic_authority_local_unresolved_error () =
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
  let ws, doc = diagnostic_authority_doc text in
  let internal =
    Lib.Workspace_semantics.validate_semantics_with_authority ws doc
  in
  expect_authority_diagnostic "local unresolved authority"
    ~authority:Lib.Workspace_diagnostic_authority.LocalSemanticAuthoritative
    ~needle:"Undefined item \"MISSING\"" internal;
  let public = Lib.Workspace_semantics.validate_semantics ws doc in
  expect_diagnostic_containing "local unresolved public error"
    ~needle:"Undefined item \"MISSING\"" public;
  expect_true "local unresolved severity remains error"
    (List.exists
       (fun (diag : T.Diagnostic.t) ->
         diagnostics_contain [ diag ] ~needle:"Undefined item \"MISSING\""
         && diag.severity = Some T.DiagnosticSeverity.Error)
       public)

let diagnostic_authority_import_workspace ~ready =
  let ws = Lib.Workspace_state.create () in
  let compool_uri =
    expect_some "diagnostic authority compool URI"
      (Lib.Uri_path.docuri_of_string "file:///diagnostic-authority-data.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:compool_uri ~file:None
    ~text:(String.concat "\n" [ "START COMPOOL DATA;"; "TERM"; "" ]);
  ws.Lib.Workspace_foundation.startup_diag_hover_ready_ms <-
    (if ready then Some 0.0 else None);
  let importer_uri =
    expect_some "diagnostic authority importer URI"
      (Lib.Uri_path.docuri_of_string "file:///diagnostic-authority-main.j73")
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('DATA');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  CLOCK'DATA = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let doc =
    Lib.Document.make ~uri:importer_uri ~file:None ~text:importer_text
  in
  (ws, doc)

let test_diagnostic_authority_imported_warmup_provisional () =
  let ws, doc = diagnostic_authority_import_workspace ~ready:false in
  let internal =
    Lib.Workspace_semantics.validate_semantics_with_authority ws doc
  in
  expect_authority_diagnostic "imported warmup provisional authority"
    ~authority:Lib.Workspace_diagnostic_authority.CrossModuleProvisional
    ~needle:"CLOCK'DATA" internal;
  expect_authority_diagnostic "imported warmup message"
    ~authority:Lib.Workspace_diagnostic_authority.CrossModuleProvisional
    ~needle:Lib.Workspace_diagnostic_authority.provisional_warmup_message
    internal;
  let public = Lib.Workspace_semantics.validate_semantics ws doc in
  expect_no_diagnostic_containing "imported warmup suppresses public hard error"
    ~needle:"CLOCK'DATA" public

let test_diagnostic_authority_imported_ready_error () =
  let ws, doc = diagnostic_authority_import_workspace ~ready:true in
  let internal =
    Lib.Workspace_semantics.validate_semantics_with_authority ws doc
  in
  expect_authority_diagnostic "imported ready authoritative authority"
    ~authority:Lib.Workspace_diagnostic_authority.CrossModuleAuthoritative
    ~needle:"Undefined item \"CLOCK'DATA\"" internal;
  let public = Lib.Workspace_semantics.validate_semantics ws doc in
  expect_diagnostic_containing "imported ready public error"
    ~needle:"Undefined item \"CLOCK'DATA\"" public

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
        ~needle:("Undefined identifier \"" ^ name ^ "\"")
        diags;
      expect_no_diagnostic_containing
        ("imported fixture item is not undefined: " ^ name)
        ~needle:("Undefined item \"" ^ name ^ "\"")
        diags;
      expect_no_diagnostic_containing
        ("imported fixture procedure is not undefined: " ^ name)
        ~needle:("Undefined procedure \"" ^ name ^ "\"")
        diags)
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

let test_status_size_and_colon_call_syntax_parse () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "repo example syntax URI"
      (Lib.Uri_path.docuri_of_string "file:///repo-example-syntax.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM MODE STATUS 3 (V(ONE), V(TWO));";
        "DEF PROC TARGET RENT (INPUT'NUM: OUT'NUM);";
        "BEGIN";
        "  ITEM INPUT'NUM U 6;";
        "  ITEM OUT'NUM U 6;";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM NUMBER U 6;";
        "  MODE = V(ONE);";
        "  TARGET(NUMBER: NUMBER);";
        "  TARGET(: NUMBER);";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "STATUS size list parses"
    ~needle:"Parse error" diags;
  expect_no_diagnostic_containing "by-ref colon actual list parses"
    ~needle:"unexpected token COLON" diags

let test_unresolved_all_import_does_not_hide_random_local () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "missing import undefined URI"
      (Lib.Uri_path.docuri_of_string
         "file:///missing-import-random-local-error.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('MISSINGCOMP');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  NOT'DEFINED'VAR = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing
    "random local unresolved remains an error despite unresolved all-import"
    ~needle:"Undefined item \"NOT'DEFINED'VAR\"" diags

let test_proc_call_argument_count_and_type_diagnostics () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "procedure call argument diagnostics URI"
      (Lib.Uri_path.docuri_of_string
         "file:///proc-call-argument-diagnostics.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "PROC ACCEPT(A, B);";
        "BEGIN";
        "  ITEM A U 6;";
        "  ITEM B B 4;";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM TEXT C 1;";
        "  ITEM BITS B 4;";
        "  ACCEPT(TEXT: BITS);";
        "  ACCEPT(TEXT: BITS, TEXT);";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "procedure argument type mismatch is diagnosed"
    ~needle:
      "Argument type mismatch in call to \"ACCEPT\": expected integer, \
       provided character"
    diags;
  expect_diagnostic_containing "procedure argument count mismatch is diagnosed"
    ~needle:
      "Argument count mismatch in call to \"ACCEPT\": expected 2, provided 3"
    diags

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
    Lib.Lsp_server.Private_for_tests.reorder_raw_messages_for_dispatch items
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
        (String.concat "; " queue_want)
        (String.concat "; " queue_got)
    else (
      expect_true "interactive request preempts workspace symbol"
        (Lib.Lsp_server.Private_for_tests.incoming_preempts_active_method
           ~active:"workspace/symbol" ~incoming:"textDocument/hover");
      expect_false "workspace symbol does not preempt hover"
        (Lib.Lsp_server.Private_for_tests.incoming_preempts_active_method
           ~active:"textDocument/hover" ~incoming:"workspace/symbol"))

let expect_same_json_set name to_json got want =
  let keys xs =
    xs
    |> List.map (fun x -> Yojson.Safe.to_string (to_json x))
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
    Lib.Workspace.references_locations_stream ws ~uri ~pos ~include_decl:true
      ~emit:(fun xs -> batches := xs :: !batches)
  in
  let full =
    Lib.Workspace.references_locations_for ws ~uri ~pos ~include_decl:true
  in
  expect_true "reference stream emitted at least one batch" (!batches <> []);
  expect_same_json_set "reference stream matches full result"
    T.Location.yojson_of_t streamed full;
  expect_same_json_set "reference emitted batches match full result"
    T.Location.yojson_of_t
    (List.concat (List.rev !batches))
    full

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
    Lib.Workspace.workspace_symbols_stream ws ~query:"MAIN" ~emit:(fun xs ->
        batches := xs :: !batches)
  in
  let full = Lib.Workspace.workspace_symbols_for ws ~query:"MAIN" in
  expect_true "workspace symbol stream emitted at least one batch"
    (!batches <> []);
  expect_same_json_set "workspace symbol stream matches full result"
    T.SymbolInformation.yojson_of_t streamed full;
  expect_same_json_set "workspace symbol emitted batches match full result"
    T.SymbolInformation.yojson_of_t
    (List.concat (List.rev !batches))
    full

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

let test_workspace_readiness_helpers () =
  let open Lib.Workspace_readiness in
  expect_string "readiness lexical label" (label LexicalOnly) "lexicalOnly";
  expect_string "readiness skeleton label" (label SkeletonReady) "skeletonReady";
  expect_string "reason label"
    (reason_label WorkspaceIndexWarming)
    "workspaceIndexWarming";
  expect_true "readiness min"
    (min WorkspaceSemanticReady LocalAstReady = LocalAstReady);
  expect_true "readiness max"
    (max SkeletonReady CrossModuleSemanticReady = CrossModuleSemanticReady);
  let mapped =
    authoritative ~readiness:LocalAstReady 41 |> map (fun n -> n + 1)
  in
  expect_int "readiness map value" mapped.value 42;
  expect_true "mapped result remains authoritative" (is_authoritative mapped);
  let provisional_result =
    provisional ~reasons:[ ParseStale ] ~readiness:LexicalOnly "pending"
  in
  expect_false "provisional is not authoritative"
    (is_authoritative provisional_result);
  expect_string "authority label"
    (authority_label Authoritative)
    "authoritative"

let test_workspace_navigation_compat_boundary () =
  let lib_dir = find_existing_relative_path [ "apps"; "lsp-server"; "lib" ] in
  let is_compat_file path =
    match Filename.basename path with
    | "workspace_navigation.ml" | "workspace_navigation.mli" -> true
    | _ -> false
  in
  let offenders =
    source_files_under lib_dir
    |> List.filter (fun path ->
        (not (is_compat_file path))
        && has_identifier_token ~token:"Workspace_navigation" (read_text path))
  in
  match offenders with
  | [] -> ()
  | _ ->
      failf
        "Workspace_navigation is a compatibility facade; internal code should \
         depend on focused Workspace_* feature modules or Workspace_query. \
         Offenders: %s"
        (String.concat "; " offenders)

let test_workspace_query_local_symbol_lookup () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 6;";
        "  VALUE = 7;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "query local symbol URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-query-local-symbol.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"VALUE" ~nth:1 in
  let pos = position_of_offset text (use_off + 1) in
  let ctx =
    expect_some "query local symbol context"
      (Lib.Workspace_query.context ws ~uri ~pos)
  in
  let sym_result = Lib.Workspace_query.symbol_at_position ctx in
  expect_true "query local symbol readiness"
    (sym_result.readiness = Lib.Workspace_readiness.LocalAstReady);
  expect_true "query local symbol authority"
    (sym_result.authority = Lib.Workspace_readiness.Authoritative);
  let sym = expect_some "query local symbol value" sym_result.value in
  expect_string "query local symbol key" sym.key "VALUE";
  let def = expect_some "query local symbol def" sym.def in
  expect_string "query local symbol def key" def.key "VALUE";
  let direct_defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri ~pos
  in
  let query_defs = Lib.Workspace_query.definition_at_position ctx in
  expect_true "query local definition readiness"
    (query_defs.readiness = Lib.Workspace_readiness.LocalAstReady);
  expect_true "query local definition authority"
    (query_defs.authority = Lib.Workspace_readiness.Authoritative);
  expect_same_json_set "query local definition matches feature module"
    T.Location.yojson_of_t query_defs.value direct_defs

let test_workspace_query_facade_preserves_feature_results () =
  let ws = Lib.Workspace_state.create () in
  let public_ws = Lib.Workspace.create () in
  let uri =
    expect_some "query facade URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-query-facade.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text:syntax_text;
  Lib.Workspace.open_doc public_ws ~uri ~file:None ~text:syntax_text;
  let use_off = find_nth syntax_text ~needle:"TWELVE" ~nth:1 in
  let pos = position_of_offset syntax_text (use_off + 1) in
  let ctx =
    expect_some "query facade context"
      (Lib.Workspace_query.context ws ~uri ~pos)
  in
  let direct_defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri ~pos
  in
  let query_defs = (Lib.Workspace_query.definition_at_position ctx).value in
  let public_defs =
    Lib.Workspace.definition_locations_for public_ws ~uri ~pos
  in
  expect_same_json_set "query definition matches feature module"
    T.Location.yojson_of_t query_defs direct_defs;
  expect_same_json_set "public definition uses query facade"
    T.Location.yojson_of_t public_defs direct_defs;
  let direct_refs =
    Lib.Workspace_references.references_locations_for ws ~uri ~pos
      ~include_decl:true
  in
  let query_refs =
    (Lib.Workspace_query.references_at_position ~include_declaration:true ctx)
      .value
  in
  let public_refs =
    Lib.Workspace.references_locations_for public_ws ~uri ~pos
      ~include_decl:true
  in
  expect_same_json_set "query references match feature module"
    T.Location.yojson_of_t query_refs direct_refs;
  expect_same_json_set "public references use query facade"
    T.Location.yojson_of_t public_refs direct_refs;
  let direct_hover = Lib.Workspace_hover.hover_for ws ~uri ~pos in
  let query_hover = (Lib.Workspace_query.hover_at_position ctx).value in
  let public_hover = Lib.Workspace.hover_for public_ws ~uri ~pos in
  let hover_json = function
    | None -> `Null
    | Some hover -> T.Hover.yojson_of_t hover
  in
  expect_string "query hover matches feature module"
    (Yojson.Safe.to_string (hover_json query_hover))
    (Yojson.Safe.to_string (hover_json direct_hover));
  expect_string "public hover uses query facade"
    (Yojson.Safe.to_string (hover_json public_hover))
    (Yojson.Safe.to_string (hover_json direct_hover));
  let sym =
    expect_some "query symbol at position"
      (Lib.Workspace_query.symbol_at_position ctx).value
  in
  expect_string "query symbol key" sym.key "TWELVE";
  let report = Lib.Workspace_reporting.debug_report_for ws ~uri ~max_tokens:8 in
  let query = expect_json_field "debug report query" "query" report in
  ignore (expect_json_field "debug query readiness" "documentReadiness" query);
  ignore (expect_json_field "debug query counters" "counters" query)

let completion_labels (items : T.CompletionItem.t list) : string list =
  items
  |> List.filter_map (fun item ->
      match T.CompletionItem.yojson_of_t item with
      | `Assoc fields -> (
          match List.assoc_opt "label" fields with
          | Some (`String label) -> Some label
          | _ -> None)
      | _ -> None)

let test_workspace_feature_split_completion_smoke () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 6;";
        "  VA";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let public_ws = Lib.Workspace.create () in
  let uri =
    expect_some "feature split completion URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-feature-completion.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  Lib.Workspace.open_doc public_ws ~uri ~file:None ~text;
  let va_off = find_nth text ~needle:"VA" ~nth:1 in
  let pos = position_of_offset text (va_off + 2) in
  let through_public = Lib.Workspace.completion_items_for public_ws ~uri ~pos in
  let direct = Lib.Workspace_completion.completion_items_for ws ~uri ~pos in
  expect_same_json_set "public completion matches feature module"
    T.CompletionItem.yojson_of_t through_public direct;
  expect_true "completion includes local symbol"
    (completion_labels through_public |> List.exists (( = ) "VALUE"))

let perf_metric_calls (name : string) : int =
  match Lib.Workspace_foundation.Perf_stats.snapshot_json () with
  | `Assoc fields -> (
      match List.assoc_opt "metrics" fields with
      | Some (`List metrics) ->
          metrics
          |> List.find_map (function
            | `Assoc metric_fields -> (
                match
                  ( List.assoc_opt "name" metric_fields,
                    List.assoc_opt "calls" metric_fields )
                with
                | Some (`String n), Some (`Int calls) when n = name ->
                    Some calls
                | _ -> None)
            | _ -> None)
          |> Option.value ~default:0
      | _ -> 0)
  | _ -> 0

let test_large_stale_change_keeps_navigation () =
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM TARGET U 6;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  TARGET = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let settings =
    {
      (Lib.Workspace_settings.from_env ()) with
      workspace_profile_mode = Lib.Workspace_settings.ProfileModeLarge;
      allow_slow_query_fallback = false;
    }
  in
  let ws = Lib.Workspace_state.create ~settings () in
  let uri =
    expect_some "large stale nav URI"
      (Lib.Uri_path.docuri_of_string "file:///large-stale-nav.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ~inline_catch_up:false ws
    ~uri ~file:None ~text;
  let open Lib.Workspace_foundation in
  ws.quick_nav_index_total <- 100;
  ws.quick_nav_index_done <- 64;
  Queue.add "pending.j73" ws.quick_nav_pending_paths;
  Hashtbl.replace ws.quick_nav_pending_set "pending.j73" true;
  let use_pos =
    position_of_offset text (find_nth text ~needle:"TARGET = 1" ~nth:0 + 1)
  in
  let current_doc =
    expect_some "large current doc" (Hashtbl.find_opt ws.docs uri)
  in
  ignore
    (Lib.Workspace_nav_lookup.nav_for_doc_cached ws (Hashtbl.create 1)
       current_doc);
  (match Lib.Workspace_query.definition_locations_for ws ~uri ~pos:use_pos with
  | loc :: _ ->
      expect_int "large ready definition before edit" loc.range.start.line 1
  | [] -> failf "large ready definition before edit returned no locations");
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:(range_of_substring text ~needle:"END" ~nth:0)
      ~text:"  % typed while checker catches up %\nEND" ()
  in
  let stale_doc =
    Lib.Document.apply_changes_no_reparse ~lsp_version:2 ~changes:[ change ]
      current_doc
  in
  Lib.Workspace_background.store_doc_fast ws uri stale_doc;
  let stale_doc =
    expect_some "large stale doc after edit" (Hashtbl.find_opt ws.docs uri)
  in
  expect_true "large didChange-style edit defers parse"
    (stale_doc.Lib.Document.parse_rev <> stale_doc.Lib.Document.rev);
  Lib.Workspace_foundation.Perf_stats.reset ();
  ignore
    (Lib.Workspace_nav_lookup.nav_for_doc_cached ws (Hashtbl.create 1) stale_doc);
  expect_true "stale semantic nav snapshot is reused"
    (perf_metric_calls "nav.stale_snapshot_reused" > 0);
  match Lib.Workspace_query.definition_locations_for ws ~uri ~pos:use_pos with
  | loc :: _ ->
      expect_int "large ready definition after stale edit" loc.range.start.line
        1
  | [] -> failf "large ready definition after stale edit returned no locations"

let test_large_deferred_change_publishes_provisional_start_diag () =
  let text = large_deferred_text () in
  let root = mk_temp_dir "jovial-large-live-edit-diag" in
  let path = Filename.concat root "LARGE.j73" in
  write_text path text;
  let uri =
    expect_some "large live-edit URI" (Lib.Uri_path.docuri_of_path path)
  in
  let settings =
    {
      (Lib.Workspace_settings.from_env ()) with
      large_file_threshold_bytes = 32 * 1024;
      full_semantic_tokens_max_bytes = 64 * 1024;
      huge_file_threshold_bytes = 4 * 1024 * 1024;
    }
  in
  let ws = Lib.Workspace_state.create ~settings () in
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ~inline_catch_up:false ws
    ~uri ~file:(Some path) ~text;
  let insert_broken =
    T.TextDocumentContentChangeEvent.create
      ~range:
        {
          T.Range.start = { T.Position.line = 0; character = 0 };
          end_ = { T.Position.line = 0; character = 0 };
        }
      ~text:"BROKEN LIVE EDIT\n" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri
    ~changes:[ insert_broken ];
  let broken_doc =
    expect_some "large doc after broken live edit"
      (Hashtbl.find_opt ws.docs uri)
  in
  expect_true "large live edit remains deferred"
    (broken_doc.Lib.Document.parse_rev <> broken_doc.Lib.Document.rev);
  expect_diagnostic_containing "large provisional live edit diagnostic"
    ~needle:"Expected START before source text"
    (Lib.Document.diagnostics broken_doc);
  expect_false "large diagnostic catch-up does not force sync parse"
    (Lib.Workspace_background.finish_open_doc_now_if_needed ws ~uri);
  let remove_broken =
    T.TextDocumentContentChangeEvent.create
      ~range:
        {
          T.Range.start = { T.Position.line = 0; character = 0 };
          end_ = { T.Position.line = 1; character = 0 };
        }
      ~text:"" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:3 ws ~uri
    ~changes:[ remove_broken ];
  let restored_doc =
    expect_some "large doc after restoring live edit"
      (Hashtbl.find_opt ws.docs uri)
  in
  expect_no_diagnostic_containing "large provisional diagnostic clears"
    ~needle:"Expected START before source text"
    (Lib.Document.diagnostics restored_doc)

let test_workspace_budget_tiny_stops_scans () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM VALUE U 6;";
        "  VALUE = 1;";
        "  VALUE = VALUE + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "budget tiny URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-budget-tiny.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"VALUE" ~nth:1 in
  let pos = position_of_offset text (use_off + 1) in
  let ref_budget = Lib.Workspace_budget.start ~ws ~soft_budget_ms:0 in
  let refs =
    Lib.Workspace_references.references_locations_for ~budget:ref_budget ws ~uri
      ~pos ~include_decl:true
  in
  expect_int "tiny reference budget returns partial empty result"
    (List.length refs) 0;
  expect_true "tiny reference budget records stop reason"
    (Lib.Workspace_budget.reason_if_stopped ref_budget
    = Some Lib.Workspace_readiness.SoftBudgetExceeded);
  let doc =
    expect_some "budget tiny doc"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs uri)
  in
  let sem_budget = Lib.Workspace_budget.start ~ws ~soft_budget_ms:0 in
  let toks =
    Lib.Workspace_reporting.semantic_tokens_for_doc ~budget:sem_budget ws doc
      ~range:None
  in
  expect_int "tiny semantic budget returns partial empty tokens"
    (List.length toks) 0;
  expect_true "tiny semantic budget records stop reason"
    (Lib.Workspace_budget.reason_if_stopped sem_budget
    = Some Lib.Workspace_readiness.SoftBudgetExceeded);
  expect_true "budget stop counter increments"
    (perf_metric_calls "budget.soft_budget_exceeded" > 0)

let test_hover_body_cache_reuses_symbol_markdown () =
  Lib.Workspace_foundation.Perf_stats.reset ();
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "hover cache URI"
      (Lib.Uri_path.docuri_of_string "file:///hover-cache.j73")
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
  let use_off = find_nth text ~needle:"HELPER" ~nth:1 in
  let pos = position_of_offset text (use_off + 1) in
  ignore
    (expect_some "first hover cache result"
       (Lib.Workspace_hover.hover_for ws ~uri ~pos));
  expect_true "first hover records cache miss"
    (perf_metric_calls "hover.body_cache_miss" > 0);
  ignore
    (expect_some "second hover cache result"
       (Lib.Workspace_hover.hover_for ws ~uri ~pos));
  expect_true "second hover records cache hit"
    (perf_metric_calls "hover.body_cache_hit" > 0)

let semantic_graph_text =
  String.concat "\n"
    [
      "START";
      "DEFINE LIMIT \"10\";";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  TYPE REC TABLE U 1;";
      "  ITEM COUNT U 6;";
      "  TABLE TAB (1) U 6;";
      "DONE: COUNT = LIMIT;";
      "END";
      "TERM";
      "";
    ]

let semantic_graph_fingerprint graph =
  Lib.Semantic_graph.symbols graph
  |> List.map (fun (sym : Lib.Semantic_graph.symbol) ->
      Printf.sprintf "%s:%s:%d" sym.key
        (Lib.Workspace_symbol_metadata.symbol_kind_label sym.kind)
        (Lib.Symbol_id.to_int sym.id))
  |> List.sort String.compare |> String.concat "|"

let ast_loc_of_offsets (text : string) ~(start_off : int) ~(end_off : int) :
    Lib.Ast.Loc.t =
  let start_pos = position_of_offset text start_off in
  let end_pos = position_of_offset text end_off in
  let mk (pos : T.Position.t) offset : Lib.Ast.Loc.pos =
    { Lib.Ast.Loc.line = pos.line + 1; col = pos.character; offset }
  in
  Lib.Ast.Loc.make_no_file ~start_pos:(mk start_pos start_off)
    ~end_pos:(mk end_pos end_off)

let ast_loc_of_substring (text : string) ~(needle : string) ~(nth : int) :
    Lib.Ast.Loc.t =
  let start_off = find_nth text ~needle ~nth in
  ast_loc_of_offsets text ~start_off ~end_off:(start_off + String.length needle)

let first_diagnostic_containing label ~(needle : string)
    (diags : T.Diagnostic.t list) : T.Diagnostic.t =
  match
    List.find_opt
      (fun diag ->
        string_contains ~needle (List.hd (diagnostic_texts [ diag ])))
      diags
  with
  | Some diag -> diag
  | None ->
      failf "%s: expected diagnostic containing %S, got [%s]" label needle
        (String.concat "; " (diagnostic_texts diags))

let test_code_action_add_compool_for_type_hint_after_start () =
  let root = mk_temp_dir "jovial-code-action-import" in
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
  let diag =
    first_diagnostic_containing "missing type import hint" ~needle:"COUNTER"
      diags
  in
  let actions =
    Lib.Workspace_code_actions.code_actions_for ws ~uri:importer_uri
      ~range:diag.range
  in
  let add_action =
    find_code_action_containing "add compool action"
      ~needle:"Add !COMPOOL ('DATA')" actions
  in
  ignore
    (find_code_action_containing "open compool action"
       ~needle:"Open COMPOOL DATA" actions);
  let edit =
    expect_single_workspace_text_edit "add compool action" add_action
  in
  expect_string "add compool edit text" edit.newText "\n!COMPOOL ('DATA');";
  expect_int "add compool insert line" edit.range.start.line 0;
  expect_int "add compool insert character" edit.range.start.character 5;
  let applied = apply_text_edit "add compool edit" importer_text edit in
  expect_true "add compool edit produces directive-like text"
    (string_contains ~needle:"START\n!COMPOOL ('DATA');\nDEF PROC MAIN RENT;"
       applied)

let manual_import_hint_doc ws uri text ~(symbol : string)
    ~(compools : string list) : T.Diagnostic.t =
  let doc0 = Lib.Document.make ~uri ~file:None ~text in
  let loc = ast_loc_of_substring text ~needle:symbol ~nth:0 in
  let diag =
    Lib.Workspace_imports.diag_missing_import_hint ~selected_imported:false ~loc
      ~kind:"Type" ~symbol ~compools
  in
  let doc = Lib.Document.with_import_diags [ diag ] doc0 in
  Hashtbl.replace ws.Lib.Workspace_foundation.docs uri doc;
  diag

let test_code_action_add_compool_near_existing_import () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "code action existing import URI"
      (Lib.Uri_path.docuri_of_string "file:///code-action-existing-import.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('OLD');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM LOCAL COUNTER;";
        "END";
        "TERM";
        "";
      ]
  in
  let diag =
    manual_import_hint_doc ws uri text ~symbol:"COUNTER" ~compools:[ "DATA" ]
  in
  let actions =
    Lib.Workspace_code_actions.code_actions_for ws ~uri ~range:diag.range
  in
  let add_action =
    find_code_action_containing "add compool near existing import"
      ~needle:"Add !COMPOOL ('DATA')" actions
  in
  let edit =
    expect_single_workspace_text_edit "add compool near existing import"
      add_action
  in
  expect_int "add compool near existing line" edit.range.start.line 1;
  expect_string "add compool near existing text" edit.newText
    "\n!COMPOOL ('DATA');";
  let applied = apply_text_edit "add compool near existing import" text edit in
  expect_true "add compool placed after existing import"
    (string_contains
       ~needle:"!COMPOOL ('OLD');\n!COMPOOL ('DATA');\nDEF PROC MAIN RENT;"
       applied)

let test_code_action_add_compool_avoids_duplicate_import () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "code action duplicate import URI"
      (Lib.Uri_path.docuri_of_string "file:///code-action-duplicate-import.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('DATA');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM LOCAL COUNTER;";
        "END";
        "TERM";
        "";
      ]
  in
  let diag =
    manual_import_hint_doc ws uri text ~symbol:"COUNTER" ~compools:[ "DATA" ]
  in
  let actions =
    Lib.Workspace_code_actions.code_actions_for ws ~uri ~range:diag.range
  in
  expect_no_code_action_containing "duplicate import is not offered"
    ~needle:"Add !COMPOOL ('DATA')" actions;
  ignore
    (find_code_action_containing "duplicate import search fallback"
       ~needle:"Search workspace for COUNTER" actions)

let code_lens_title (lens : T.CodeLens.t) : string =
  match lens.command with Some cmd -> cmd.title | None -> ""

let find_code_lens_containing label ~(needle : string)
    (lenses : T.CodeLens.t list) : T.CodeLens.t =
  match
    List.find_opt
      (fun lens -> string_contains ~needle (code_lens_title lens))
      lenses
  with
  | Some lens -> lens
  | None ->
      failf "%s: expected CodeLens containing %S, got [%s]" label needle
        (String.concat "; " (List.map code_lens_title lenses))

let expect_code_lens_json_shape label (lens : T.CodeLens.t) : unit =
  let json = T.CodeLens.yojson_of_t lens in
  ignore (expect_json_field label "range" json);
  ignore (expect_json_field label "command" json);
  ignore (expect_json_field label "data" json)

let code_lens_data_json label (lens : T.CodeLens.t) : Yojson.Safe.t =
  expect_json_field label "data" (T.CodeLens.yojson_of_t lens)

let expect_json_string_field name key json want =
  match expect_json_field name key json with
  | `String got when got = want -> ()
  | `String got -> failf "%s: expected %s=%S, got %S" name key want got
  | got ->
      failf "%s: expected JSON string field %S, got %s" name key
        (Yojson.Safe.to_string got)

let expect_json_bool_field name key json want =
  match expect_json_field name key json with
  | `Bool got when got = want -> ()
  | `Bool got -> failf "%s: expected %s=%b, got %b" name key want got
  | got ->
      failf "%s: expected JSON bool field %S, got %s" name key
        (Yojson.Safe.to_string got)

let expect_json_string_list_contains name key json ~(needle : string) =
  match expect_json_field name key json with
  | `List values ->
      let strings =
        values |> List.filter_map (function `String s -> Some s | _ -> None)
      in
      if not (List.exists (string_contains ~needle) strings) then
        failf "%s: expected %S in [%s]" name needle (String.concat "; " strings)
  | got ->
      failf "%s: expected JSON string list field %S, got %s" name key
        (Yojson.Safe.to_string got)

let inlay_hint_label (hint : T.InlayHint.t) : string =
  match hint.label with
  | `String label -> label
  | `List parts ->
      parts
      |> List.map (fun (part : T.InlayHintLabelPart.t) -> part.value)
      |> String.concat ""

let find_inlay_hint_containing label ~(needle : string)
    (hints : T.InlayHint.t list) : T.InlayHint.t =
  match
    List.find_opt
      (fun hint -> string_contains ~needle (inlay_hint_label hint))
      hints
  with
  | Some hint -> hint
  | None ->
      failf "%s: expected inlay hint containing %S, got [%s]" label needle
        (String.concat "; " (List.map inlay_hint_label hints))

let expect_no_inlay_hint_containing label ~(needle : string)
    (hints : T.InlayHint.t list) : unit =
  match
    List.find_opt
      (fun hint -> string_contains ~needle (inlay_hint_label hint))
      hints
  with
  | None -> ()
  | Some hint ->
      failf "%s: unexpected inlay hint %S" label (inlay_hint_label hint)

let expect_inlay_hint_json_shape label (hint : T.InlayHint.t) : unit =
  let json = T.InlayHint.yojson_of_t hint in
  ignore (expect_json_field label "position" json);
  ignore (expect_json_field label "label" json);
  ignore (expect_json_field label "kind" json);
  ignore (expect_json_field label "data" json)

let test_codelens_simple_counts () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "CodeLens simple URI"
      (Lib.Uri_path.docuri_of_string "file:///codelens-simple.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEFINE LIMIT \"10\";";
        "ITEM COUNT U 6;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  COUNT = LIMIT;";
        "  COUNT = COUNT;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let lenses = Lib.Workspace_codelens.code_lenses_for ws ~uri in
  let proc_lens =
    find_code_lens_containing "procedure CodeLens"
      ~needle:"~0 references | ~0 REFs | ~1 implementation | show impact" lenses
  in
  let item_lens =
    find_code_lens_containing "item CodeLens" ~needle:"~3 references" lenses
  in
  let define_lens =
    find_code_lens_containing "define CodeLens"
      ~needle:"~1 macro use | expansion impact" lenses
  in
  List.iter
    (fun (label, lens) -> expect_code_lens_json_shape label lens)
    [
      ("procedure CodeLens JSON", proc_lens);
      ("item CodeLens JSON", item_lens);
      ("define CodeLens JSON", define_lens);
    ];
  let item_data = code_lens_data_json "item CodeLens data" item_lens in
  expect_json_string_field "item CodeLens confidence" "confidence" item_data
    "provisional";
  expect_json_bool_field "item CodeLens provisional" "provisional" item_data
    true

let test_codelens_authoritative_title () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "CodeLens authoritative URI"
      (Lib.Uri_path.docuri_of_string "file:///codelens-authoritative.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM COUNT U 6;";
        "BEGIN";
        "  COUNT = 1;";
        "  COUNT = COUNT;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  ws.Lib.Workspace_foundation.startup_fully_nav_ready_ms <- Some 0.0;
  let lenses = Lib.Workspace_codelens.code_lenses_for ws ~uri in
  let lens =
    find_code_lens_containing "authoritative CodeLens" ~needle:"3 references"
      lenses
  in
  expect_false "authoritative CodeLens has no provisional marker"
    (string_contains ~needle:"~3 references" (code_lens_title lens));
  let data = code_lens_data_json "authoritative CodeLens data" lens in
  expect_json_string_field "authoritative CodeLens confidence" "confidence" data
    "exact-authoritative";
  expect_json_bool_field "authoritative CodeLens authoritative" "authoritative"
    data true

let test_codelens_compool_importer_count () =
  let root = mk_temp_dir "jovial-codelens-compool" in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  let compool_text = String.concat "\n" [ "START COMPOOL DATA;"; "TERM"; "" ] in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('DATA');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  let compool_uri, _ = open_workspace_text ws root "DATA.j73" compool_text in
  ignore (open_workspace_text ws root "MAIN.j73" importer_text);
  let lenses = Lib.Workspace_codelens.code_lenses_for ws ~uri:compool_uri in
  let lens =
    find_code_lens_containing "compool importer CodeLens"
      ~needle:"1 importer | show import graph" lenses
  in
  expect_code_lens_json_shape "compool importer CodeLens JSON" lens;
  let data = code_lens_data_json "compool importer CodeLens data" lens in
  expect_json_string_field "compool importer CodeLens confidence" "confidence"
    data "exact-authoritative";
  expect_json_bool_field "compool importer CodeLens authoritative"
    "authoritative" data true

let test_explain_symbol_resolution_json_shape () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "explain shape URI"
      (Lib.Uri_path.docuri_of_string "file:///explain-shape.j73")
  in
  let text =
    String.concat "\n"
      [ "START"; "ITEM COUNT U 6;"; "BEGIN"; "  COUNT = 1;"; "END"; "TERM"; "" ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let use_off = find_nth text ~needle:"COUNT" ~nth:1 in
  let pos = position_of_offset text (use_off + 1) in
  let explanation =
    Lib.Workspace_query.explain_symbol_resolution_json ws ~uri ~pos
  in
  expect_json_string_field "explain symbol name" "symbolName" explanation
    "COUNT";
  expect_json_string_field "explain symbol key" "symbolKey" explanation "COUNT";
  ignore (expect_json_field "explain position" "position" explanation);
  ignore (expect_json_field "explain readiness" "readiness" explanation);
  ignore (expect_json_field "explain authority" "authority" explanation);
  ignore (expect_json_field "explain symbol" "symbol" explanation);
  ignore (expect_json_field "explain target" "targetDefinition" explanation);
  expect_json_bool_field "explain fallback scan" "fallbackScanUsed" explanation
    false;
  expect_json_string_list_contains "explain path" "resolutionPath" explanation
    ~needle:"symbolAtPosition"

let test_explain_symbol_resolution_fallback_path_visible () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "explain fallback URI"
      (Lib.Uri_path.docuri_of_string "file:///explain-fallback.j73")
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
  let use_off = find_nth text ~needle:"MISSING" ~nth:0 in
  let pos = position_of_offset text (use_off + 1) in
  let explanation =
    Lib.Workspace_query.explain_symbol_resolution_json ws ~uri ~pos
  in
  expect_json_bool_field "explain fallback path" "fallbackPathUsed" explanation
    true;
  expect_json_string_field "explain fallback cache source" "cacheSource"
    explanation "fallback-word";
  expect_json_string_list_contains "explain fallback path step" "resolutionPath"
    explanation ~needle:"fallback"

let test_inlay_hints_proc_args_and_type_details () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "inlay hint URI"
      (Lib.Uri_path.docuri_of_string "file:///inlay-hints-simple.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "% FIND(CODE,TAB) in comment %";
        "DEF PROC FIND(CODE,TAB) RENT;";
        "BEGIN";
        "END";
        "ITEM CODE U 6;";
        "TABLE TAB(1:2,1:3) U 6;";
        "ITEM TEXT C 30;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  FIND(CODE,TAB);";
        "  TEXT = 'FIND(CODE,TAB)';";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let full_range =
    {
      T.Range.start = { T.Position.line = 0; character = 0 };
      end_ = { T.Position.line = 999; character = 0 };
    }
  in
  let hints =
    Lib.Workspace_inlay_hints.inlay_hints_for ws ~uri ~range:full_range
  in
  let code_hint =
    find_inlay_hint_containing "procedure call CODE parameter" ~needle:"CODE:"
      hints
  in
  ignore
    (find_inlay_hint_containing "procedure call TAB parameter" ~needle:"TAB:"
       hints);
  ignore
    (find_inlay_hint_containing "ITEM type detail"
       ~needle:"unsigned integer, 6 bits" hints);
  ignore
    (find_inlay_hint_containing "TABLE entry count" ~needle:"6 entries" hints);
  ignore
    (find_inlay_hint_containing "character type detail"
       ~needle:"character string, 30 characters" hints);
  let expected_code_pos =
    position_of_offset text
      (find_nth text ~needle:"  FIND(CODE,TAB);" ~nth:0
      + String.length "  FIND(")
  in
  expect_int "CODE hint line" code_hint.position.line expected_code_pos.line;
  expect_int "CODE hint character" code_hint.position.character
    expected_code_pos.character;
  expect_inlay_hint_json_shape "CODE inlay hint JSON" code_hint;

  let param_count =
    hints
    |> List.filter (fun hint ->
        let label = inlay_hint_label hint in
        label = "CODE:" || label = "TAB:")
    |> List.length
  in
  expect_int "strings and comments do not produce call-argument hints"
    param_count 2;

  let item_range = range_of_substring text ~needle:"ITEM CODE U 6;" ~nth:0 in
  let item_hints =
    Lib.Workspace_inlay_hints.inlay_hints_for ws ~uri ~range:item_range
  in
  ignore
    (find_inlay_hint_containing "range-limited type detail"
       ~needle:"unsigned integer, 6 bits" item_hints);
  expect_no_inlay_hint_containing "range-limited request omits call hints"
    ~needle:"CODE:" item_hints

let test_inlay_hints_range_uses_top_level_proc_sig_after_range () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "forward inlay hint URI"
      (Lib.Uri_path.docuri_of_string "file:///inlay-hints-forward.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM X U 6;";
        "  LATER(X);";
        "END";
        "";
        "DEF PROC LATER(ARG) RENT;";
        "BEGIN";
        "  ITEM ARG U 6;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let call_range = range_of_substring text ~needle:"  LATER(X);" ~nth:0 in
  let hints =
    Lib.Workspace_inlay_hints.inlay_hints_for ws ~uri ~range:call_range
  in
  ignore
    (find_inlay_hint_containing "range-limited forward procedure parameter"
       ~needle:"ARG:" hints);
  expect_no_inlay_hint_containing "range-limited forward request omits far body"
    ~needle:"unsigned integer, 6 bits" hints

let semantic_scope_at_substring graph uri text ~(needle : string) ~(nth : int) =
  let loc = ast_loc_of_substring text ~needle ~nth in
  expect_some ("scope at " ^ needle)
    (Lib.Semantic_graph.scope_at_loc graph ~uri loc)

let semantic_resolve_one graph scope_id name =
  match Lib.Semantic_graph.resolve_name graph scope_id name with
  | [ id ] ->
      expect_some
        ("resolved symbol " ^ name)
        (Lib.Semantic_graph.find_symbol graph id)
  | ids ->
      failf "resolve %s: expected one symbol, got %d" name (List.length ids)

let semantic_scope_kind graph scope_id =
  let scope =
    expect_some "semantic scope" (Lib.Semantic_graph.find_scope graph scope_id)
  in
  scope.Lib.Semantic_graph.kind

let test_semantic_graph_stable_ids_from_defs () =
  let module Metadata = Lib.Workspace_symbol_metadata in
  let uri =
    expect_some "semantic graph URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-semantic-graph.j73")
  in
  let doc1 = Lib.Document.make ~uri ~file:None ~text:semantic_graph_text in
  let doc2 = Lib.Document.make ~uri ~file:None ~text:semantic_graph_text in
  let graph1 = Lib.Semantic_graph.of_doc_defs doc1 in
  let graph2 = Lib.Semantic_graph.of_doc_defs doc2 in
  expect_string "semantic graph IDs are stable across builds"
    (semantic_graph_fingerprint graph1)
    (semantic_graph_fingerprint graph2);
  let defs = Lib.Workspace_nav_model.collect_doc_defs doc1 in
  let graph_from_defs = Lib.Semantic_graph.of_defs defs in
  expect_string "semantic graph of_defs matches document adapter"
    (semantic_graph_fingerprint graph_from_defs)
    (semantic_graph_fingerprint graph1);
  List.iter
    (fun (def : Lib.Workspace_nav_model.def) ->
      let sym =
        expect_some
          ("semantic graph finds def " ^ def.key)
          (Lib.Semantic_graph.find_symbol_by_def graph_from_defs def)
      in
      expect_string ("semantic graph maps key " ^ def.key) sym.key def.key)
    defs;
  let symbols = Lib.Semantic_graph.symbols graph1 in
  let has_symbol ~(key : string) ~(kind : Metadata.jovial_symbol_kind) =
    List.exists
      (fun (sym : Lib.Semantic_graph.symbol) ->
        sym.key = key && sym.kind = kind)
      symbols
  in
  expect_true "semantic graph represents procedure"
    (has_symbol ~key:"MAIN" ~kind:Metadata.JovialProcedure);
  expect_true "semantic graph represents item"
    (has_symbol ~key:"COUNT" ~kind:Metadata.JovialItem);
  expect_true "semantic graph represents table"
    (has_symbol ~key:"TAB" ~kind:Metadata.JovialTable);
  expect_true "semantic graph represents type"
    (has_symbol ~key:"REC" ~kind:Metadata.JovialType);
  expect_true "semantic graph represents label"
    (has_symbol ~key:"DONE" ~kind:Metadata.JovialLabel);
  expect_true "semantic graph represents define"
    (has_symbol ~key:"LIMIT" ~kind:Metadata.JovialDefine);
  expect_true "semantic graph records declaration references"
    (Lib.Semantic_graph.references graph1 <> [])

let test_jovial_type_model_display_and_compatibility () =
  let module A = Lib.Ast in
  let module JT = Lib.Jovial_type in
  let node v = A.node v in
  let ident name = node name in
  let int_expr value = node (A.ELit (A.LInt value)) in
  let tname name = node (A.TName (ident name)) in
  let sized name dims =
    node (A.TArray { elem = tname name; dims = List.map int_expr dims })
  in
  let pointer name = node (A.TPointer (tname name)) in
  let table =
    node (A.TArray { elem = sized "U" [ "6" ]; dims = [ int_expr "4" ] })
  in
  let block =
    node
      (A.TRecord
         [
           node
             { A.fname = ident "FIELD"; ftype = sized "U" [ "1" ]; fpos = None };
         ])
  in
  let proc =
    node
      (A.TFunc
         {
           params =
             [
               node
                 {
                   A.pname = ident "ARG";
                   pmode = A.In;
                   ptype = sized "U" [ "6" ];
                 };
             ];
           returns = Some (sized "S" [ "15" ]);
         })
  in
  let env = JT.empty_type_env () in
  let rich ty = JT.of_ast_type_expr env ty in
  let expect_display label want ty =
    expect_string label (JT.display (rich ty)) want
  in
  expect_display "jovial type display U" "U 16" (sized "U" [ "16" ]);
  expect_display "jovial type display S" "S 15" (sized "S" [ "15" ]);
  expect_display "jovial type display F" "F 30" (sized "F" [ "30" ]);
  expect_display "jovial type display A" "A 2,13" (sized "A" [ "2"; "13" ]);
  expect_display "jovial type display B" "B 10" (sized "B" [ "10" ]);
  expect_display "jovial type display C" "C 80" (sized "C" [ "80" ]);
  expect_display "jovial type display STATUS" "STATUS" (tname "STATUS");
  expect_display "jovial type display P TYPE" "P TYPE" (pointer "TYPE");
  expect_display "jovial type display TABLE" "TABLE(4) U 6" table;
  expect_display "jovial type display BLOCK" "BLOCK { FIELD U 1 }" block;
  expect_display "jovial type display PROC" "PROC(ARG: U 6) RETURNS S 15" proc;
  let u16 = rich (sized "U" [ "16" ]) in
  let f30 = rich (sized "F" [ "30" ]) in
  let b10 = rich (sized "B" [ "10" ]) in
  expect_true "jovial numeric compatibility" (JT.compatible ~lhs:u16 ~rhs:f30);
  expect_false "jovial bit numeric incompatibility"
    (JT.compatible ~lhs:b10 ~rhs:u16);
  expect_true "jovial numeric conversion required"
    (JT.conversion_required ~lhs:u16 ~rhs:f30);
  expect_string "jovial adapter to sem_ty"
    (Lib.Workspace_semantics.sem_ty_to_string
       (Lib.Workspace_semantics.sem_ty_of_jovial_type u16))
    "integer";
  expect_string "jovial adapter from sem_ty"
    (JT.display
       (Lib.Workspace_semantics.jovial_type_of_sem_ty
          (Lib.Workspace_semantics.TyPointer
             (Some Lib.Workspace_semantics.TyInt))))
    "P S"

let expect_ctf_int label want result =
  match result with
  | Lib.Jovial_compile_time.Known (Lib.Jovial_compile_time.CtfInt got) ->
      expect_string label (Int64.to_string got) (Int64.to_string want)
  | Lib.Jovial_compile_time.Known _ -> failf "%s: expected integer value" label
  | Lib.Jovial_compile_time.Unknown errors ->
      failf "%s: expected integer value, got unknown [%s]" label
        (String.concat "; "
           (List.map
              (fun (e : Lib.Jovial_compile_time.ctf_error) -> e.message)
              errors))

let test_compile_time_table_dimension_integer_expression () =
  let module A = Lib.Ast in
  let node v = A.node v in
  let lit n = node (A.ELit (A.LInt n)) in
  let expr =
    node
      (A.EBinop
         {
           op = A.BAdd;
           lhs = lit "2";
           rhs = node (A.EBinop { op = A.BMul; lhs = lit "3"; rhs = lit "4" });
         })
  in
  expect_ctf_int "compile-time table dimension expression" 14L
    (Lib.Jovial_compile_time.eval_expr expr);
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "compile-time table dimension URI"
      (Lib.Uri_path.docuri_of_string "file:///compile-time-table-dim.j73")
  in
  let text =
    String.concat "\n" [ "START"; "TABLE VALUES (2 + 3 * 4) U 6;"; "TERM"; "" ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "constant table dimension has no diagnostic"
    ~needle:"Compile-time constant expression required" diags

let test_compile_time_bit_char_size_constant_reference () =
  let module A = Lib.Ast in
  let node v = A.node v in
  let id name = node name in
  let env = Lib.Jovial_compile_time.empty_env () in
  Lib.Jovial_compile_time.add_constant env "N" (node (A.ELit (A.LInt "8")));
  let expr =
    node
      (A.EBinop
         {
           op = A.BAdd;
           lhs = node (A.EName (id "N"));
           rhs = node (A.ELit (A.LInt "1"));
         })
  in
  expect_ctf_int "compile-time constant item reference" 9L
    (Lib.Jovial_compile_time.eval_expr ~env expr);
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "compile-time type size URI"
      (Lib.Uri_path.docuri_of_string "file:///compile-time-type-size.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "CONSTANT ITEM N STATIC U 6 = 8;";
        "ITEM FLAGS B N;";
        "ITEM NAME C N;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing
    "constant bit/character sizes have no diagnostic"
    ~needle:"Compile-time constant expression required" diags

let test_compile_time_bad_nonconstant_required_context () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "compile-time nonconstant dimension URI"
      (Lib.Uri_path.docuri_of_string "file:///compile-time-nonconstant.j73")
  in
  let text =
    String.concat "\n"
      [ "START"; "ITEM N U 6;"; "TABLE VALUES (N + 1) U 6;"; "TERM"; "" ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing
    "nonconstant table dimension is diagnosed only in required context"
    ~needle:"Compile-time constant expression required" diags

let test_compile_time_unknown_does_not_overdiagnose () =
  let module A = Lib.Ast in
  let node v = A.node v in
  let id name = node name in
  let loc_builtin =
    node
      (A.ECall { callee = id "LOC"; args = [ node (A.EName (id "TARGET")) ] })
  in
  let loc_result = Lib.Jovial_compile_time.eval_expr loc_builtin in
  (match loc_result with
  | Lib.Jovial_compile_time.Unknown _ -> ()
  | Lib.Jovial_compile_time.Known _ ->
      failf
        "LOC should remain unknown until target layout rules are implemented");
  (match
     Lib.Jovial_compile_time.diagnostic_for_required loc_result loc_builtin.loc
   with
  | None -> ()
  | Some diag ->
      failf "unsupported LOC should not overdiagnose yet: %s"
        (String.concat "; " (diagnostic_texts [ diag ])));
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "compile-time unknown URI"
      (Lib.Uri_path.docuri_of_string "file:///compile-time-unknown.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE VALUES (WORDSIZE) U 6;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM TARGET U 6;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing
    "recognized but unimplemented compile-time formulas stay quiet"
    ~needle:"Compile-time constant expression required" diags

let test_implementation_config_compile_time_type_and_layout () =
  let module A = Lib.Ast in
  let node v = A.node v in
  let id name = node name in
  let config =
    {
      Lib.Implementation_config.empty with
      dialect = Some "test-target";
      bits_in_word = Some 24;
      bytes_in_word = Some 3;
      float_precision = Some 48;
      fixed_precision = Some 32;
      max_int_size = Some 24;
      max_bits = Some 4096;
      max_bytes = Some 512;
    }
  in
  let env = Lib.Jovial_compile_time.empty_env () in
  Lib.Jovial_compile_time.add_implementation_config env config;
  expect_ctf_int "configured BITSINWORD" 24L
    (Lib.Jovial_compile_time.eval_expr ~env (node (A.EName (id "BITSINWORD"))));
  expect_ctf_int "configured WORDSIZE" 24L
    (Lib.Jovial_compile_time.eval_expr ~env (node (A.EName (id "WORDSIZE"))));
  expect_ctf_int "configured BYTESINWORD" 3L
    (Lib.Jovial_compile_time.eval_expr ~env (node (A.EName (id "BYTESINWORD"))));
  expect_ctf_int "configured BYTESIZE" 8L
    (Lib.Jovial_compile_time.eval_expr ~env (node (A.EName (id "BYTESIZE"))));
  expect_ctf_int "configured FLOATPRECISION" 48L
    (Lib.Jovial_compile_time.eval_expr ~env
       (node (A.EName (id "FLOATPRECISION"))));
  expect_string "configured unsized float type display"
    (Lib.Jovial_type.display_with_config config
       (Lib.Jovial_type.Float { precision = None }))
    "F 48";
  expect_string "configured unsized fixed type display"
    (Lib.Jovial_type.display_with_config config
       (Lib.Jovial_type.Fixed { scale = None; fraction = None }))
    "A ?,32";
  let layout_config =
    Lib.Jovial_layout.config_of_implementation_config config
  in
  (match
     Lib.Jovial_layout.logical_size_of_type layout_config env
       (node (A.TName (id "F")))
   with
  | Lib.Jovial_layout.KnownBits bits ->
      expect_string "configured float layout size" (Int64.to_string bits) "48"
  | Lib.Jovial_layout.UnknownBits reason ->
      failf "configured float layout should be known, got %s" reason);
  match
    Lib.Jovial_layout.logical_size_of_type layout_config env
      (node
         (A.TArray
            {
              elem = node (A.TName (id "C"));
              dims = [ node (A.ELit (A.LInt "2")) ];
            }))
  with
  | Lib.Jovial_layout.KnownBits bits ->
      expect_string "configured character byte layout size"
        (Int64.to_string bits) "16"
  | Lib.Jovial_layout.UnknownBits reason ->
      failf "configured character layout should be known, got %s" reason

let test_system_subroutine_config_suppresses_unresolved_and_hovers () =
  let implementation_config =
    {
      Lib.Implementation_config.empty with
      dialect = Some "test-target";
      system_subroutines = [ "SYSIO" ];
    }
  in
  let settings =
    { (Lib.Workspace_settings.from_env ()) with implementation_config }
  in
  let ws = Lib.Workspace_state.create ~settings () in
  let uri =
    expect_some "system subroutine URI"
      (Lib.Uri_path.docuri_of_string "file:///system-subroutine.j73")
  in
  let text = String.concat "\n" [ "START"; "SYSIO(1);"; "TERM"; "" ] in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing
    "configured system subroutine suppresses unresolved diagnostic"
    ~needle:"Undefined procedure \"SYSIO\"" diags;
  let hover =
    expect_some "system subroutine hover"
      (Lib.Workspace_hover.hover_for ws ~uri
         ~pos:
           (position_of_offset text (find_nth text ~needle:"SYSIO" ~nth:0 + 1)))
  in
  let body = hover_markdown_text hover in
  expect_true "system routine hover classification"
    (string_contains ~needle:"JOVIAL system procedure" body);
  expect_true "system routine hover external kind"
    (string_contains ~needle:"| External kind | system/built-in |" body);
  expect_true "system routine hover dialect"
    (string_contains ~needle:"| Dialect/profile | `test-target` |" body)

let test_jovial_typecheck_integer_float_requires_conversion () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "jovial typecheck int-float URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-typecheck-int-float.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM IVALUE U 10;";
        "ITEM FVALUE F 30;";
        "IVALUE = FVALUE;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing
    "integer/float assignment asks for explicit conversion"
    ~needle:"Explicit conversion required" diags

let test_jovial_typecheck_explicit_conversion_suppresses_mismatch () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "jovial typecheck conversion URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-typecheck-conversion.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM IVALUE U 10;";
        "ITEM FVALUE F 30;";
        "IVALUE = (* U 10 *) FVALUE;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing
    "explicit conversion suppresses numeric mismatch warning"
    ~needle:"Explicit conversion required" diags

let test_jovial_typecheck_bit_length_propagation () =
  let module JT = Lib.Jovial_type in
  let b8 = JT.BitString { bits = Some 8 } in
  let result =
    Lib.Jovial_typecheck.binary_result ~op:Lib.Ast.BAnd ~lhs:b8 ~rhs:b8
      ~loc:Lib.Ast.Loc.none
  in
  expect_string "bit operator preserves simple length"
    (JT.display result.Lib.Jovial_typecheck.ty)
    "B 8";
  expect_int "bit operator has no issue for equal lengths"
    (List.length result.Lib.Jovial_typecheck.issues)
    0;
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "jovial typecheck bit op URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-typecheck-bit-op.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM MASK B 8;";
        "ITEM COUNT U 8;";
        "MASK = MASK AND COUNT;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "bit operator rejects mixed known operands"
    ~needle:"Invalid bit operator operand" diags

let test_jovial_typecheck_fixed_display_and_mixing () =
  let module JT = Lib.Jovial_type in
  let fixed = JT.Fixed { scale = Some 2; fraction = Some 13 } in
  let same = JT.Fixed { scale = Some 2; fraction = Some 13 } in
  expect_string "fixed type display remains rich" (JT.display fixed) "A 2,13";
  expect_true "fixed type shallow compatibility"
    (JT.compatible ~lhs:fixed ~rhs:same);
  let result =
    Lib.Jovial_typecheck.binary_result ~op:Lib.Ast.BAdd ~lhs:fixed
      ~rhs:(JT.Integer { kind = JT.Unsigned; bits = Some 10 })
      ~loc:Lib.Ast.Loc.none
  in
  expect_diagnostic_containing
    "fixed/integer mixing gets a conservative warning"
    ~needle:"Fixed/integer mixing"
    (List.map Lib.Jovial_typecheck.diagnostic result.Lib.Jovial_typecheck.issues)

let test_jovial_typecheck_typed_pointer_dereference () =
  let good_ws = Lib.Workspace_state.create () in
  let good_uri =
    expect_some "jovial typecheck pointer URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-typecheck-pointer.j73")
  in
  let good_text =
    String.concat "\n"
      [
        "START";
        "TYPE COUNTER U 10;";
        "ITEM PTR P COUNTER;";
        "ITEM OUT U 10;";
        "OUT = @ PTR;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc good_ws ~uri:good_uri ~file:None
    ~text:good_text;
  let good_diags = revalidated_diagnostics good_ws good_uri in
  expect_no_diagnostic_containing "typed pointer dereference is accepted"
    ~needle:"Invalid pointer dereference" good_diags;
  let bad_ws = Lib.Workspace_state.create () in
  let bad_uri =
    expect_some "jovial typecheck bad pointer URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-typecheck-bad-pointer.j73")
  in
  let bad_text =
    String.concat "\n"
      [
        "START";
        "ITEM VALUE U 10;";
        "ITEM OUT U 10;";
        "OUT = @ VALUE;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc bad_ws ~uri:bad_uri ~file:None
    ~text:bad_text;
  let bad_diags = revalidated_diagnostics bad_ws bad_uri in
  expect_diagnostic_containing "known non-pointer dereference is diagnosed"
    ~needle:"Invalid pointer dereference" bad_diags

let test_jovial_typecheck_undefined_named_type_diagnostic () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "jovial undefined named type URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-undefined-type.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM NEEDS'IMPORT E2E_REMOTE_COUNT;";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "unknown named type is diagnosed"
    ~needle:"Undefined type \"E2E_REMOTE_COUNT\"" diags;
  expect_no_diagnostic_containing "implicit procedure formals stay quiet"
    ~needle:"__implicit__" diags

let test_jovial_status_declaration_metadata () =
  let module Metadata = Lib.Workspace_symbol_metadata in
  let text =
    String.concat "\n"
      [
        "START";
        "TYPE MODE STATUS (V(OFF), V(ON));";
        "ITEM STATE MODE;";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "jovial status metadata URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-status-metadata.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let defs = Lib.Workspace_nav_model.collect_doc_defs doc in
  let mode =
    expect_some "MODE status type def"
      (List.find_opt
         (fun (def : Lib.Workspace_nav_model.def) -> def.key = "MODE")
         defs)
  in
  (match mode.metadata.Metadata.type_info with
  | Some info ->
      expect_string "STATUS type metadata display" info.Metadata.display
        "STATUS (OFF, ON)"
  | None -> failf "MODE status type should expose type metadata");
  let on_value =
    expect_some "ON status value def"
      (List.find_opt
         (fun (def : Lib.Workspace_nav_model.def) -> def.key = "ON")
         defs)
  in
  expect_true "ON is status constant metadata"
    (on_value.metadata.Metadata.jovial_kind = Metadata.JovialStatusConstant)

let test_jovial_status_assignment_valid_invalid () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "jovial status assignment URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-status-assignment.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "TYPE MODE STATUS (V(OFF), V(ON));";
        "ITEM STATE MODE;";
        "STATE = V(ON);";
        "STATE = V(BAD);";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "invalid status assignment is diagnosed"
    ~needle:"is not a member of expected status type" diags;
  expect_no_diagnostic_containing "valid status assignment remains quiet"
    ~needle:"V(ON) is not a member" diags

let test_jovial_status_hover_goto_references () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "jovial status nav URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-status-nav.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "TYPE MODE STATUS (V(OFF), V(ON));";
        "ITEM STATE MODE;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  STATE = V(ON);";
        "  STATE = V(ON);";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let use_pos =
    position_of_offset text (find_nth text ~needle:"ON" ~nth:1 + 1)
  in
  let defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri ~pos:use_pos
  in
  (match defs with
  | loc :: _ ->
      expect_int "status value goto declaration line" loc.range.start.line 1
  | [] -> failf "status value goto definition returned no locations");
  let hover =
    expect_some "status value hover"
      (Lib.Workspace_hover.hover_for ws ~uri ~pos:use_pos)
  in
  let body = hover_markdown_text hover in
  expect_true "status value hover reports status constant"
    (string_contains ~needle:"status constant" body);
  let decl_pos =
    position_of_offset text (find_nth text ~needle:"ON" ~nth:0 + 1)
  in
  let refs =
    Lib.Workspace_references.references_locations_for ws ~uri ~pos:decl_pos
      ~include_decl:true
  in
  let ref_lines =
    refs
    |> List.map (fun (loc : T.Location.t) -> loc.range.start.line)
    |> List.sort_uniq Int.compare
  in
  if ref_lines <> [ 1; 5; 6 ] then
    failf "status value references: expected lines [1,5,6], got [%s]"
      (String.concat "," (List.map string_of_int ref_lines))

let test_jovial_status_duplicate_and_ambiguous_diagnostics () =
  let dup_ws = Lib.Workspace_state.create () in
  let dup_uri =
    expect_some "jovial status duplicate URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-status-duplicate.j73")
  in
  let dup_text =
    String.concat "\n"
      [ "START"; "TYPE BAD STATUS (V(READY), V(READY));"; "TERM"; "" ]
  in
  Lib.Workspace_doc_lifecycle.open_doc dup_ws ~uri:dup_uri ~file:None
    ~text:dup_text;
  expect_diagnostic_containing "duplicate status value is diagnosed"
    ~needle:"Duplicate status value"
    (revalidated_diagnostics dup_ws dup_uri);
  let amb_ws = Lib.Workspace_state.create () in
  let amb_uri =
    expect_some "jovial status ambiguous URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-status-ambiguous.j73")
  in
  let amb_text =
    String.concat "\n"
      [
        "START";
        "TYPE LEFT_STATUS STATUS (V(SAME), V(LEFT_ONLY));";
        "TYPE RIGHT_STATUS STATUS (V(SAME), V(RIGHT_ONLY));";
        "ITEM FLAG B 1;";
        "FLAG = V(SAME);";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc amb_ws ~uri:amb_uri ~file:None
    ~text:amb_text;
  expect_diagnostic_containing "ambiguous status value is diagnosed"
    ~needle:"Ambiguous status constant"
    (revalidated_diagnostics amb_ws amb_uri)

let test_constant_table_parsing_and_metadata () =
  let module Metadata = Lib.Workspace_symbol_metadata in
  let text =
    String.concat "\n"
      [
        "START";
        "CONSTANT TABLE EMPTY(2);";
        "CONSTANT TABLE LOOKUP(2) U 6 - 0(1,2);";
        "CONSTANT TABLE RECORDS(2) BEGIN";
        "  ITEM FIELD U 6;";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "constant table parse URI"
      (Lib.Uri_path.docuri_of_string "file:///constant-table-parse.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let prog =
    match Lib.Document.current_parse doc with
    | Some { Lib.Document.parsed_ast = Some prog; _ } -> prog
    | _ -> failf "constant table forms should parse"
  in
  let count =
    prog
    |> List.filter (function
      | Lib.Ast.TopDecl
          { v = Lib.Ast.DConst { data_decl_kind = Lib.Ast.DataTable; _ }; _ } ->
          true
      | _ -> false)
    |> List.length
  in
  expect_int "all common constant table forms parse" count 3;
  let defs = Lib.Workspace_nav_model.collect_doc_defs doc in
  let lookup =
    expect_some "LOOKUP constant table def"
      (List.find_opt
         (fun (def : Lib.Workspace_nav_model.def) -> def.key = "LOOKUP")
         defs)
  in
  expect_true "LOOKUP metadata is constant table"
    (lookup.metadata.Metadata.jovial_kind = Metadata.JovialConstantTable);
  expect_true "LOOKUP metadata is readonly constant"
    lookup.metadata.Metadata.is_constant;
  match lookup.metadata.Metadata.type_info with
  | Some info ->
      expect_string "constant table type display" info.Metadata.display
        "TABLE(2) U 6"
  | None -> failf "constant table should expose type metadata"

let test_constant_table_hover_and_readonly_assignment () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "constant table hover URI"
      (Lib.Uri_path.docuri_of_string "file:///constant-table-hover.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "CONSTANT TABLE LOOKUP(2) U 6;";
        "ITEM OUT U 6;";
        "OUT = LOOKUP(1);";
        "LOOKUP = 3;";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let hover_pos =
    position_of_offset text (find_nth text ~needle:"LOOKUP" ~nth:0 + 1)
  in
  let hover =
    expect_some "constant table hover"
      (Lib.Workspace_hover.hover_for ws ~uri ~pos:hover_pos)
  in
  let body = hover_markdown_text hover in
  expect_true "constant table hover classification"
    (string_contains ~needle:"| Classification | constant table |" body);
  expect_true "constant table hover readonly"
    (string_contains ~needle:"| Readonly | yes |" body);
  expect_true "constant table hover type"
    (string_contains ~needle:"| Type | `TABLE(2) U 6` |" body);
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "constant table assignment is readonly"
    ~needle:"Cannot assign to constant table" diags

let test_constant_table_symbols () =
  let text =
    String.concat "\n" [ "START"; "CONSTANT TABLE LOOKUP(2) U 6;"; "TERM"; "" ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "constant table symbol URI"
      (Lib.Uri_path.docuri_of_string "file:///constant-table-symbols.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let doc_symbols = Lib.Workspace.document_symbols_for ws ~uri in
  let document_symbol =
    expect_some "constant table document symbol"
      (List.find_opt
         (fun symbol ->
           match name_of_symbol_response symbol with
           | Some name -> normalize_name name = "LOOKUP"
           | None -> false)
         doc_symbols)
  in
  let document_kind =
    match document_symbol with
    | `DocumentSymbol symbol -> (
        match T.DocumentSymbol.yojson_of_t symbol with
        | `Assoc fields -> List.assoc_opt "kind" fields
        | _ -> None)
    | `SymbolInformation symbol -> (
        match T.SymbolInformation.yojson_of_t symbol with
        | `Assoc fields -> List.assoc_opt "kind" fields
        | _ -> None)
  in
  (match document_kind with
  | Some (`Int kind) -> expect_int "constant table document symbol kind" kind 14
  | _ -> failf "constant table document symbol should expose Constant kind");
  let workspace_symbols =
    Lib.Workspace.workspace_symbols_for ws ~query:"LOOKUP"
  in
  let workspace_symbol =
    expect_some "constant table workspace symbol"
      (List.find_opt
         (fun symbol ->
           match T.SymbolInformation.yojson_of_t symbol with
           | `Assoc fields -> (
               match List.assoc_opt "name" fields with
               | Some (`String name) -> normalize_name name = "LOOKUP"
               | _ -> false)
           | _ -> false)
         workspace_symbols)
  in
  match T.SymbolInformation.yojson_of_t workspace_symbol with
  | `Assoc fields -> (
      match List.assoc_opt "kind" fields with
      | Some (`Int kind) ->
          expect_int "constant table workspace symbol kind" kind 14
      | _ -> failf "constant table workspace symbol should expose Constant kind"
      )
  | _ -> failf "constant table workspace symbol JSON should be object"

let test_inline_and_readonly_parsing () =
  let module A = Lib.Ast in
  let text =
    String.concat "\n"
      [
        "START";
        "READONLY ITEM LIMIT U 6;";
        "ITEM TRAILING U 6 READONLY;";
        "READONLY TABLE LOCKED(2) U 6;";
        "INLINE PROC FAST RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "inline readonly parse URI"
      (Lib.Uri_path.docuri_of_string "file:///inline-readonly-parse.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let prog =
    match Lib.Document.current_parse doc with
    | Some { Lib.Document.parsed_ast = Some prog; _ } -> prog
    | _ -> failf "INLINE and READONLY declarations should parse"
  in
  let readonly_names =
    prog
    |> List.filter_map (function
      | A.TopDecl { v = A.DVar { name; is_readonly = true; _ }; _ } ->
          Some name.v
      | _ -> None)
  in
  expect_true "prefix READONLY item is preserved"
    (List.mem "LIMIT" readonly_names);
  expect_true "trailing READONLY item attribute is preserved"
    (List.mem "TRAILING" readonly_names);
  expect_true "READONLY table is preserved" (List.mem "LOCKED" readonly_names);
  let fast =
    prog
    |> List.find_map (function
      | A.TopDecl { v = A.DProc p; _ } when p.v.name.v = "FAST" -> Some p
      | _ -> None)
    |> expect_some "FAST inline proc"
  in
  expect_true "INLINE proc flag is preserved" fast.v.is_inline;
  (match fast.v.use_attr with
  | A.UseRent -> ()
  | _ -> failf "INLINE proc should retain existing RENT attribute");
  let invalid_text =
    String.concat "\n" [ "START"; "INLINE ITEM BAD U 6;"; "TERM"; "" ]
  in
  let invalid_uri =
    expect_some "invalid inline target URI"
      (Lib.Uri_path.docuri_of_string "file:///inline-invalid-target.j73")
  in
  let invalid_doc =
    Lib.Document.make ~uri:invalid_uri ~file:None ~text:invalid_text
  in
  let invalid_payload =
    expect_some "invalid INLINE target parse"
      (Lib.Document.current_parse invalid_doc)
  in
  expect_diagnostic_containing "invalid INLINE target is diagnosed"
    ~needle:"INLINE applies only to PROC declarations"
    invalid_payload.Lib.Document.parsed_diags

let test_inline_readonly_hover_and_diagnostics () =
  let text =
    String.concat "\n"
      [
        "START";
        "READONLY ITEM LIMIT U 6;";
        "INLINE PROC FAST;";
        "BEGIN";
        "END";
        "LIMIT = 1;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "inline readonly hover URI"
      (Lib.Uri_path.docuri_of_string "file:///inline-readonly-hover.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let limit_hover =
    expect_some "readonly item hover"
      (Lib.Workspace_hover.hover_for ws ~uri
         ~pos:
           (position_of_offset text (find_nth text ~needle:"LIMIT" ~nth:0 + 1)))
  in
  let limit_body = hover_markdown_text limit_hover in
  expect_true "readonly hover flag"
    (string_contains ~needle:"| Readonly | yes |" limit_body);
  let fast_hover =
    expect_some "inline proc hover"
      (Lib.Workspace_hover.hover_for ws ~uri
         ~pos:
           (position_of_offset text (find_nth text ~needle:"FAST" ~nth:0 + 1)))
  in
  let fast_body = hover_markdown_text fast_hover in
  expect_true "inline hover flag"
    (string_contains ~needle:"| Inline | yes |" fast_body);
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "readonly assignment is diagnosed"
    ~needle:"Cannot assign to readonly data" diags

let test_jovial_type_metadata_display () =
  let text =
    String.concat "\n"
      [
        "START";
        "TYPE COUNTER U 10;";
        "ITEM UVAL U 16;";
        "ITEM AVAL A 2 13;";
        "ITEM PTR P COUNTER;";
        "TABLE TAB (4) U 6;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "jovial type metadata URI"
      (Lib.Uri_path.docuri_of_string "file:///jovial-type-metadata.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let defs = Lib.Workspace_nav_model.collect_doc_defs doc in
  let display_for key =
    let def =
      expect_some ("metadata def " ^ key)
        (List.find_opt
           (fun (def : Lib.Workspace_nav_model.def) -> def.key = key)
           defs)
    in
    match def.metadata.Lib.Workspace_symbol_metadata.type_info with
    | Some info -> info.Lib.Workspace_symbol_metadata.display
    | None -> failf "metadata def %s: expected type_info" key
  in
  expect_string "metadata U display" (display_for "UVAL") "U 16";
  expect_string "metadata A display" (display_for "AVAL") "A 2,13";
  expect_string "metadata pointer display" (display_for "PTR") "P COUNTER";
  expect_string "metadata table display" (display_for "TAB") "TABLE(4) U 6"

let test_semantic_scope_resolution_shadowing () =
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM TARGET U 6;";
        "ITEM VALUE U 6;";
        "DEF PROC OUTER(VALUE) RENT;";
        "BEGIN";
        "  ITEM TARGET U 6;";
        "  DEF PROC INNER RENT;";
        "  BEGIN";
        "    TARGET = 1;";
        "    VALUE = 2;";
        "  END";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "semantic scope shadow URI"
      (Lib.Uri_path.docuri_of_string "file:///semantic-scope-shadow.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let graph = Lib.Semantic_graph.of_doc_defs doc in
  let target_scope =
    semantic_scope_at_substring graph uri text ~needle:"TARGET = 1" ~nth:0
  in
  let target = semantic_resolve_one graph target_scope "TARGET" in
  expect_int "nested proc resolves outer local item"
    target.Lib.Semantic_graph.decl_loc.start_pos.line 6;
  let value_scope =
    semantic_scope_at_substring graph uri text ~needle:"VALUE = 2" ~nth:0
  in
  let value = semantic_resolve_one graph value_scope "VALUE" in
  expect_int "parameter shadows global item"
    value.Lib.Semantic_graph.decl_loc.start_pos.line 4;
  expect_true "scope_at_loc sees nested block or procedure"
    (match semantic_scope_kind graph target_scope with
    | Lib.Semantic_graph.BlockScope | Lib.Semantic_graph.ProcedureScope -> true
    | _ -> false)

let test_semantic_scope_duplicate_same_scope () =
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM VALUE U 6;";
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
  let uri =
    expect_some "semantic duplicate URI"
      (Lib.Uri_path.docuri_of_string "file:///semantic-scope-duplicates.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let graph = Lib.Semantic_graph.of_doc_defs doc in
  let duplicates =
    Lib.Semantic_graph.duplicate_declarations graph
    |> List.filter (fun d -> d.Lib.Semantic_graph.key = "VALUE")
  in
  expect_true "duplicate declaration in same scope is represented"
    (List.exists
       (fun d -> List.length d.Lib.Semantic_graph.declarations = 2)
       duplicates);
  let usage_scope =
    semantic_scope_at_substring graph uri text ~needle:"VALUE = 1" ~nth:0
  in
  let value = semantic_resolve_one graph usage_scope "VALUE" in
  expect_int "same name in nested scope shadows duplicate globals"
    value.Lib.Semantic_graph.decl_loc.start_pos.line 6

let test_semantic_scope_labels_are_procedure_local () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEF PROC FIRST RENT;";
        "BEGIN";
        "L1: GOTO L1;";
        "END";
        "DEF PROC SECOND RENT;";
        "BEGIN";
        "L1: GOTO L1;";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "semantic label URI"
      (Lib.Uri_path.docuri_of_string "file:///semantic-scope-labels.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let graph = Lib.Semantic_graph.of_doc_defs doc in
  let first_scope =
    semantic_scope_at_substring graph uri text ~needle:"GOTO L1" ~nth:0
  in
  let second_scope =
    semantic_scope_at_substring graph uri text ~needle:"GOTO L1" ~nth:1
  in
  let first_label = semantic_resolve_one graph first_scope "L1" in
  let second_label = semantic_resolve_one graph second_scope "L1" in
  expect_int "first goto resolves first procedure label"
    first_label.Lib.Semantic_graph.decl_loc.start_pos.line 4;
  expect_int "second goto resolves second procedure label"
    second_label.Lib.Semantic_graph.decl_loc.start_pos.line 8;
  expect_true "same label name in different procedures is allowed"
    (Lib.Semantic_graph.duplicate_declarations graph
    |> List.exists (fun d -> d.Lib.Semantic_graph.key = "L1")
    |> not)

let test_semantic_scope_table_and_block_field_ownership () =
  let module Metadata = Lib.Workspace_symbol_metadata in
  let text =
    String.concat "\n"
      [
        "START";
        "TYPE REC TABLE U 1;";
        "BEGIN";
        "  ITEM FIELD U 1;";
        "END";
        "BLOCK BLK;";
        "BEGIN";
        "  ITEM BFIELD U 1;";
        "END";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM FIELD U 6;";
        "  FIELD = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "semantic field URI"
      (Lib.Uri_path.docuri_of_string "file:///semantic-scope-fields.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let graph = Lib.Semantic_graph.of_doc_defs doc in
  let table_scope =
    semantic_scope_at_substring graph uri text ~needle:"FIELD U 1" ~nth:0
  in
  expect_true "table field has table scope"
    (semantic_scope_kind graph table_scope = Lib.Semantic_graph.TableScope);
  let table_field = semantic_resolve_one graph table_scope "FIELD" in
  expect_true "table field symbol kind"
    (table_field.Lib.Semantic_graph.kind = Metadata.JovialField);
  let block_scope =
    semantic_scope_at_substring graph uri text ~needle:"BFIELD U 1" ~nth:0
  in
  expect_true "block field has block scope"
    (semantic_scope_kind graph block_scope = Lib.Semantic_graph.BlockScope);
  let proc_scope =
    semantic_scope_at_substring graph uri text ~needle:"FIELD = 1" ~nth:0
  in
  let proc_field = semantic_resolve_one graph proc_scope "FIELD" in
  expect_int "procedure local FIELD is separate from table field"
    proc_field.Lib.Semantic_graph.decl_loc.start_pos.line 12

let test_semantic_scope_skeleton_fallback () =
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
        "LBL: NUM = ;";
        "";
      ]
  in
  let uri =
    expect_some "semantic skeleton URI"
      (Lib.Uri_path.docuri_of_string "file:///semantic-scope-skeleton.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let graph = Lib.Semantic_graph.of_doc_defs doc in
  let scope_id =
    semantic_scope_at_substring graph uri text ~needle:"NUM U 6" ~nth:0
  in
  let visible =
    Lib.Semantic_graph.visible_symbols graph scope_id
    |> List.filter_map (Lib.Semantic_graph.find_symbol graph)
    |> List.map (fun (sym : Lib.Semantic_graph.symbol) -> sym.key)
  in
  expect_true "broken file gets top-level skeleton scope"
    (match semantic_scope_kind graph scope_id with
    | Lib.Semantic_graph.ModuleScope | Lib.Semantic_graph.CompoolScope -> true
    | _ -> false);
  List.iter
    (fun key ->
      expect_true ("skeleton visible includes " ^ key) (List.mem key visible))
    [ "MAIN"; "NUM"; "TAB"; "MAC"; "LBL" ]

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
    expect_some "unresolved MISSING hover"
      (Lib.Workspace.hover_for ws ~uri ~pos)
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
  | loc :: _ ->
      expect_int "compool SHARED definition line" loc.range.start.line 3
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
  | loc :: _ ->
      expect_int "REF PROC definition jumps to DEF PROC" loc.range.start.line 1
  | [] -> failf "REF PROC definition returned no implementation");
  match Lib.Workspace.declaration_locations_for ws ~uri:ref_uri ~pos with
  | loc :: _ ->
      expect_int "REF PROC declaration stays on REF line" loc.range.start.line 1
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
      expect_some label (Lib.Workspace_hover.hover_for ws ~uri ~pos)
    in
    hover_markdown_text hover
  in
  let find_call_pos =
    let off = find_nth main_text ~needle:"FIND(CLOCK" ~nth:0 in
    position_of_offset main_text (off + 1)
  in
  (match
     Lib.Workspace_definition.definition_locations_for ws ~uri:main_uri
       ~pos:find_call_pos
   with
  | loc :: _ ->
      expect_int "FIND call definition goes to DEF PROC" loc.range.start.line 1
  | [] -> failf "FIND call definition returned no locations");
  (match
     Lib.Workspace_implementation.implementation_locations_for ws ~uri:main_uri
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
  expect_contains "DEF PROC hover classification" "JOVIAL external DEF function"
    def_hover;
  let clock_hover =
    hover_text "CLOCK hover" main_uri main_text "CLOCK COUNTER" 0
  in
  expect_contains "CLOCK is item" "JOVIAL item" clock_hover;
  expect_contains "CLOCK type display" "`COUNTER`" clock_hover;
  expect_contains "CLOCK resolved type" "`U 10`" clock_hover;
  let limit_hover = hover_text "LIMIT hover" main_uri main_text "LIMIT" 0 in
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
  expect_contains "F 30 built-in floating" "JOVIAL built-in floating type"
    f_hover

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

let test_table_field_owner_navigation_and_hover () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE FIRST(1), BEGIN";
        "  ITEM FIELD U 1;";
        "END;";
        "TABLE SECOND(1), BEGIN";
        "  ITEM FIELD U 2;";
        "END;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM OUT U 2;";
        "  OUT = FIRST(1).FIELD;";
        "  OUT = SECOND(1).FIELD;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "owned field URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-owned-fields.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let first_field_off =
    find_nth text ~needle:"FIRST(1).FIELD" ~nth:0 + String.length "FIRST(1)."
  in
  let second_field_off =
    find_nth text ~needle:"SECOND(1).FIELD" ~nth:0 + String.length "SECOND(1)."
  in
  let first_pos = position_of_offset text (first_field_off + 1) in
  let second_pos = position_of_offset text (second_field_off + 1) in
  (match Lib.Workspace.definition_locations_for ws ~uri ~pos:first_pos with
  | loc :: _ -> expect_int "FIRST.FIELD definition" loc.range.start.line 2
  | [] -> failf "FIRST.FIELD definition returned no locations");
  (match Lib.Workspace.definition_locations_for ws ~uri ~pos:second_pos with
  | loc :: _ -> expect_int "SECOND.FIELD definition" loc.range.start.line 5
  | [] -> failf "SECOND.FIELD definition returned no locations");
  let first_refs =
    Lib.Workspace.references_locations_for ws ~uri ~pos:first_pos
      ~include_decl:true
    |> List.map (fun (loc : T.Location.t) -> loc.range.start.line)
    |> List.sort_uniq Int.compare
  in
  expect_true "FIRST.FIELD references include declaration"
    (List.mem 2 first_refs);
  expect_true "FIRST.FIELD references include first use"
    (List.mem 10 first_refs);
  expect_false "FIRST.FIELD references exclude SECOND declaration"
    (List.mem 5 first_refs);
  expect_false "FIRST.FIELD references exclude SECOND use"
    (List.mem 11 first_refs);
  let hover =
    expect_some "FIRST.FIELD hover"
      (Lib.Workspace.hover_for ws ~uri ~pos:first_pos)
  in
  let body = hover_markdown_text hover in
  expect_true "field hover shows owner"
    (string_contains ~needle:"| Field owner | `FIRST` |" body);
  expect_true "field hover shows table context"
    (string_contains ~needle:"| Field context | table field |" body);
  expect_true "field hover shows field type"
    (string_contains ~needle:"| Type | `U 1` |" body)

let test_table_invalid_dimension_diagnostics () =
  let text =
    String.concat "\n"
      [ "START"; "TABLE ZERO(0) U 6;"; "TABLE REVERSED(5:3) U 6;"; "TERM"; "" ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "invalid table dimension URI"
      (Lib.Uri_path.docuri_of_string "file:///workspace-invalid-table-dim.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "zero dimension is invalid"
    ~needle:"entry count must be positive" diags;
  expect_diagnostic_containing "reversed range dimension is invalid"
    ~needle:"lower bound 5 is greater than upper bound 3" diags

let test_table_preset_mismatch_diagnostics () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE TOO_MANY(2) U 6 - 0(1,2,3);";
        "TABLE BAD_TYPE(2) U 6 - 0('BAD');";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "table preset mismatch URI"
      (Lib.Uri_path.docuri_of_string
         "file:///workspace-table-preset-mismatch.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "preset count overflow is diagnosed"
    ~needle:"Table preset has 3 positions but table capacity is 2" diags;
  expect_diagnostic_containing "preset type mismatch is diagnosed"
    ~needle:"cannot assign C 3 to U 6" diags

let test_table_preset_omitted_values_are_allowed () =
  let text =
    String.concat "\n" [ "START"; "TABLE OK(3) U 6 - 0(1,,3);"; "TERM"; "" ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "table omitted preset URI"
      (Lib.Uri_path.docuri_of_string
         "file:///workspace-table-preset-omitted.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "omitted preset values do not overflow"
    ~needle:"Table preset has" diags;
  expect_no_diagnostic_containing "omitted preset values do not type mismatch"
    ~needle:"cannot assign C 0 to U 6" diags

let test_specified_table_w_parsing_and_pos () =
  let module A = Lib.Ast in
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE PACKED(2) W 16, BEGIN";
        "  ITEM CODE U 6 POS(0,1);";
        "  ITEM FLAG B 1 POS(6,1);";
        "END;";
        "TERM";
        "";
      ]
  in
  let uri =
    expect_some "specified table parse URI"
      (Lib.Uri_path.docuri_of_string "file:///specified-table-parse.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text in
  let prog =
    match Lib.Document.current_parse doc with
    | Some { Lib.Document.parsed_ast = Some prog; _ } -> prog
    | _ -> failf "specified table should parse"
  in
  let dtype =
    prog
    |> List.find_map (function
      | A.TopDecl
          { v = A.DVar { name; dtype; data_decl_kind = A.DataTable; _ }; _ }
        when name.v = "PACKED" ->
          Some dtype
      | _ -> None)
    |> expect_some "PACKED specified table declaration"
  in
  match dtype.v with
  | A.TSpecifiedTable { elem = { v = A.TRecord fields; _ }; dims; kind } -> (
      expect_int "specified table dimension count" (List.length dims) 1;
      (match kind with
      | A.SpecTableW { v = A.ELit (A.LInt "16"); _ } -> ()
      | _ -> failf "specified table should record W entry size");
      let code =
        fields
        |> List.find_opt (fun (field : A.field_decl A.node) ->
            field.v.fname.v = "CODE")
        |> expect_some "CODE field"
      in
      match code.v.fpos with
      | Some
          {
            A.pos_start_bit = { v = A.ELit (A.LInt "0"); _ };
            pos_start_word = { v = A.ELit (A.LInt "1"); _ };
          } ->
          ()
      | _ -> failf "CODE field should record POS(0,1)")
  | _ -> failf "PACKED should be represented as TSpecifiedTable"

let test_specified_table_hover () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE PACKED(2) W 16, BEGIN";
        "  ITEM CODE U 6 POS(0,1);";
        "  ITEM FLAG B 1 POS(6,1);";
        "END;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "specified table hover URI"
      (Lib.Uri_path.docuri_of_string "file:///specified-table-hover.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let hover_pos =
    position_of_offset text (find_nth text ~needle:"PACKED" ~nth:0 + 1)
  in
  let hover =
    expect_some "specified table hover"
      (Lib.Workspace_hover.hover_for ws ~uri ~pos:hover_pos)
  in
  let body = hover_markdown_text hover in
  expect_true "specified table hover classification"
    (string_contains ~needle:"| Classification | table |" body);
  expect_true "specified table hover type"
    (string_contains ~needle:"SPECIFIED TABLE(2) W 16" body);
  expect_true "specified table hover flag"
    (string_contains ~needle:"| Specified table | yes |" body);
  expect_true "specified table hover entry size"
    (string_contains ~needle:"| Entry size | `16` |" body);
  expect_true "specified table hover field positions"
    (string_contains
       ~needle:"| Field positions | `CODE POS(0,1)`, `FLAG POS(6,1)` |" body)

let test_specified_table_diagnostics () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE BAD W 8, BEGIN";
        "  ITEM TOO_FAR U 1 POS(9,1);";
        "  ITEM MISSING U 1;";
        "  ITEM FIRST U 1 POS(1,1);";
        "  ITEM SECOND U 1 POS(1,1);";
        "  ITEM UNKNOWN_POS U 1 POS(UNKNOWN,1);";
        "END;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "specified table diagnostics URI"
      (Lib.Uri_path.docuri_of_string "file:///specified-table-diags.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "missing POS is diagnosed"
    ~needle:"requires a POS(startbit,startword) clause" diags;
  expect_diagnostic_containing "outside entry size is diagnosed"
    ~needle:"outside entry size" diags;
  expect_diagnostic_containing "duplicate POS is diagnosed"
    ~needle:"Duplicate specified table field POS" diags;
  expect_diagnostic_containing "invalid POS expression is diagnosed"
    ~needle:"Invalid POS expression" diags

let test_table_layout_non_overlap_specified_table () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE PACKED W 8, BEGIN";
        "  ITEM CODE U 4 POS(0,1);";
        "  ITEM FLAG B 2 POS(4,1);";
        "END;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "table layout non-overlap URI"
      (Lib.Uri_path.docuri_of_string "file:///table-layout-ok.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "non-overlap layout stays quiet"
    ~needle:"Table layout field" diags;
  let hover_pos =
    position_of_offset text (find_nth text ~needle:"PACKED" ~nth:0 + 1)
  in
  let hover =
    expect_some "table layout hover"
      (Lib.Workspace_hover.hover_for ws ~uri ~pos:hover_pos)
  in
  let body = hover_markdown_text hover in
  expect_true "layout hover entry size"
    (string_contains ~needle:"| Layout entry size | `8 bits` |" body);
  expect_true "layout hover field offsets"
    (string_contains
       ~needle:
         "| Field layout | `CODE @ bit 0 + 4 bits`, `FLAG @ bit 4 + 2 bits` |"
       body);
  let debug_ws = Lib.Workspace.create () in
  Lib.Workspace.open_doc debug_ws ~uri ~file:None ~text;
  let debug_json = Lib.Workspace.debug_report_for debug_ws ~uri ~max_tokens:0 in
  let debug_text = Yojson.Safe.to_string debug_json in
  expect_true "debug report exposes layout"
    (string_contains ~needle:"\"layout\"" debug_text);
  expect_true "debug report exposes entry size"
    (string_contains ~needle:"\"entrySizeBits\"" debug_text)

let test_table_layout_overlap_and_exceeds_diagnostics () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE BAD W 8, BEGIN";
        "  ITEM FIRST U 4 POS(0,1);";
        "  ITEM SECOND U 4 POS(2,1);";
        "  ITEM TOO_WIDE U 4 POS(6,1);";
        "END;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "table layout overlap URI"
      (Lib.Uri_path.docuri_of_string "file:///table-layout-overlap.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "exact field overlap is diagnosed"
    ~needle:"overlaps" diags;
  expect_diagnostic_containing "field exceeds entry size is diagnosed"
    ~needle:"exceeds entry size" diags

let test_table_layout_unknown_values_no_false_errors () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE UNKNOWN_LAYOUT W 8, BEGIN";
        "  ITEM FLEX U WIDTH POS(0,1);";
        "  ITEM OTHER U 4 POS(2,1);";
        "END;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "table layout unknown URI"
      (Lib.Uri_path.docuri_of_string "file:///table-layout-unknown.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_no_diagnostic_containing "unknown field size does not fake overlap"
    ~needle:"overlaps" diags;
  expect_no_diagnostic_containing "unknown field size does not fake exceed"
    ~needle:"exceeds entry size" diags

let overlay_fixture_text =
  String.concat "\n"
    [
      "START";
      "ITEM A U 6;";
      "ITEM B U 6;";
      "OVERLAY PACK POS(0) (A, B, SPACER(4));";
      "TERM";
      "";
    ]

let test_overlay_parsing () =
  let module A = Lib.Ast in
  let uri =
    expect_some "overlay parse URI"
      (Lib.Uri_path.docuri_of_string "file:///overlay-parse.j73")
  in
  let doc = Lib.Document.make ~uri ~file:None ~text:overlay_fixture_text in
  let prog =
    match Lib.Document.current_parse doc with
    | Some { Lib.Document.parsed_ast = Some prog; _ } -> prog
    | _ -> failf "simple OVERLAY declaration should parse"
  in
  let overlay =
    prog
    |> List.find_map (function
      | A.TopDecl { v = A.DOverlay overlay; _ }
        when overlay.overlay_name.v = "PACK" ->
          Some overlay
      | _ -> None)
    |> expect_some "PACK overlay declaration"
  in
  (match overlay.overlay_pos with
  | Some { v = A.ELit (A.LInt "0"); _ } -> ()
  | _ -> failf "OVERLAY POS(address) should be represented");
  let rec collect_items targets spacers (items : A.overlay_item A.node list) =
    List.fold_left
      (fun (targets, spacers) (item : A.overlay_item A.node) ->
        match item.v with
        | A.OverlayTarget id -> (id.v :: targets, spacers)
        | A.OverlaySpacer expr -> (targets, expr :: spacers)
        | A.OverlayGroup nested -> collect_items targets spacers nested)
      (targets, spacers) items
  in
  let targets, spacers = collect_items [] [] overlay.overlay_items in
  expect_true "overlay records target A" (List.mem "A" targets);
  expect_true "overlay records target B" (List.mem "B" targets);
  match spacers with
  | { A.v = A.ELit (A.LInt "4"); _ } :: _ -> ()
  | _ -> failf "OVERLAY spacer expression should be represented"

let test_overlay_hover_and_document_symbol () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "overlay hover URI"
      (Lib.Uri_path.docuri_of_string "file:///overlay-hover.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None
    ~text:overlay_fixture_text;
  let hover_pos =
    position_of_offset overlay_fixture_text
      (find_nth overlay_fixture_text ~needle:"PACK" ~nth:0 + 1)
  in
  let hover =
    expect_some "overlay hover"
      (Lib.Workspace_hover.hover_for ws ~uri ~pos:hover_pos)
  in
  let body = hover_markdown_text hover in
  expect_true "overlay hover classification"
    (string_contains ~needle:"| Classification | overlay declaration |" body);
  expect_true "overlay hover lists targets"
    (string_contains ~needle:"| Overlay targets | `A`, `B` |" body);
  expect_true "overlay hover shows POS"
    (string_contains ~needle:"| Overlay POS | `0` |" body);
  let symbol_ws = Lib.Workspace.create () in
  Lib.Workspace.open_doc symbol_ws ~uri ~file:None ~text:overlay_fixture_text;
  let doc_symbols = Lib.Workspace.document_symbols_for symbol_ws ~uri in
  let overlay_symbol =
    expect_some "overlay document symbol"
      (List.find_opt
         (fun symbol ->
           match name_of_symbol_response symbol with
           | Some name -> normalize_name name = "PACK"
           | None -> false)
         doc_symbols)
  in
  let detail =
    match overlay_symbol with
    | `DocumentSymbol symbol -> (
        match T.DocumentSymbol.yojson_of_t symbol with
        | `Assoc fields -> (
            match List.assoc_opt "detail" fields with
            | Some (`String detail) -> detail
            | _ -> "")
        | _ -> "")
    | `SymbolInformation symbol -> (
        match T.SymbolInformation.yojson_of_t symbol with
        | `Assoc fields -> (
            match List.assoc_opt "detail" fields with
            | Some (`String detail) -> detail
            | _ -> "")
        | _ -> "")
  in
  expect_true "overlay document symbol detail"
    (string_contains ~needle:"overlay A, B" detail)

let test_overlay_diagnostics () =
  let text =
    String.concat "\n"
      [
        "START";
        "ITEM A U 6;";
        "OVERLAY BAD POS(UNKNOWN_POS) (A, MISSING, A, SPACER(UNKNOWN_SPACER));";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "overlay diagnostics URI"
      (Lib.Uri_path.docuri_of_string "file:///overlay-diags.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "unknown overlay target is diagnosed"
    ~needle:"Unknown OVERLAY target" diags;
  expect_diagnostic_containing "duplicate overlay target is diagnosed"
    ~needle:"Duplicate OVERLAY target" diags;
  expect_diagnostic_containing "invalid overlay POS is diagnosed"
    ~needle:"Invalid OVERLAY POS expression" diags;
  expect_diagnostic_containing "invalid overlay spacer is diagnosed"
    ~needle:"Invalid OVERLAY spacer expression" diags

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
  let uri =
    expect_some "dependency change URI" (Lib.Uri_path.docuri_of_path path)
  in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_doc_lifecycle.open_doc ~lsp_version:1 ~inline_catch_up:false ws
    ~uri ~file:(Some path) ~text:dependency_change_text;
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
  expect_true "declaration edit dirties dependency graph" ws.graph_needs_refresh

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

let open_summary_compool_workspace ~(compool_text : string)
    ~(importer_text : string) =
  let ws = Lib.Workspace_state.create () in
  let compool_uri =
    expect_some "summary compool URI"
      (Lib.Uri_path.docuri_of_string "file:///summary-target-compool.j73")
  in
  let importer_uri =
    expect_some "summary importer URI"
      (Lib.Uri_path.docuri_of_string "file:///summary-compool-user.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:compool_uri ~file:None
    ~text:compool_text;
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:importer_uri ~file:None
    ~text:importer_text;
  let open Lib.Workspace_foundation in
  ws.graph_needs_refresh <- false;
  Hashtbl.clear ws.open_diag_revalidate_set;
  Lib.Workspace_foundation.Perf_stats.reset ();
  (ws, compool_uri, importer_uri)

let summary_body_compool_text =
  String.concat "\n"
    [
      "START";
      "COMPOOL TARGET;";
      "DEF PROC HELPER RENT;";
      "BEGIN";
      "  ITEM LOCAL U 6;";
      "  LOCAL = 1;";
      "END";
      "TERM";
      "";
    ]

let summary_body_importer_text =
  String.concat "\n"
    [
      "START";
      "!COMPOOL ('TARGET');";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  HELPER;";
      "END";
      "TERM";
      "";
    ]

let test_compool_whitespace_change_prunes_importer_invalidation () =
  let ws, compool_uri, importer_uri =
    open_summary_compool_workspace ~compool_text:summary_body_compool_text
      ~importer_text:summary_body_importer_text
  in
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:
        (range_of_substring summary_body_compool_text ~needle:"  LOCAL = 1;"
           ~nth:0)
      ~text:"    LOCAL = 1;" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri:compool_uri
    ~changes:[ change ];
  let open Lib.Workspace_foundation in
  expect_false "compool whitespace edit keeps dependency graph clean"
    ws.graph_needs_refresh;
  expect_false "compool whitespace edit does not revalidate importer"
    (Hashtbl.mem ws.open_diag_revalidate_set
       (Lib.Uri_path.docuri_to_string importer_uri));
  expect_true "public hash unchanged counter increments"
    (perf_metric_calls "summary.public_hash_unchanged" > 0)

let test_compool_private_body_decl_prunes_importer_invalidation () =
  let ws, compool_uri, importer_uri =
    open_summary_compool_workspace ~compool_text:summary_body_compool_text
      ~importer_text:summary_body_importer_text
  in
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:
        (range_of_substring summary_body_compool_text ~needle:"ITEM LOCAL U 6;"
           ~nth:0)
      ~text:"ITEM LOCAL U 12;" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri:compool_uri
    ~changes:[ change ];
  let open Lib.Workspace_foundation in
  expect_false "private body declaration keeps dependency graph clean"
    ws.graph_needs_refresh;
  expect_false "private body declaration does not revalidate importer"
    (Hashtbl.mem ws.open_diag_revalidate_set
       (Lib.Uri_path.docuri_to_string importer_uri));
  expect_true "legacy declaration invalidation is pruned by summary"
    (perf_metric_calls "dep.invalidate.pruned_by_summary" > 0)

let test_compool_public_type_change_revalidates_importers () =
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF BEGIN";
        "  TYPE COUNTER U 10;";
        "  TABLE VALUES (1) U 6;";
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
        "  ITEM LOCAL COUNTER;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws, compool_uri, importer_uri =
    open_summary_compool_workspace ~compool_text ~importer_text
  in
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:(range_of_substring compool_text ~needle:"U 10" ~nth:0)
      ~text:"U 12" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri:compool_uri
    ~changes:[ change ];
  let open Lib.Workspace_foundation in
  expect_true "public type edit dirties graph" ws.graph_needs_refresh;
  expect_true "public type edit revalidates importer"
    (Hashtbl.mem ws.open_diag_revalidate_set
       (Lib.Uri_path.docuri_to_string importer_uri));
  expect_true "public hash changed counter increments"
    (perf_metric_calls "summary.public_hash_changed" > 0)

let test_compool_import_change_invalidates_graph () =
  let ws = Lib.Workspace_state.create () in
  let uri =
    expect_some "summary import change URI"
      (Lib.Uri_path.docuri_of_string "file:///summary-import-change.j73")
  in
  let text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('TARGET');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:None ~text;
  let open Lib.Workspace_foundation in
  ws.graph_needs_refresh <- false;
  Hashtbl.clear ws.open_diag_revalidate_set;
  Lib.Workspace_foundation.Perf_stats.reset ();
  let change =
    T.TextDocumentContentChangeEvent.create
      ~range:(range_of_substring text ~needle:"TARGET" ~nth:0)
      ~text:"OTHER" ()
  in
  Lib.Workspace_doc_lifecycle.change_doc ~lsp_version:2 ws ~uri
    ~changes:[ change ];
  expect_true "COMPOOL import edit dirties graph" ws.graph_needs_refresh;
  expect_true "COMPOOL import edit records import invalidation"
    (perf_metric_calls "dep.invalidate.icompools" > 0)

let open_cross_module_lookup_workspace ~(compool_text : string)
    ~(importer_text : string) =
  let ws = Lib.Workspace_state.create () in
  let compool_uri =
    expect_some "cross-module compool URI"
      (Lib.Uri_path.docuri_of_string "file:///cross-module-target.j73")
  in
  let importer_uri =
    expect_some "cross-module importer URI"
      (Lib.Uri_path.docuri_of_string "file:///cross-module-user.j73")
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:compool_uri ~file:None
    ~text:compool_text;
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:importer_uri ~file:None
    ~text:importer_text;
  Lib.Workspace_foundation.Perf_stats.reset ();
  (ws, compool_uri, importer_uri)

let expect_location_uri label (loc : T.Location.t) (uri : T.DocumentUri.t) =
  expect_string label
    (Lib.Uri_path.docuri_to_string loc.uri)
    (Lib.Uri_path.docuri_to_string uri)

let expect_first_location label = function
  | loc :: _ -> loc
  | [] -> failf "%s: expected at least one location" label

let test_open_doc_installs_semantic_nav_snapshot () =
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
  let ws, compool_uri, importer_uri =
    open_cross_module_lookup_workspace ~compool_text ~importer_text
  in
  let expect_current_snapshot label uri =
    let doc =
      expect_some (label ^ " doc")
        (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs uri)
    in
    let snap =
      expect_some
        (label ^ " semantic snapshot")
        (Lib.Semantic_store.snapshot_for_uri ws.semantic_store ~uri)
    in
    expect_int
      (label ^ " semantic snapshot rev")
      snap.Lib.Semantic_store.Snapshot.doc_rev doc.Lib.Document.parse_rev;
    expect_true
      (label ^ " semantic snapshot has nav defs")
      (snap.Lib.Semantic_store.Snapshot.nav_defs <> [])
  in
  expect_current_snapshot "compool" compool_uri;
  expect_current_snapshot "importer" importer_uri;
  Lib.Workspace_foundation.Perf_stats.reset ();
  let shared_pos =
    position_of_offset importer_text
      (find_nth importer_text ~needle:"SHARED" ~nth:0 + 1)
  in
  let defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri:importer_uri
      ~pos:shared_pos
  in
  let def = expect_first_location "cached imported item definition" defs in
  expect_location_uri "cached imported item definition URI" def compool_uri;
  expect_int "cached definition lookup avoids lazy nav miss"
    (perf_metric_calls "nav.cache_miss")
    0;
  expect_int "cached definition lookup avoids lazy nav build"
    (perf_metric_calls "nav.build")
    0

let test_imported_compool_symbol_prefers_semantic_path () =
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF BEGIN";
        "  ITEM SHARED U 6;";
        "  TYPE COUNTER U 10;";
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
        "  ITEM LOCAL COUNTER;";
        "  SHARED = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws, compool_uri, importer_uri =
    open_cross_module_lookup_workspace ~compool_text ~importer_text
  in
  let target_doc =
    expect_some "semantic imported item target doc"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs compool_uri)
  in
  ignore
    (Lib.Workspace_nav_lookup.nav_for_doc_cached ws (Hashtbl.create 1)
       target_doc);
  Lib.Workspace_foundation.Perf_stats.reset ();
  let importer_doc =
    expect_some "semantic imported item importer doc"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs importer_uri)
  in
  expect_true "semantic helper finds imported COMPOOL symbol"
    (Lib.Workspace_nav_lookup.semantic_defs_for_imported_compools ws
       importer_doc ~key:"SHARED"
    <> []);
  let shared_pos =
    position_of_offset importer_text
      (find_nth importer_text ~needle:"SHARED" ~nth:0 + 1)
  in
  let defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri:importer_uri
      ~pos:shared_pos
  in
  let def = expect_first_location "semantic imported item definition" defs in
  expect_location_uri "semantic imported item definition URI" def compool_uri;
  let type_pos =
    position_of_offset importer_text
      (find_nth importer_text ~needle:"COUNTER" ~nth:0 + 1)
  in
  let type_defs =
    Lib.Workspace_type_definition.type_definition_locations_for ws
      ~uri:importer_uri ~pos:type_pos
  in
  let type_def =
    expect_first_location "semantic imported type definition" type_defs
  in
  expect_location_uri "semantic imported type definition URI" type_def
    compool_uri;
  expect_true "semantic cross-module hit counter increments"
    (perf_metric_calls "query.cross_module.semantic_hit" > 0);
  expect_int "warm semantic lookup avoids fallback scans"
    (perf_metric_calls "query.cross_module.fallback_scan")
    0

let test_imported_compool_proc_prefers_summary_path () =
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF PROC HELPER RENT;";
        "BEGIN";
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
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws, compool_uri, importer_uri =
    open_cross_module_lookup_workspace ~compool_text ~importer_text
  in
  let helper_pos =
    position_of_offset importer_text
      (find_nth importer_text ~needle:"HELPER" ~nth:0 + 1)
  in
  let defs =
    Lib.Workspace_definition.definition_locations_for ws ~uri:importer_uri
      ~pos:helper_pos
  in
  let def = expect_first_location "summary imported proc definition" defs in
  expect_location_uri "summary imported proc definition URI" def compool_uri;
  let refs =
    Lib.Workspace_references.references_locations_for ws ~uri:importer_uri
      ~pos:helper_pos ~include_decl:true
  in
  expect_true "summary imported proc references include call and declaration"
    (List.length refs >= 2);
  expect_true "summary cross-module hit counter increments"
    (perf_metric_calls "query.cross_module.summary_hit" > 0);
  expect_int "warm summary lookup avoids fallback scans"
    (perf_metric_calls "query.cross_module.fallback_scan")
    0

let test_broken_compool_summary_result_is_provisional () =
  let compool_text =
    String.concat "\n"
      [ "START"; "COMPOOL TARGET;"; "DEF PROC HELPER RENT;"; "BEGIN"; "" ]
  in
  let importer_text =
    String.concat "\n"
      [
        "START";
        "!COMPOOL ('TARGET');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws, compool_uri, importer_uri =
    open_cross_module_lookup_workspace ~compool_text ~importer_text
  in
  let helper_pos =
    position_of_offset importer_text
      (find_nth importer_text ~needle:"HELPER" ~nth:0 + 1)
  in
  let ctx =
    expect_some "broken summary query context"
      (Lib.Workspace_query.context ws ~uri:importer_uri ~pos:helper_pos)
  in
  let result = Lib.Workspace_query.definition_at_position ctx in
  let def =
    expect_first_location "broken summary imported proc definition" result.value
  in
  expect_location_uri "broken summary definition URI" def compool_uri;
  expect_true "broken summary result is provisional"
    (result.authority = Lib.Workspace_readiness.Provisional);
  expect_true "broken summary hit counter increments"
    (perf_metric_calls "query.cross_module.summary_hit" > 0);
  let doc =
    expect_some "broken summary importer doc"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs importer_uri)
  in
  let debug = Lib.Workspace_query.debug_report_json ws doc in
  let cross =
    expect_json_field "query debug cross-module" "crossModule" debug
  in
  let fallback_scan_count =
    match
      expect_json_field "query debug cross-module" "fallbackScanCount" cross
    with
    | `Int n -> n
    | got ->
        failf
          "query debug cross-module: expected integer fallbackScanCount, got %s"
          (Yojson.Safe.to_string got)
  in
  expect_int "debug report exposes fallback scan count" fallback_scan_count
    (perf_metric_calls "query.cross_module.fallback_scan")

let test_cross_module_reference_budget_still_stops () =
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF PROC HELPER RENT;";
        "BEGIN";
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
        "  HELPER;";
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws, _, importer_uri =
    open_cross_module_lookup_workspace ~compool_text ~importer_text
  in
  let helper_pos =
    position_of_offset importer_text
      (find_nth importer_text ~needle:"HELPER" ~nth:0 + 1)
  in
  let budget = Lib.Workspace_budget.start ~ws ~soft_budget_ms:0 in
  let refs =
    Lib.Workspace_references.references_locations_for ~budget ws
      ~uri:importer_uri ~pos:helper_pos ~include_decl:true
  in
  expect_int "cross-module tiny reference budget returns partial empty result"
    (List.length refs) 0;
  expect_true "cross-module tiny reference budget records stop reason"
    (Lib.Workspace_budget.reason_if_stopped budget
    = Some Lib.Workspace_readiness.SoftBudgetExceeded)

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
    expect_some "include graph node"
      (Hashtbl.find_opt ws.graph_nodes include_key)
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
  let include_target =
    match user_node.gn_include_targets with
    | target :: _ -> target
    | [] -> failf "ICOPY include target model was not recorded"
  in
  expect_string "ICOPY target raw path"
    include_target.Lib.Workspace_include_model.target "INC.j73";
  expect_string "ICOPY target normalized path"
    include_target.Lib.Workspace_include_model.normalized_target "INC.J73";
  expect_int "ICOPY directive location line"
    include_target.Lib.Workspace_include_model.directive_loc.start_pos.line 2;
  (match include_target.Lib.Workspace_include_model.resolved_path with
  | Some path ->
      expect_string "ICOPY resolved path" (normalize_path path) include_key
  | None -> failf "ICOPY include target did not resolve");
  Lib.Workspace_doc_lifecycle.apply_watched_file_changes ws
    ~changes:[ (include_path, `Changed) ];
  expect_true "ICOPY target change enqueues dependent"
    (Hashtbl.mem ws.bg_enqueued user_key)

let test_icopy_debug_report_exposes_includes () =
  let root = mk_temp_dir "jovial-icopy-debug" in
  let include_path = Filename.concat root "INC.j73" in
  let user_path = Filename.concat root "USER.j73" in
  let include_text =
    String.concat "\n"
      [ "START"; "DEF PROC HELPER RENT;"; "BEGIN"; "END"; "TERM"; "" ]
  in
  let user_text =
    String.concat "\n"
      [
        "START";
        "ICOPY ('INC.j73');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  write_text include_path include_text;
  write_text user_path user_text;
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ include_path; user_path ]);
  Lib.Workspace_index_graph.pump_index_background ws;
  let user_uri =
    expect_some "ICOPY debug user URI" (Lib.Uri_path.docuri_of_path user_path)
  in
  let include_uri =
    expect_some "ICOPY debug include URI"
      (Lib.Uri_path.docuri_of_path include_path)
  in
  Lib.Workspace_doc_lifecycle.open_doc ~inline_catch_up:false ws ~uri:user_uri
    ~file:(Some user_path) ~text:user_text;
  Lib.Workspace_doc_lifecycle.open_doc ~inline_catch_up:false ws
    ~uri:include_uri ~file:(Some include_path) ~text:include_text;
  let user_report =
    Lib.Workspace_reporting.debug_report_for ws ~uri:user_uri ~max_tokens:16
  in
  let include_targets =
    match
      expect_json_field "ICOPY debug report" "includeTargets" user_report
    with
    | `List xs -> xs
    | got ->
        failf "ICOPY debug includeTargets: expected list, got %s"
          (Yojson.Safe.to_string got)
  in
  expect_int "ICOPY debug include target count" (List.length include_targets) 1;
  let target_json =
    match include_targets with
    | [ item ] -> item
    | _ -> failf "ICOPY debug include target count changed unexpectedly"
  in
  expect_string "ICOPY debug target"
    (match expect_json_field "ICOPY debug target" "target" target_json with
    | `String s -> s
    | got ->
        failf "ICOPY debug target: expected string, got %s"
          (Yojson.Safe.to_string got))
    "INC.j73";
  expect_string "ICOPY debug resolved path"
    (match
       expect_json_field "ICOPY debug target" "resolvedPath" target_json
     with
    | `String s -> normalize_path s
    | got ->
        failf "ICOPY debug resolvedPath: expected string, got %s"
          (Yojson.Safe.to_string got))
    (normalize_path include_path);
  let include_report =
    Lib.Workspace_reporting.debug_report_for ws ~uri:include_uri ~max_tokens:16
  in
  let reverse_users =
    match
      expect_json_field "ICOPY debug include report" "reverseIncludeUsers"
        include_report
    with
    | `List xs ->
        xs
        |> List.filter_map (function
          | `String s -> Some (normalize_path s)
          | _ -> None)
    | got ->
        failf "ICOPY debug reverseIncludeUsers: expected list, got %s"
          (Yojson.Safe.to_string got)
  in
  expect_true "ICOPY debug reverse include user recorded"
    (List.mem (normalize_path user_path) reverse_users)

let test_icopy_unresolved_include_diagnostic () =
  let root = mk_temp_dir "jovial-icopy-missing" in
  let user_path = Filename.concat root "USER.j73" in
  let user_text =
    String.concat "\n"
      [
        "START";
        "ICOPY ('MISSING.j73');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  write_text user_path user_text;
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ user_path ]);
  let uri =
    expect_some "ICOPY missing URI" (Lib.Uri_path.docuri_of_path user_path)
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:(Some user_path)
    ~text:user_text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "unresolved ICOPY target is diagnosed"
    ~needle:"Unresolved ICOPY target: MISSING.j73" diags

let test_icopy_cyclic_include_diagnostic () =
  let root = mk_temp_dir "jovial-icopy-cycle" in
  let path = Filename.concat root "LOOP.j73" in
  let text =
    String.concat "\n"
      [
        "START";
        "ICOPY ('LOOP.j73');";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "END";
        "TERM";
        "";
      ]
  in
  write_text path text;
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ path ]);
  let uri = expect_some "ICOPY cycle URI" (Lib.Uri_path.docuri_of_path path) in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:(Some path) ~text;
  let diags = revalidated_diagnostics ws uri in
  expect_diagnostic_containing "cyclic ICOPY target is diagnosed"
    ~needle:"Cyclic ICOPY include detected for LOOP.j73" diags

let test_icopy_source_map_json_roundtrip () =
  let pos line col offset : Lib.Ast.Loc.pos = { line; col; offset } in
  let expanded_range =
    Lib.Ast.Loc.make ~file:(Some "expanded.j73") ~start_pos:(pos 3 2 20)
      ~end_pos:(pos 3 8 26)
  in
  let original_range =
    Lib.Ast.Loc.make ~file:(Some "INC.j73") ~start_pos:(pos 7 1 70)
      ~end_pos:(pos 7 7 76)
  in
  let record =
    Lib.Workspace_include_model.make_source_map_record ~expanded_range
      ~original_source_file:(Some "INC.j73") ~original_range
  in
  let json = Lib.Workspace_include_model.source_map_record_to_yojson record in
  let roundtrip =
    expect_some "ICOPY source map roundtrip"
      (Lib.Workspace_include_model.source_map_record_of_yojson json)
  in
  expect_string "ICOPY source map original file"
    (Option.value roundtrip.Lib.Workspace_include_model.original_source_file
       ~default:"")
    "INC.j73";
  expect_int "ICOPY source map expanded line"
    roundtrip.Lib.Workspace_include_model.expanded_range.start_pos.line 3;
  expect_int "ICOPY source map original line"
    roundtrip.Lib.Workspace_include_model.original_range.start_pos.line 7

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
  expect_quoted_identifier_definition ws uri use_off "quoted identifier digit" 7;
  expect_quoted_identifier_definition ws uri use_off "quoted identifier quote" 8;
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

let test_macro_rename_updates_define_and_uses () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEFINE LIMIT \"10\";";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM COUNT U 6;";
        "  COUNT = LIMIT;";
        "  \"LIMIT in comment\";";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "macro rename URI"
      (Lib.Uri_path.docuri_of_string "file:///rename-macro.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let use_off =
    find_nth text ~needle:"COUNT = LIMIT" ~nth:0 + String.length "COUNT = "
  in
  let pos = position_of_offset text (use_off + 1) in
  (match Lib.Workspace.prepare_rename_for ws ~uri ~pos with
  | Some (`RangeWithPlaceholder (_, placeholder)) ->
      expect_string "macro rename placeholder" placeholder "LIMIT"
  | Some (`Range _) -> failf "macro rename returned bare range"
  | None -> failf "macro rename prepare returned None");
  let edit =
    expect_some "macro rename edit"
      (Lib.Workspace.rename_for ws ~uri ~pos ~new_name:"BOUND")
  in
  let updated = apply_workspace_edit_for_uri "macro rename" text edit uri in
  expect_true "macro declaration renamed"
    (string_contains ~needle:"DEFINE BOUND \"10\";" updated);
  expect_true "macro use renamed"
    (string_contains ~needle:"COUNT = BOUND;" updated);
  expect_true "macro rename ignores comments"
    (string_contains ~needle:"\"LIMIT in comment\";" updated);
  expect_false "macro declaration old name removed"
    (string_contains ~needle:"DEFINE LIMIT \"10\";" updated)

let test_macro_rename_rejects_generated_actual_symbol () =
  let text =
    String.concat "\n"
      [
        "START";
        "DEFINE SET(X) \"$X = 1\";";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM TARGET U 6;";
        "  SET(TARGET);";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "macro generated rename URI"
      (Lib.Uri_path.docuri_of_string "file:///rename-generated-macro.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let actual_off =
    find_nth text ~needle:"SET(TARGET)" ~nth:0 + String.length "SET("
  in
  let pos = position_of_offset text (actual_off + 1) in
  expect_none "generated macro actual prepare rename is rejected"
    (Lib.Workspace.prepare_rename_for ws ~uri ~pos);
  expect_none "generated macro actual rename is rejected"
    (Lib.Workspace.rename_for ws ~uri ~pos ~new_name:"RENAMED")

let test_field_rename_respects_owner () =
  let text =
    String.concat "\n"
      [
        "START";
        "TABLE FIRST(1), BEGIN";
        "  ITEM FIELD U 1;";
        "END;";
        "TABLE SECOND(1), BEGIN";
        "  ITEM FIELD U 2;";
        "END;";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM OUT U 2;";
        "  OUT = FIRST(1).FIELD;";
        "  OUT = SECOND(1).FIELD;";
        "END";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "field rename URI"
      (Lib.Uri_path.docuri_of_string "file:///rename-owned-fields.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let first_field_off =
    find_nth text ~needle:"FIRST(1).FIELD" ~nth:0 + String.length "FIRST(1)."
  in
  let pos = position_of_offset text (first_field_off + 1) in
  let edit =
    expect_some "field rename edit"
      (Lib.Workspace.rename_for ws ~uri ~pos ~new_name:"ALPHA")
  in
  let updated = apply_workspace_edit_for_uri "field rename" text edit uri in
  expect_true "FIRST field declaration renamed"
    (string_contains ~needle:"  ITEM ALPHA U 1;" updated);
  expect_true "FIRST field use renamed"
    (string_contains ~needle:"OUT = FIRST(1).ALPHA;" updated);
  expect_true "SECOND field declaration preserved"
    (string_contains ~needle:"  ITEM FIELD U 2;" updated);
  expect_true "SECOND field use preserved"
    (string_contains ~needle:"OUT = SECOND(1).FIELD;" updated);
  expect_false "FIRST field old use removed"
    (string_contains ~needle:"OUT = FIRST(1).FIELD;" updated)

let test_field_rename_rejects_ambiguous_fallback () =
  let text =
    String.concat "\n"
      [
        "START";
        "TYPE REC1 TABLE;";
        "BEGIN";
        "  ITEM FIELD U 1;";
        "END";
        "TYPE REC2 TABLE;";
        "BEGIN";
        "  ITEM FIELD U 1;";
        "END";
        "FIELD = 1;";
        "TERM";
        "";
      ]
  in
  let ws = Lib.Workspace.create () in
  let uri =
    expect_some "field fallback rename URI"
      (Lib.Uri_path.docuri_of_string "file:///rename-ambiguous-field.j73")
  in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text;
  let off = find_nth text ~needle:"FIELD = 1" ~nth:0 in
  let pos = position_of_offset text (off + 1) in
  expect_none "ambiguous field prepare rename is rejected"
    (Lib.Workspace.prepare_rename_for ws ~uri ~pos);
  expect_none "ambiguous field rename is rejected"
    (Lib.Workspace.rename_for ws ~uri ~pos ~new_name:"ALPHA")

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
  expect_mode "15 MB threshold stays large" settings.huge_file_threshold_bytes
    Lib.Workspace_tuning.Large;
  expect_mode "over 15 MB enters huge mode"
    (settings.huge_file_threshold_bytes + 1)
    Lib.Workspace_tuning.Huge;
  expect_true "15 MB full parse allowed by default"
    (Lib.Workspace_tuning.full_parse_allowed_for_size ws
       ~bytes:settings.huge_file_threshold_bytes);
  expect_false "over 15 MB full parse disabled by default"
    (Lib.Workspace_tuning.full_parse_allowed_for_size ws
       ~bytes:(settings.huge_file_threshold_bytes + 1))

let test_open_doc_readiness_targets_reset_per_open () =
  let settings = Lib.Workspace_settings.from_env () in
  let ws = Lib.Workspace_state.create ~settings () in
  ws.Lib.Workspace_foundation.startup_fully_nav_ready_ms <- Some 0.0;
  Lib.Workspace_runtime.startup_mark_open_doc ws ~bytes:1024;
  expect_int "normal open diag target"
    ws.Lib.Workspace_foundation.startup_diag_hover_target_ms 1500;
  expect_int "normal open nav target"
    ws.Lib.Workspace_foundation.startup_nav_target_ms 1500;
  expect_none "normal open resets readiness"
    ws.Lib.Workspace_foundation.startup_fully_nav_ready_ms;
  ws.Lib.Workspace_foundation.startup_fully_nav_ready_ms <- Some 0.0;
  Lib.Workspace_runtime.startup_mark_open_doc ws
    ~bytes:(settings.huge_file_threshold_bytes + 1);
  expect_int "huge open diag target"
    ws.Lib.Workspace_foundation.startup_diag_hover_target_ms 10000;
  expect_int "huge open nav target"
    ws.Lib.Workspace_foundation.startup_nav_target_ms 10000;
  expect_none "huge open resets readiness"
    ws.Lib.Workspace_foundation.startup_fully_nav_ready_ms;
  Lib.Workspace_runtime.startup_mark_open_doc ws ~bytes:1024;
  expect_int "normal open restores nav target"
    ws.Lib.Workspace_foundation.startup_nav_target_ms 1500

let expect_json_int_field name key json : int =
  match expect_json_field name key json with
  | `Int n -> n
  | `Intlit s -> ( try int_of_string s with _ -> failf "%s: bad intlit" name)
  | got ->
      failf "%s: expected integer field %S, got %s" name key
        (Yojson.Safe.to_string got)

let test_debug_scheduler_memory_json_shape () =
  let root = mk_temp_dir "jovial-debug-state" in
  let path = Filename.concat root "MAIN.j73" in
  write_text path (main_text "MAIN");
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ path ]);
  Lib.Workspace_state.enqueue_bg_path ws ~lane:Lib.Workspace_foundation.LaneOpen
    ~reason_group:"debug_test" ~high:true path;
  let scheduler = Lib.Workspace_reporting.debug_scheduler_json ws in
  ignore (expect_json_field "debug scheduler" "pressureMode" scheduler);
  let queues = expect_json_field "debug scheduler" "queues" scheduler in
  expect_true "debug scheduler highSmall queued"
    (expect_json_int_field "debug scheduler queues" "highSmall" queues >= 1);
  ignore
    (expect_json_field "debug scheduler" "parseWorkerInflightCount" scheduler);
  ignore (expect_json_field "debug scheduler" "cursors" scheduler);
  (match expect_json_field "debug scheduler" "nextJobs" scheduler with
  | `List (_ :: _) -> ()
  | got ->
      failf "debug scheduler: expected non-empty nextJobs, got %s"
        (Yojson.Safe.to_string got));
  let memory = Lib.Workspace_reporting.debug_memory_json ws in
  ignore (expect_json_field "debug memory" "pressureMode" memory);
  ignore (expect_json_field "debug memory" "liveMb" memory);
  expect_int "debug memory open docs"
    (expect_json_int_field "debug memory" "openDocs" memory)
    0;
  ignore (expect_json_field "debug memory" "closedDocsCached" memory);
  ignore (expect_json_field "debug memory" "astsShed" memory);
  ignore (expect_json_field "debug memory" "closedDocEvictions" memory);
  ignore (expect_json_field "debug memory" "parseJobsInflight" memory)

let test_large_startup_readiness_allows_incremental_drain () =
  let root = mk_temp_dir "jovial-large-startup-ready" in
  let settings =
    {
      (Lib.Workspace_settings.from_env ()) with
      workspace_profile_mode = Lib.Workspace_settings.ProfileModeLarge;
    }
  in
  let ws = Lib.Workspace_state.create ~settings () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ws.Lib.Workspace_foundation.index <-
    Some
      (Lib.Workspace_index.of_source_files
         ~source_extensions:ws.Lib.Workspace_foundation.source_extensions ~root
         ~paths:[]);
  ws.Lib.Workspace_foundation.bg_seed_needs_refresh <- false;
  ws.Lib.Workspace_foundation.graph_needs_refresh <- false;
  ws.Lib.Workspace_foundation.bg_seed_paths <- [||];
  ws.Lib.Workspace_foundation.bg_seed_cursor <- 0;
  ws.Lib.Workspace_foundation.graph_root_closure_paths <- [||];
  ws.Lib.Workspace_foundation.graph_root_closure_cursor <- 0;
  ws.Lib.Workspace_foundation.quick_nav_index_total <- 100;
  ws.Lib.Workspace_foundation.quick_nav_index_done <- 64;
  Queue.add "pending.j73" ws.Lib.Workspace_foundation.quick_nav_pending_paths;
  Hashtbl.replace ws.Lib.Workspace_foundation.quick_nav_pending_set
    "pending.j73" true;
  Queue.add "background-large.j73"
    ws.Lib.Workspace_foundation.bg_norm_large_queue;
  Queue.add "root-large.j73" ws.Lib.Workspace_foundation.bg_root_large_queue;
  Queue.add "import-large.j73" ws.Lib.Workspace_foundation.bg_high_large_queue;
  expect_true
    "large startup reaches interactive nav readiness without full drain"
    (Lib.Workspace_runtime.interactive_nav_ready ws);
  expect_true "large startup reaches nav readiness before quick-nav complete"
    (Lib.Workspace_runtime.startup_is_ready_now ws);
  let readiness = Lib.Workspace_runtime.startup_readiness_json_for_report ws in
  expect_json_bool_field "large startup" "isReady" readiness true;
  let components = expect_json_field "large startup" "components" readiness in
  expect_json_bool_field "large startup interactive ready" "interactiveNavReady"
    components true;
  expect_json_bool_field "large startup quick ready" "quickNavIndexReady"
    components true;
  expect_json_bool_field "large startup quick complete" "quickNavIndexComplete"
    components false;
  expect_json_bool_field "large startup background drain"
    "backgroundDrainRequired" components false;
  expect_json_bool_field "large startup high queues continue" "highQueuesEmpty"
    components false;
  expect_json_bool_field "large startup interactive queues continue"
    "interactiveQueuesEmpty" components false;
  expect_json_bool_field "large startup queues still draining" "queuesEmpty"
    components false;
  expect_json_bool_field "large startup quick complete alias" "quickNavComplete"
    components false;
  expect_json_bool_field "large startup deep semantic complete"
    "deepSemanticComplete" components true

let test_large_critical_pressure_initializes_quick_nav_seed () =
  let root = mk_temp_dir "jovial-large-critical-seed" in
  let path = Filename.concat root "MAIN.j73" in
  write_text path (main_text "MAIN");
  let settings =
    {
      (Lib.Workspace_settings.from_env ()) with
      workspace_profile_mode = Lib.Workspace_settings.ProfileModeLarge;
      pressure_soft_mb = 1;
      pressure_critical_mb = 1;
    }
  in
  let ws = Lib.Workspace.create ~settings () in
  Lib.Workspace.set_root_path ws (Some root);
  ignore (Lib.Workspace.set_source_files ws [ path ]);
  ignore (Lib.Workspace.ensure_index_health ws);
  Lib.Workspace.background_tick ws ~budget_ms:50 ~mode:Lib.Workspace.BgTickIdle
    ~idle_quiet_ms:0 ~last_message_ms:0.0;
  let readiness = Lib.Workspace.startup_readiness_json_for_report ws in
  let components = expect_json_field "critical seed" "components" readiness in
  expect_json_bool_field "critical seed ready" "seedReady" components true;
  expect_true "critical seed initializes quick-nav total"
    (expect_json_int_field "critical seed" "quickNavTotal" components > 0);
  expect_json_bool_field "critical seed quick ready" "quickNavIndexReady"
    components true;
  expect_json_bool_field "critical seed root closure ready" "rootClosureReady"
    components true;
  expect_json_bool_field "critical seed navigable" "fullyNavigable" components
    true

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
  expect_int "persistent source count"
    (Lib.Workspace_index.source_count loaded)
    2;
  ignore
    (expect_some "persistent compool"
       (Lib.Workspace_index.find_compool loaded ~name:"TARGET"));
  write_text compool_path "";
  ignore (Lib.Workspace_index.replace_source_files loaded ~paths:[ main_path ]);
  expect_none "persistent deleted file pruned"
    (Lib.Workspace_index.find_compool loaded ~name:"TARGET");
  let ws = Lib.Workspace.create () in
  let main_uri =
    expect_some "persistent main URI" (Lib.Uri_path.docuri_of_path main_path)
  in
  let compool_uri =
    expect_some "persistent compool URI"
      (Lib.Uri_path.docuri_of_path compool_path)
  in
  Lib.Workspace.open_doc ws ~uri:main_uri ~file:(Some main_path)
    ~text:(main_text "MAIN");
  Lib.Workspace.open_doc ws ~uri:compool_uri ~file:(Some compool_path)
    ~text:target_text;
  Lib.Workspace.snapshot ws
  |> Lib.Workspace_persistent_index.save_snapshot_index ~root;
  let expect_cache_file name path = expect_true name (Sys.file_exists path) in
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

let test_persistent_cache_source_index_invalidation_and_corrupt () =
  let root = mk_temp_dir "jovial-persistent-cache-source" in
  let main_path = Filename.concat root "MAIN.j73" in
  let compool_path = Filename.concat root "TARGET.j73" in
  let source_extensions = [ ".j73" ] in
  write_text main_path (main_text "MAIN");
  write_text compool_path target_text;
  let load1 =
    Lib.Persistent_cache.load_or_build_source_index ~root ~source_extensions
      ~paths:[ main_path; compool_path ]
  in
  expect_false "source cache first startup is cold"
    load1.Lib.Persistent_cache.loaded_from_cache;
  expect_int "source cache cold source count"
    (Lib.Workspace_index.source_count load1.index)
    2;
  Lib.Persistent_cache.save_source_index ~root ~source_extensions load1.index;
  expect_true "source cache file written"
    (Sys.file_exists (Lib.Persistent_cache.source_index_json_path ~root));
  let load2 =
    Lib.Persistent_cache.load_or_build_source_index ~root ~source_extensions
      ~paths:[ main_path; compool_path ]
  in
  expect_true "source cache second startup is warm"
    load2.Lib.Persistent_cache.loaded_from_cache;
  expect_int "source cache unchanged paths"
    (List.length load2.Lib.Persistent_cache.changed_paths)
    0;
  expect_int "source cache unchanged pruned paths"
    (List.length load2.Lib.Persistent_cache.pruned_paths)
    0;
  write_text compool_path (String.concat "\n" [ "START"; "TERM"; "" ]);
  let load3 =
    Lib.Persistent_cache.load_or_build_source_index ~root ~source_extensions
      ~paths:[ main_path; compool_path ]
  in
  expect_true "source cache changed startup still uses cache"
    load3.Lib.Persistent_cache.loaded_from_cache;
  expect_path "source cache invalidates modified file only"
    load3.Lib.Persistent_cache.changed_paths compool_path;
  expect_false "source cache does not invalidate unchanged file"
    (path_in load3.Lib.Persistent_cache.changed_paths main_path);
  expect_none "source cache reconciles removed compool"
    (Lib.Workspace_index.find_compool load3.index ~name:"TARGET");
  write_text (Lib.Persistent_cache.source_index_json_path ~root) "{not json";
  let load4 =
    Lib.Persistent_cache.load_or_build_source_index ~root ~source_extensions
      ~paths:[ main_path; compool_path ]
  in
  expect_false "corrupt source cache is ignored"
    load4.Lib.Persistent_cache.loaded_from_cache;
  expect_int "corrupt source cache rebuild source count"
    (Lib.Workspace_index.source_count load4.index)
    2

let test_persistent_cache_skeleton_roundtrip_and_startup_hydrate () =
  let root = mk_temp_dir "jovial-persistent-cache-skeleton" in
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
        "LBL: NUM = ;";
        "";
      ]
  in
  write_text path text;
  let ws = Lib.Workspace_state.create () in
  let source_extensions = ws.Lib.Workspace_foundation.source_extensions in
  let max_bytes = Lib.Workspace_tuning.nav_quick_scan_per_file_bytes in
  let entries =
    Lib.Workspace_background.quick_nav_entries_of_path_prefix path ~max_bytes
  in
  Lib.Persistent_cache.save_skeleton_entry ~root ~source_extensions ~max_bytes
    ~path ~entries;
  let cache =
    Lib.Persistent_cache.load_skeleton_cache ~root ~source_extensions ~max_bytes
      ~paths:[ path ]
  in
  let cached_entries =
    expect_some "skeleton cache roundtrip entries"
      (Lib.Persistent_cache.skeleton_entries cache ~path)
  in
  let names =
    cached_entries
    |> List.map (fun (entry : Lib.Workspace_foundation.quick_nav_entry) ->
        normalize_name entry.qn_name)
  in
  expect_true "skeleton cache stores procedure" (List.mem "MAIN" names);
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ path ]);
  ignore (Lib.Workspace_index_graph.ensure_index_health ws);
  Lib.Workspace_background.refresh_bg_seed_paths ws;
  let path_key = Lib.Uri_path.normalize_path_key path in
  expect_true "startup quick-nav hydrates cached file"
    (Hashtbl.mem ws.Lib.Workspace_foundation.quick_nav_done_set path_key);
  expect_int "startup quick-nav has no pending scan after hydrate"
    (Queue.length ws.Lib.Workspace_foundation.quick_nav_pending_paths)
    0;
  expect_true "startup quick-nav cache contains MAIN"
    (match
       Hashtbl.find_opt ws.Lib.Workspace_foundation.quick_nav_index "MAIN"
     with
    | Some (_ :: _) -> true
    | _ -> false);
  write_text path (text ^ "ITEM EXTRA U 1;\n");
  let stale_cache =
    Lib.Persistent_cache.load_skeleton_cache ~root ~source_extensions ~max_bytes
      ~paths:[ path ]
  in
  expect_none "skeleton cache invalidates modified file"
    (Lib.Persistent_cache.skeleton_entries stale_cache ~path);
  write_text (Lib.Persistent_cache.skeleton_index_json_path ~root) "{not json";
  let corrupt_cache =
    Lib.Persistent_cache.load_skeleton_cache ~root ~source_extensions ~max_bytes
      ~paths:[ path ]
  in
  expect_none "corrupt skeleton cache is ignored"
    (Lib.Persistent_cache.skeleton_entries corrupt_cache ~path)

let test_persistent_cache_skeleton_buffered_flush () =
  let root = mk_temp_dir "jovial-persistent-cache-skeleton-buffered" in
  let path = Filename.concat root "MAIN.j73" in
  let text = main_text "MAIN" in
  write_text path text;
  let ws = Lib.Workspace_state.create () in
  let source_extensions = ws.Lib.Workspace_foundation.source_extensions in
  let max_bytes = Lib.Workspace_tuning.nav_quick_scan_per_file_bytes in
  let entries =
    Lib.Workspace_background.quick_nav_entries_of_path_prefix path ~max_bytes
  in
  Lib.Persistent_cache.save_skeleton_entries_buffered ~root ~source_extensions
    ~max_bytes
    ~entries_by_path:[ (path, entries) ];
  let before_flush =
    Lib.Persistent_cache.load_skeleton_cache ~root ~source_extensions ~max_bytes
      ~paths:[ path ]
  in
  expect_none "buffered skeleton cache waits for flush"
    (Lib.Persistent_cache.skeleton_entries before_flush ~path);
  Lib.Persistent_cache.flush_skeleton_entries ~root ~source_extensions
    ~max_bytes;
  let after_flush =
    Lib.Persistent_cache.load_skeleton_cache ~root ~source_extensions ~max_bytes
      ~paths:[ path ]
  in
  let flushed_entries =
    expect_some "buffered skeleton cache flushes entries"
      (Lib.Persistent_cache.skeleton_entries after_flush ~path)
  in
  expect_true "buffered skeleton cache stores MAIN"
    (List.exists
       (fun (entry : Lib.Workspace_foundation.quick_nav_entry) ->
         normalize_name entry.qn_name = "MAIN")
       flushed_entries)

let module_summary_of_text ~(path : string) ~(text : string) :
    Lib.Module_summary.t =
  let uri =
    expect_some "module summary URI" (Lib.Uri_path.docuri_of_path path)
  in
  let doc = Lib.Document.make ~uri ~file:(Some path) ~text in
  Lib.Module_summary.of_document doc

let first_module_summary_entry label entries =
  match entries with
  | entry :: _ -> entry
  | [] -> failf "%s: expected one cached module summary" label

let test_persistent_cache_module_summary_roundtrip_and_corrupt () =
  let root = mk_temp_dir "jovial-summary-cache-roundtrip" in
  let path = Filename.concat root "TARGET.j73" in
  let text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF ITEM LIMIT U 10;";
        "TYPE COUNT U 10;";
        "TERM";
        "";
      ]
  in
  write_text path text;
  let source_extensions = [ ".j73" ] in
  let summary = module_summary_of_text ~path ~text in
  Lib.Persistent_cache.save_module_summary_entry ~root ~source_extensions ~path
    ~summary;
  let cache =
    Lib.Persistent_cache.load_module_summary_cache ~root ~source_extensions
      ~paths:[ path ]
  in
  let entry =
    first_module_summary_entry "module summary cache roundtrip"
      (Lib.Persistent_cache.module_summary_entries cache)
  in
  expect_string "module summary public hash roundtrip"
    entry.Lib.Workspace_foundation.msc_summary
      .Lib.Module_summary.public_signature_hash
    summary.Lib.Module_summary.public_signature_hash;
  expect_true "module summary roundtrip is metadata validated"
    (entry.Lib.Workspace_foundation.msc_authority
   = Lib.Workspace_foundation.ModuleSummaryMetadataValidated);
  write_text (Lib.Persistent_cache.module_summary_json_path ~root) "{not json";
  let corrupt =
    Lib.Persistent_cache.load_module_summary_cache ~root ~source_extensions
      ~paths:[ path ]
  in
  expect_int "corrupt module summary cache is ignored"
    (List.length (Lib.Persistent_cache.module_summary_entries corrupt))
    0

let test_persistent_cache_module_summary_invalidation_and_body_edit () =
  let root = mk_temp_dir "jovial-summary-cache-invalidation" in
  let path = Filename.concat root "TARGET.j73" in
  let public_v1 =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF PROC HELPER RENT;";
        "BEGIN";
        "  ITEM LOCAL U 1;";
        "  LOCAL = 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let body_v2 =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF PROC HELPER RENT;";
        "BEGIN";
        "  ITEM LOCAL U 1;";
        "  LOCAL = 2;";
        "END";
        "TERM";
        "";
      ]
  in
  let public_v2 =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF PROC HELPER(ARG) RENT;";
        "BEGIN";
        "  ITEM ARG U 1;";
        "END";
        "TERM";
        "";
      ]
  in
  let source_extensions = [ ".j73" ] in
  write_text path public_v1;
  let summary_v1 = module_summary_of_text ~path ~text:public_v1 in
  Lib.Persistent_cache.save_module_summary_entry ~root ~source_extensions ~path
    ~summary:summary_v1;
  write_text path public_v2;
  let summary_public_v2 = module_summary_of_text ~path ~text:public_v2 in
  expect_false "public signature edit changes summary hash"
    (Lib.Module_summary.public_signature_unchanged summary_v1 summary_public_v2);
  let stale =
    Lib.Persistent_cache.load_module_summary_cache ~root ~source_extensions
      ~paths:[ path ]
  in
  expect_int "changed public file metadata invalidates cached summary"
    (List.length (Lib.Persistent_cache.module_summary_entries stale))
    0;
  write_text path body_v2;
  let summary_body_v2 = module_summary_of_text ~path ~text:body_v2 in
  expect_true "body-only edit preserves public summary hash"
    (Lib.Module_summary.public_signature_unchanged summary_v1 summary_body_v2);
  Lib.Persistent_cache.save_module_summary_entry ~root ~source_extensions ~path
    ~summary:summary_body_v2;
  let refreshed =
    Lib.Persistent_cache.load_module_summary_cache ~root ~source_extensions
      ~paths:[ path ]
  in
  let entry =
    first_module_summary_entry "body edit refreshed summary"
      (Lib.Persistent_cache.module_summary_entries refreshed)
  in
  expect_string "body edit keeps cached public hash"
    entry.Lib.Workspace_foundation.msc_summary
      .Lib.Module_summary.public_signature_hash
    summary_v1.Lib.Module_summary.public_signature_hash

let test_persistent_cache_warm_start_summary_definition_provisional () =
  let root = mk_temp_dir "jovial-summary-cache-warm-start" in
  let compool_path = Filename.concat root "TARGET.j73" in
  let main_path = Filename.concat root "MAIN.j73" in
  let compool_text =
    String.concat "\n"
      [
        "START";
        "COMPOOL TARGET;";
        "DEF PROC HELPER RENT;";
        "BEGIN";
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
        "  HELPER;";
        "END";
        "TERM";
        "";
      ]
  in
  write_text compool_path compool_text;
  write_text main_path main_text;
  let ws = Lib.Workspace_state.create () in
  let source_extensions = ws.Lib.Workspace_foundation.source_extensions in
  let summary = module_summary_of_text ~path:compool_path ~text:compool_text in
  Lib.Persistent_cache.save_module_summary_entry ~root ~source_extensions
    ~path:compool_path ~summary;
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws [ compool_path; main_path ]);
  ignore (Lib.Workspace_index_graph.ensure_index_health ws);
  let total, _, validated =
    Lib.Workspace_state.module_summary_cache_counts ws
  in
  expect_true "warm startup hydrates summary cache" (total >= 1);
  expect_true "warm startup validates summary metadata" (validated >= 1);
  let main_uri =
    expect_some "warm summary main URI" (Lib.Uri_path.docuri_of_path main_path)
  in
  Lib.Workspace_doc_lifecycle.open_doc ws ~uri:main_uri ~file:(Some main_path)
    ~text:main_text;
  let helper_pos =
    position_of_offset main_text (find_nth main_text ~needle:"HELPER" ~nth:0 + 1)
  in
  let ctx =
    expect_some "warm summary query context"
      (Lib.Workspace_query.context ws ~uri:main_uri ~pos:helper_pos)
  in
  Lib.Workspace_foundation.Perf_stats.reset ();
  let result = Lib.Workspace_query.definition_at_position ctx in
  let loc =
    expect_first_location "warm summary-backed definition" result.value
  in
  let compool_uri =
    expect_some "warm summary compool URI"
      (Lib.Uri_path.docuri_of_path compool_path)
  in
  expect_location_uri "warm summary-backed definition URI" loc compool_uri;
  expect_true "warm summary-backed definition is provisional"
    (result.authority = Lib.Workspace_readiness.Provisional);
  expect_true "warm summary-backed definition uses summary cache"
    (perf_metric_calls "query.cross_module.summary_hit" > 0);
  let importer_doc =
    expect_some "warm summary importer doc"
      (Hashtbl.find_opt ws.Lib.Workspace_foundation.docs main_uri)
  in
  let debug = Lib.Workspace_query.debug_report_json ws importer_doc in
  let cache_json =
    expect_json_field "warm summary debug" "moduleSummaryCache" debug
  in
  expect_true "debug report exposes hydrated summary count"
    (expect_json_int_field "warm summary debug cache" "entryCount" cache_json
    >= 1);
  expect_true "debug report exposes metadata validated summary count"
    (expect_json_int_field "warm summary debug cache" "metadataValidatedCount"
       cache_json
    >= 1)

let test_skeleton_snapshot_ide_query () =
  let root = mk_temp_dir "jovial-snapshot-query" in
  let path = Filename.concat root "MAIN.j73" in
  let text = main_text "MAIN" in
  write_text path text;
  let uri = expect_some "snapshot uri" (Lib.Uri_path.docuri_of_path path) in
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
  with exn -> failf "%s failed: %s" name (Printexc.to_string exn)

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
      ( "macro_graph_navigation_hover_references",
        test_macro_graph_navigation_hover_references );
      ( "formatting_normal_nested_macro_and_broken",
        test_formatting_normal_nested_macro_and_broken );
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
      ( "multiline_selected_import_excludes_unselected_symbol",
        test_multiline_selected_import_excludes_unselected_symbol );
      ( "text_compool_import_scanner_preserves_selected_order",
        test_text_compool_import_scanner_preserves_selected_order );
      ( "selected_import_hint_does_not_suppress_unselected_symbol",
        test_selected_import_hint_does_not_suppress_unselected_symbol );
      ( "direct_missing_custom_type_still_hints",
        test_direct_missing_custom_type_still_hints );
      ( "code_action_add_compool_for_type_hint_after_start",
        test_code_action_add_compool_for_type_hint_after_start );
      ( "code_action_add_compool_near_existing_import",
        test_code_action_add_compool_near_existing_import );
      ( "code_action_add_compool_avoids_duplicate_import",
        test_code_action_add_compool_avoids_duplicate_import );
      ("codelens_simple_counts", test_codelens_simple_counts);
      ("codelens_authoritative_title", test_codelens_authoritative_title);
      ("codelens_compool_importer_count", test_codelens_compool_importer_count);
      ( "explain_symbol_resolution_json_shape",
        test_explain_symbol_resolution_json_shape );
      ( "explain_symbol_resolution_fallback_path_visible",
        test_explain_symbol_resolution_fallback_path_visible );
      ( "inlay_hints_proc_args_and_type_details",
        test_inlay_hints_proc_args_and_type_details );
      ( "inlay_hints_range_uses_top_level_proc_sig_after_range",
        test_inlay_hints_range_uses_top_level_proc_sig_after_range );
      ( "diagnostic_authority_local_unresolved_error",
        test_diagnostic_authority_local_unresolved_error );
      ( "diagnostic_authority_imported_warmup_provisional",
        test_diagnostic_authority_imported_warmup_provisional );
      ( "diagnostic_authority_imported_ready_error",
        test_diagnostic_authority_imported_ready_error );
      ( "open_doc_installs_semantic_nav_snapshot",
        test_open_doc_installs_semantic_nav_snapshot );
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
      ( "status_size_and_colon_call_syntax_parse",
        test_status_size_and_colon_call_syntax_parse );
      ( "unresolved_all_import_does_not_hide_random_local",
        test_unresolved_all_import_does_not_hide_random_local );
      ( "proc_call_argument_count_and_type_diagnostics",
        test_proc_call_argument_count_and_type_diagnostics );
      ( "open_doc_owner_survives_source_set_replacement",
        test_open_doc_owner_survives_source_set_replacement );
      ("request_priority_dispatch_order", test_request_priority_dispatch_order);
      ("partial_references_stream", test_partial_references_stream);
      ("partial_workspace_symbols_stream", test_partial_workspace_symbols_stream);
      ("basic_navigation", test_basic_navigation);
      ("workspace_readiness_helpers", test_workspace_readiness_helpers);
      ( "workspace_navigation_compat_boundary",
        test_workspace_navigation_compat_boundary );
      ( "workspace_query_local_symbol_lookup",
        test_workspace_query_local_symbol_lookup );
      ( "workspace_query_facade_preserves_feature_results",
        test_workspace_query_facade_preserves_feature_results );
      ( "workspace_feature_split_completion_smoke",
        test_workspace_feature_split_completion_smoke );
      ( "large_stale_change_keeps_navigation",
        test_large_stale_change_keeps_navigation );
      ( "large_deferred_change_publishes_provisional_start_diag",
        test_large_deferred_change_publishes_provisional_start_diag );
      ( "workspace_budget_tiny_stops_scans",
        test_workspace_budget_tiny_stops_scans );
      ( "hover_body_cache_reuses_symbol_markdown",
        test_hover_body_cache_reuses_symbol_markdown );
      ( "literals_keywords_and_strings_are_not_hover_targets",
        test_literals_keywords_and_strings_are_not_hover_targets );
      ( "semantic_graph_stable_ids_from_defs",
        test_semantic_graph_stable_ids_from_defs );
      ( "jovial_type_model_display_and_compatibility",
        test_jovial_type_model_display_and_compatibility );
      ( "compile_time_table_dimension_integer_expression",
        test_compile_time_table_dimension_integer_expression );
      ( "compile_time_bit_char_size_constant_reference",
        test_compile_time_bit_char_size_constant_reference );
      ( "compile_time_bad_nonconstant_required_context",
        test_compile_time_bad_nonconstant_required_context );
      ( "compile_time_unknown_does_not_overdiagnose",
        test_compile_time_unknown_does_not_overdiagnose );
      ( "implementation_config_compile_time_type_and_layout",
        test_implementation_config_compile_time_type_and_layout );
      ( "system_subroutine_config_suppresses_unresolved_and_hovers",
        test_system_subroutine_config_suppresses_unresolved_and_hovers );
      ( "jovial_typecheck_integer_float_requires_conversion",
        test_jovial_typecheck_integer_float_requires_conversion );
      ( "jovial_typecheck_explicit_conversion_suppresses_mismatch",
        test_jovial_typecheck_explicit_conversion_suppresses_mismatch );
      ( "jovial_typecheck_bit_length_propagation",
        test_jovial_typecheck_bit_length_propagation );
      ( "jovial_typecheck_fixed_display_and_mixing",
        test_jovial_typecheck_fixed_display_and_mixing );
      ( "jovial_typecheck_typed_pointer_dereference",
        test_jovial_typecheck_typed_pointer_dereference );
      ( "jovial_typecheck_undefined_named_type_diagnostic",
        test_jovial_typecheck_undefined_named_type_diagnostic );
      ( "jovial_status_declaration_metadata",
        test_jovial_status_declaration_metadata );
      ( "jovial_status_assignment_valid_invalid",
        test_jovial_status_assignment_valid_invalid );
      ( "jovial_status_hover_goto_references",
        test_jovial_status_hover_goto_references );
      ( "jovial_status_duplicate_and_ambiguous_diagnostics",
        test_jovial_status_duplicate_and_ambiguous_diagnostics );
      ( "constant_table_parsing_and_metadata",
        test_constant_table_parsing_and_metadata );
      ( "constant_table_hover_and_readonly_assignment",
        test_constant_table_hover_and_readonly_assignment );
      ("constant_table_symbols", test_constant_table_symbols);
      ("inline_and_readonly_parsing", test_inline_and_readonly_parsing);
      ( "inline_readonly_hover_and_diagnostics",
        test_inline_readonly_hover_and_diagnostics );
      ("jovial_type_metadata_display", test_jovial_type_metadata_display);
      ( "semantic_scope_resolution_shadowing",
        test_semantic_scope_resolution_shadowing );
      ( "semantic_scope_duplicate_same_scope",
        test_semantic_scope_duplicate_same_scope );
      ( "semantic_scope_labels_are_procedure_local",
        test_semantic_scope_labels_are_procedure_local );
      ( "semantic_scope_table_and_block_field_ownership",
        test_semantic_scope_table_and_block_field_ownership );
      ("semantic_scope_skeleton_fallback", test_semantic_scope_skeleton_fallback);
      ( "scope_shadowing_prefers_innermost",
        test_scope_shadowing_prefers_innermost );
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
      ( "table_field_owner_navigation_and_hover",
        test_table_field_owner_navigation_and_hover );
      ( "table_invalid_dimension_diagnostics",
        test_table_invalid_dimension_diagnostics );
      ( "table_preset_mismatch_diagnostics",
        test_table_preset_mismatch_diagnostics );
      ( "table_preset_omitted_values_are_allowed",
        test_table_preset_omitted_values_are_allowed );
      ( "specified_table_w_parsing_and_pos",
        test_specified_table_w_parsing_and_pos );
      ("specified_table_hover", test_specified_table_hover);
      ("specified_table_diagnostics", test_specified_table_diagnostics);
      ( "table_layout_non_overlap_specified_table",
        test_table_layout_non_overlap_specified_table );
      ( "table_layout_overlap_and_exceeds_diagnostics",
        test_table_layout_overlap_and_exceeds_diagnostics );
      ( "table_layout_unknown_values_no_false_errors",
        test_table_layout_unknown_values_no_false_errors );
      ("overlay_parsing", test_overlay_parsing);
      ( "overlay_hover_and_document_symbol",
        test_overlay_hover_and_document_symbol );
      ("overlay_diagnostics", test_overlay_diagnostics);
      ( "procedure_body_change_keeps_graph_clean",
        test_procedure_body_change_keeps_graph_clean );
      ("declaration_change_dirties_graph", test_declaration_change_dirties_graph);
      ( "compool_export_change_revalidates_importers",
        test_compool_export_change_revalidates_importers );
      ( "compool_whitespace_change_prunes_importer_invalidation",
        test_compool_whitespace_change_prunes_importer_invalidation );
      ( "compool_private_body_decl_prunes_importer_invalidation",
        test_compool_private_body_decl_prunes_importer_invalidation );
      ( "compool_public_type_change_revalidates_importers",
        test_compool_public_type_change_revalidates_importers );
      ( "compool_import_change_invalidates_graph",
        test_compool_import_change_invalidates_graph );
      ( "imported_compool_symbol_prefers_semantic_path",
        test_imported_compool_symbol_prefers_semantic_path );
      ( "imported_compool_proc_prefers_summary_path",
        test_imported_compool_proc_prefers_summary_path );
      ( "broken_compool_summary_result_is_provisional",
        test_broken_compool_summary_result_is_provisional );
      ( "cross_module_reference_budget_still_stops",
        test_cross_module_reference_budget_still_stops );
      ("icopy_include_reverse_dependency", test_icopy_include_reverse_dependency);
      ( "icopy_debug_report_exposes_includes",
        test_icopy_debug_report_exposes_includes );
      ( "icopy_unresolved_include_diagnostic",
        test_icopy_unresolved_include_diagnostic );
      ("icopy_cyclic_include_diagnostic", test_icopy_cyclic_include_diagnostic);
      ("icopy_source_map_json_roundtrip", test_icopy_source_map_json_roundtrip);
      ("quoted_identifier_navigation", test_quoted_identifier_navigation);
      ( "macro_rename_updates_define_and_uses",
        test_macro_rename_updates_define_and_uses );
      ( "macro_rename_rejects_generated_actual_symbol",
        test_macro_rename_rejects_generated_actual_symbol );
      ("field_rename_respects_owner", test_field_rename_respects_owner);
      ( "field_rename_rejects_ambiguous_fallback",
        test_field_rename_rejects_ambiguous_fallback );
      ("file_modes_and_huge_policy", test_file_modes_and_huge_policy);
      ( "open_doc_readiness_targets_reset_per_open",
        test_open_doc_readiness_targets_reset_per_open );
      ( "debug_scheduler_memory_json_shape",
        test_debug_scheduler_memory_json_shape );
      ( "large_startup_readiness_allows_incremental_drain",
        test_large_startup_readiness_allows_incremental_drain );
      ( "large_critical_pressure_initializes_quick_nav_seed",
        test_large_critical_pressure_initializes_quick_nav_seed );
      ( "persistent_workspace_index_roundtrip",
        test_persistent_workspace_index_roundtrip );
      ( "persistent_cache_source_index_invalidation_and_corrupt",
        test_persistent_cache_source_index_invalidation_and_corrupt );
      ( "persistent_cache_skeleton_roundtrip_and_startup_hydrate",
        test_persistent_cache_skeleton_roundtrip_and_startup_hydrate );
      ( "persistent_cache_skeleton_buffered_flush",
        test_persistent_cache_skeleton_buffered_flush );
      ( "persistent_cache_module_summary_roundtrip_and_corrupt",
        test_persistent_cache_module_summary_roundtrip_and_corrupt );
      ( "persistent_cache_module_summary_invalidation_and_body_edit",
        test_persistent_cache_module_summary_invalidation_and_body_edit );
      ( "persistent_cache_warm_start_summary_definition_provisional",
        test_persistent_cache_warm_start_summary_definition_provisional );
      ("skeleton_snapshot_ide_query", test_skeleton_snapshot_ide_query);
    ]
  in
  let selected = List.tl (Array.to_list Sys.argv) in
  List.iter
    (fun (name, f) ->
      if selected = [] || List.mem name selected then run name f)
    tests
