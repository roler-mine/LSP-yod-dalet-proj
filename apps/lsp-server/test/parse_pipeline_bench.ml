(* Module overview: Test support and regression coverage for parse pipeline bench. *)

module T = Lsp.Types
module Lib = Jovial_lsp_lib

let now_ms () = Unix.gettimeofday () *. 1000.0

let time label f =
  Gc.compact ();
  let before = Gc.quick_stat () in
  let t0 = now_ms () in
  let result = f () in
  let t1 = now_ms () in
  let after = Gc.quick_stat () in
  let alloc_words =
    after.minor_words +. after.major_words
    -. before.minor_words -. before.major_words
  in
  Printf.printf "%-28s %9.2f ms  %12.0f words\n%!" label (t1 -. t0)
    alloc_words;
  result

let synthetic_source ?(with_define = true) target_tokens =
  let b = Buffer.create (target_tokens * 8) in
  Buffer.add_string b "START\n";
  if with_define then Buffer.add_string b "DEFINE TEN \"10\";\n";
  Buffer.add_string b "DEF PROC MAIN RENT;\nBEGIN\n";
  let rec decls i =
    if i <= 0 then ()
    else (
      Buffer.add_string b "ITEM V";
      Buffer.add_string b (string_of_int i);
      Buffer.add_string b " U 6;\n";
      decls (i - 1))
  in
  let rec stmts i =
    if i <= 0 then ()
    else (
      Buffer.add_string b "V";
      Buffer.add_string b (string_of_int i);
      Buffer.add_string b " = TEN + ";
      Buffer.add_string b (string_of_int i);
      Buffer.add_string b ";\n";
      stmts (i - 1))
  in
  let rows = max 1 (target_tokens / 12) in
  decls rows;
  stmts rows;
  Buffer.add_string b "END\nTERM\n";
  Buffer.contents b

let uri_of_size n =
  match Lib.Uri_path.docuri_of_string (Printf.sprintf "file:///bench-%d.j73" n) with
  | Some uri -> uri
  | None -> failwith "failed to build benchmark URI"

let position_of_second_ten text =
  let position_of_offset off =
    let rec loop i line character =
      if i >= off then ({ T.Position.line; character } : T.Position.t)
      else if text.[i] = '\n' then loop (i + 1) (line + 1) 0
      else loop (i + 1) line (character + 1)
    in
    loop 0 0 0
  in
  match Str.search_forward (Str.regexp "TEN") text 0 with
  | first ->
      let second = Str.search_forward (Str.regexp "TEN") text (first + 1) in
      position_of_offset second
  | exception Not_found -> ({ T.Position.line = 0; character = 0 } : T.Position.t)

let run_one n =
  Printf.printf "\n== synthetic target: %d tokens ==\n%!" n;
  let text = synthetic_source n in
  Printf.printf "bytes: %d\n%!" (String.length text);
  let raw =
    time "lex raw" (fun () -> Lib.Preprocess.lex_all_tokens ~file:None ~text)
  in
  let preprocess, expanded_changed =
    time "preprocess" (fun () ->
        Lib.Preprocess.run_from_tokens ~file:None ~text ~tokens:raw)
  in
  let expanded =
    time "lex expanded" (fun () ->
        if expanded_changed then
          Lib.Preprocess.lex_all_tokens ~file:None ~text:preprocess.text
        else raw)
  in
  ignore
    (time "parse interactive" (fun () ->
         Lib.Parser.parse_tokens ~file:None ~dump_ast:false
           ~profile:Lib.Parser.Interactive ~tokens:expanded));
  ignore
    (time "parse background" (fun () ->
         Lib.Parser.parse_tokens ~file:None ~dump_ast:false
           ~profile:Lib.Parser.Background ~tokens:expanded));
  ignore
    (time "syntax cache" (fun () ->
         Lib.Syntax_cache.build_with_profile ~profile:Lib.Parser.Batch ~file:None
           ~text ()));
  let incremental_text = synthetic_source ~with_define:false n in
  let edit_start =
    try
      Str.search_forward (Str.regexp "TEN") incremental_text
        (String.length incremental_text / 2)
    with Not_found -> Str.search_forward (Str.regexp "TEN") incremental_text 0
  in
  let edited =
    String.sub incremental_text 0 edit_start ^ "ONE"
    ^ String.sub incremental_text (edit_start + 3)
        (String.length incremental_text - edit_start - 3)
  in
  let previous =
        Lib.Syntax_cache.build_with_profile ~profile:Lib.Parser.Interactive
      ~file:None ~text:incremental_text ()
  in
  let edit_summary : Lib.Syntax_cache.edit_summary =
    {
      full_sync = false;
      start_off = edit_start;
      old_end_off = edit_start + 3;
      new_end_off = edit_start + 3;
      inserted_chars = 3;
      change_count = 1;
    }
  in
  let updated =
    time "syntax cache edited" (fun () ->
        Lib.Syntax_cache.build_with_profile ~previous ~edit_summary
          ~profile:Lib.Parser.Interactive ~file:None ~text:edited ())
  in
  Printf.printf
    "%-28s prefix=%d suffix=%d old=%d new=%d fallback=%s\n%!"
    "token reuse"
    updated.Lib.Syntax_cache.token_reuse.reused_prefix_tokens
    updated.Lib.Syntax_cache.token_reuse.reused_suffix_tokens
    updated.Lib.Syntax_cache.token_reuse.old_token_count
    updated.Lib.Syntax_cache.token_reuse.new_token_count
    (Option.value updated.Lib.Syntax_cache.token_reuse.fallback_reason
       ~default:"none");
  Printf.printf "%-28s checkpoints=%d reused=%b fallback=%s\n%!"
    "checkpoint stats"
    updated.Lib.Syntax_cache.checkpoint_stats.checkpoint_count
    updated.Lib.Syntax_cache.checkpoint_stats.checkpoint_reused
    (Option.value updated.Lib.Syntax_cache.checkpoint_stats.fallback_reason
       ~default:"none");
  let ws = Lib.Workspace.create () in
  let uri = uri_of_size n in
  time "workspace open" (fun () ->
      Lib.Workspace.open_doc ws ~uri ~file:None ~text);
  ignore
    (time "semantic tokens full" (fun () ->
         Lib.Workspace.semantic_tokens_full_for ws ~uri));
  ignore
    (time "definition lookup" (fun () ->
         Lib.Workspace.definition_locations_for ws ~uri
           ~pos:(position_of_second_ten text)))

let run_menhir_codegen_probe () =
  let cwd = Sys.getcwd () in
  let parser_mly = Filename.concat cwd "lib/syntax/parser.mly" in
  let temp =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "jovial-menhir-codegen-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir temp 0o755 with _ -> ());
  let command =
    Printf.sprintf "cd %s && menhir --infer --code --base parser_fast %s"
      (Filename.quote temp) (Filename.quote parser_mly)
  in
  Printf.printf "\n== menhir code backend generation probe ==\n%!";
  let status = time "menhir --infer --code" (fun () -> Sys.command command) in
  Printf.printf "exit status: %d\n%!" status

let has_arg flag = Array.exists (( = ) flag) Sys.argv

let arg_value flag =
  let rec loop i =
    if i >= Array.length Sys.argv - 1 then None
    else if Sys.argv.(i) = flag then Some Sys.argv.(i + 1)
    else loop (i + 1)
  in
  loop 1

let parser_health_label = function
  | Lib.Parser.ParseClean -> "clean"
  | Lib.Parser.ParseRecovered -> "recovered"
  | Lib.Parser.ParsePartial -> "partial"
  | Lib.Parser.ParseSkeletonOnly -> "skeleton"
  | Lib.Parser.ParseLexicalOnly -> "lexical"
  | Lib.Parser.ParseFailedInternal -> "internal"

let diagnostic_message (diag : T.Diagnostic.t) =
  match diag.T.Diagnostic.message with
  | `String s -> s
  | `MarkupContent mc -> mc.value

let diagnostic_line (diag : T.Diagnostic.t) =
  diag.T.Diagnostic.range.T.Range.start.T.Position.line + 1

let has_suffix ~suffix s =
  let n = String.length s and m = String.length suffix in
  n >= m && String.lowercase_ascii (String.sub s (n - m) m) = suffix

let rec collect_jovial_files root =
  let entries =
    try Sys.readdir root |> Array.to_list with Sys_error _ -> []
  in
  entries
  |> List.concat_map (fun name ->
         let path = Filename.concat root name in
         if name = ".jovial_ls" || name = ".git" || name = "_build" then []
         else if Sys.is_directory path then collect_jovial_files path
         else if
           List.exists (fun suffix -> has_suffix ~suffix path)
             [ ".j73"; ".jov"; ".jovial"; ".j" ]
         then [ path ]
         else [])

let rec collect_assembly_files root =
  let entries =
    try Sys.readdir root |> Array.to_list with Sys_error _ -> []
  in
  entries
  |> List.concat_map (fun name ->
         let path = Filename.concat root name in
         if name = ".jovial_ls" || name = ".git" || name = "_build" then []
         else if Sys.is_directory path then collect_assembly_files path
         else if has_suffix ~suffix:".asm" path then [ path ]
         else [])

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let bad_token_count (tokens : Lib.Preprocess.lex_tok array option) =
  match tokens with
  | None -> 0
  | Some toks ->
      Array.fold_left
        (fun acc (span : Lib.Preprocess.lex_tok) ->
          match span.tok with
          | Lib.Parser.BAD_CHAR _ | Lib.Parser.BAD_STRING _
          | Lib.Parser.BAD_COMMENT _ | Lib.Parser.BAD_DIRECTIVE _
          | Lib.Parser.BAD_LITERAL _ ->
              acc + 1
          | _ -> acc)
        0 toks

let line_at ~(text : string) line =
  let lines = String.split_on_char '\n' text in
  if line <= 0 || line > List.length lines then ""
  else List.nth lines (line - 1)

let report_corpus_file ~verbose path =
  let text = read_file path in
  let cache =
    Lib.Syntax_cache.build_with_profile ~profile:Lib.Parser.Batch
      ~file:(Some path) ~text ()
  in
  let parse = cache.Lib.Syntax_cache.parse in
  let syntax_count = List.length parse.diags in
  let recovery_count = List.length parse.recovery_diags in
  let taint_count = List.length parse.tainted_ranges in
  let bad_tokens = bad_token_count cache.raw_tokens in
  let skeleton_symbols = List.length cache.skeleton.symbols in
  let first_diag =
    match parse.diags @ parse.recovery_diags with
    | [] -> "none"
    | diag :: _ ->
        Printf.sprintf "line %d: %s" (diagnostic_line diag)
          (diagnostic_message diag)
  in
  Printf.printf
    "%s\tbytes=%d\thealth=%s\tconfidence=%.2f\tsyntax=%d\trecovery=%d\ttaints=%d\tbad_tokens=%d\tskeleton_symbols=%d\tfirst=%s\n%!"
    path (String.length text) (parser_health_label parse.parse_health)
    parse.parse_confidence syntax_count recovery_count taint_count bad_tokens
    skeleton_symbols first_diag;
  if verbose && (syntax_count > 0 || recovery_count > 0) then (
    List.iter
      (fun diag ->
        let line = diagnostic_line diag in
        Printf.printf "  diag line %d: %s\n    source: %s\n%!" line
          (diagnostic_message diag) (line_at ~text line))
      (parse.diags @ parse.recovery_diags);
    List.iter
      (fun taint ->
        Printf.printf "  taint line %d penalty=%.2f semantic=%b: %s\n%!"
          taint.Lib.Parser.taint_loc.start_pos.line
          taint.taint_confidence_penalty taint.taint_allows_semantic
          taint.taint_reason)
      parse.tainted_ranges);
  (parse.parse_health, parse.parse_confidence, syntax_count + recovery_count)

let workspace_report_diagnostics root =
  let files = collect_jovial_files root |> List.sort String.compare in
  let asm_files = collect_assembly_files root |> List.sort String.compare in
  let ws = Lib.Workspace_state.create () in
  Lib.Workspace_state.set_root_path ws (Some root);
  ignore (Lib.Workspace_state.set_source_files ws files);
  ignore (Lib.Workspace_state.set_assembly_files ws asm_files);
  List.iter
    (fun path ->
      let text = read_file path in
      match Lib.Uri_path.docuri_of_path path with
      | None -> Printf.printf "bad-uri\t%s\n%!" path
      | Some uri ->
          Lib.Workspace_doc_lifecycle.open_doc ws ~uri ~file:(Some path) ~text)
    files;
  ignore (Lib.Workspace_doc_lifecycle.revalidate_all ws);
  List.iter
    (fun path ->
      match Lib.Uri_path.docuri_of_path path with
      | None -> ()
      | Some uri ->
          let diags = Lib.Workspace_state.diagnostics_for ws ~uri in
          Printf.printf "== %s diagnostics=%d ==\n%!" path (List.length diags);
          List.iter
            (fun diag ->
              Printf.printf "line %d:%d [%s] %s\n%!"
                (diag.T.Diagnostic.range.start.line + 1)
                (diag.T.Diagnostic.range.start.character + 1)
                (Option.value diag.T.Diagnostic.source ~default:"")
                (diagnostic_message diag))
            diags)
    files

let run_corpus root =
  let files = collect_jovial_files root |> List.sort String.compare in
  Printf.printf "== corpus: %s ==\nfiles: %d\n%!" root (List.length files);
  let totals =
    List.fold_left
      (fun (clean, recovered, low_conf, diag_files) path ->
        let health, confidence, diag_count =
          report_corpus_file ~verbose:(has_arg "--verbose") path
        in
        let clean =
          match health with Lib.Parser.ParseClean -> clean + 1 | _ -> clean
        in
        let recovered =
          match health with
          | Lib.Parser.ParseRecovered -> recovered + 1
          | _ -> recovered
        in
        let low_conf = if confidence < 0.80 then low_conf + 1 else low_conf in
        let diag_files = if diag_count > 0 then diag_files + 1 else diag_files in
        (clean, recovered, low_conf, diag_files))
      (0, 0, 0, 0) files
  in
  let clean, recovered, low_conf, diag_files = totals in
  Printf.printf
    "summary\tclean=%d\trecovered=%d\tlow_confidence=%d\tdiagnostic_files=%d\n%!"
    clean recovered low_conf diag_files

let () =
  if has_arg "--menhir-codegen" then
    run_menhir_codegen_probe ();
  match arg_value "--workspace-diags" with
  | Some root -> workspace_report_diagnostics root
  | None -> (
  match arg_value "--corpus" with
  | Some root -> run_corpus root
  | None ->
      let sizes =
        if has_arg "--quick" then [ 200 ]
        else if has_arg "--large" then [ 10_000; 100_000; 1_000_000 ]
        else [ 10_000 ]
      in
      List.iter run_one sizes)
