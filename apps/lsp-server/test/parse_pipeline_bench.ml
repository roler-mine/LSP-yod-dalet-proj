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

let () =
  if Array.exists (( = ) "--menhir-codegen") Sys.argv then
    run_menhir_codegen_probe ();
  let sizes =
    if Array.exists (( = ) "--large") Sys.argv then [ 10_000; 100_000; 1_000_000 ]
    else [ 10_000 ]
  in
  List.iter run_one sizes
