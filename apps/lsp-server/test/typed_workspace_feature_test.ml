module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let uri_of_string_exn (s:string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_string s with
  | Some u -> u
  | None -> failf "invalid URI: %s" s

let find_substring (s:string) ~(needle:string) : int =
  let n = String.length s in
  let m = String.length needle in
  let rec loop i =
    if i + m > n then failf "substring %S not found" needle
    else if String.sub s i m = needle then i
    else loop (i + 1)
  in
  loop 0

let position_of_offset (s:string) (off:int) : T.Position.t =
  let rec loop i line col =
    if i >= off then ({ line; character = col } : T.Position.t)
    else if s.[i] = '\n' then loop (i + 1) (line + 1) 0
    else loop (i + 1) line (col + 1)
  in
  loop 0 0 0

let source_text =
  String.concat "\n"
    [
      "START";
      "DEF PROC ADD(A, B);";
      "BEGIN";
      "  RETURN A + B;";
      "END";
      "DEF PROC MAIN;";
      "BEGIN";
      "  ITEM RESULT U 4;";
      "  RESULT = ADD(1, 2);";
      "END";
      "TERM";
      "";
    ]

let () =
  let ws = Lib.Workspace.create () in
  let uri = uri_of_string_exn "file:///typed-workspace-feature-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let symbols = Lib.Workspace.document_symbols_for ws ~uri in
  if not (List.exists (function `DocumentSymbol _ -> true | _ -> false) symbols) then
    failf "expected typed document symbols";

  let completion_pos =
    find_substring source_text ~needle:"RESULT = "
    |> fun off -> position_of_offset source_text (off + String.length "RESULT = ")
  in
  let completions = Lib.Workspace.completion_items_for ws ~uri ~pos:completion_pos in
  if not (List.exists (fun item -> item.T.CompletionItem.label = "ADD") completions) then
    failf "expected ADD completion item";

  let range =
    let start = position_of_offset source_text (find_substring source_text ~needle:"ADD(1, 2)") in
    let end_ = position_of_offset source_text (String.length source_text) in
    { T.Range.start; end_ }
  in
  let hints = Lib.Workspace.inlay_hints_for ws ~uri ~range in
  let labels =
    hints
    |> List.map (fun hint ->
         match hint.T.InlayHint.label with
         | `String s -> s
         | `List _ -> "")
  in
  if not (List.mem "A:" labels && List.mem "B:" labels) then
    failf "expected typed inlay hints for ADD arguments";

  print_endline "typed_workspace_feature_test: ok"
