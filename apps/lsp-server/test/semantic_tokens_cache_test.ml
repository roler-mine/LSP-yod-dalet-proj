module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let uri_of_string_exn (s:string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_string s with
  | Some u -> u
  | None -> failf "invalid URI: %s" s

let data_of_tokens (tokens:T.SemanticTokens.t option) : int array =
  match tokens with
  | Some payload -> payload.data
  | None -> failf "semantic tokens payload missing"

let source_text =
  String.concat "\n"
    [
      "START";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  ITEM VALUE U 4;";
      "  VALUE = VALUE + 1;";
      "END";
      "TERM";
      "";
    ]

let () =
  let ws = Lib.Workspace.create () in
  let uri = uri_of_string_exn "file:///semantic-tokens-cache-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let t1 = Lib.Workspace.semantic_tokens_full_for ws ~uri in
  let t2 = Lib.Workspace.semantic_tokens_full_for ws ~uri in
  let d1 = data_of_tokens t1 in
  let d2 = data_of_tokens t2 in
  if d1 <> d2 then
    failf "semantic token payload should be stable across repeated full requests";

  print_endline "semantic_tokens_cache_test: ok"
