module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let uri_of_string_exn (s : string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_string s with
  | Some u -> u
  | None -> failf "invalid URI: %s" s

let find_nth_substring (s : string) ~(needle : string) ~(nth : int) : int =
  let n = String.length s in
  let m = String.length needle in
  if m = 0 then failf "needle must be non-empty";
  let rec seek_from i found =
    if i + m > n then failf "substring %S occurrence #%d not found" needle nth
    else if String.sub s i m = needle then
      if found = nth then i else seek_from (i + 1) (found + 1)
    else seek_from (i + 1) found
  in
  seek_from 0 0

let position_of_offset (s : string) (off : int) : T.Position.t =
  if off < 0 || off > String.length s then failf "offset out of bounds: %d" off;
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
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  ITEM COUNTER U 4;";
      "  COUNTER = COUNTER + 1;";
      "END";
      "TERM";
      "";
    ]

let () =
  let ws = Lib.Workspace.create () in
  let uri = uri_of_string_exn "file:///semantic-store-rename-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let pos_counter =
    find_nth_substring source_text ~needle:"COUNTER" ~nth:1 |> fun off ->
    position_of_offset source_text (off + 1)
  in

  let prep = Lib.Workspace.prepare_rename_for ws ~uri ~pos:pos_counter in
  (match prep with
  | Some (`RangeWithPlaceholder _) -> ()
  | Some (`Range _) -> ()
  | None -> failf "prepareRename should return a range");

  let rename =
    Lib.Workspace.rename_for ws ~uri ~pos:pos_counter ~new_name:"COUNT2"
  in
  let has_changes =
    match rename with
    | Some { T.WorkspaceEdit.changes = Some xs; _ } -> xs <> []
    | _ -> false
  in
  if not has_changes then failf "rename should produce workspace edits";

  print_endline "semantic_store_rename_test: ok"
