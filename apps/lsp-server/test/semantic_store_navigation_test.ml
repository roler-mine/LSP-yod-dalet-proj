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

let line_of_first_definition_location (locations : T.Location.t list) : int =
  match locations with
  | loc :: _ -> loc.range.start.line
  | [] -> failf "expected at least one definition location"

let expect_eq_int ~(name : string) ~(got : int) ~(want : int) : unit =
  if got <> want then failf "%s: expected %d, got %d" name want got

let source_text =
  String.concat "\n"
    [
      "START";
      "DEF PROC FOO(INP);";
      "BEGIN";
      "  ITEM V U 1;";
      "  V = INP;";
      "END";
      "TERM";
      "";
    ]

let () =
  let ws = Lib.Workspace.create () in
  let uri = uri_of_string_exn "file:///semantic-store-navigation-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let pos_inp_usage =
    find_nth_substring source_text ~needle:"INP" ~nth:1 |> fun off ->
    position_of_offset source_text (off + 1)
  in

  let def1 =
    Lib.Workspace.definition_locations_for ws ~uri ~pos:pos_inp_usage
  in
  let def2 =
    Lib.Workspace.definition_locations_for ws ~uri ~pos:pos_inp_usage
  in
  expect_eq_int ~name:"definition line first call"
    ~got:(line_of_first_definition_location def1)
    ~want:1;
  expect_eq_int ~name:"definition line second call"
    ~got:(line_of_first_definition_location def2)
    ~want:1;

  (match Lib.Workspace.perf_stats_json ws with
  | `Assoc _ -> ()
  | _ -> failf "perf stats should be an object");

  print_endline "semantic_store_navigation_test: ok"
