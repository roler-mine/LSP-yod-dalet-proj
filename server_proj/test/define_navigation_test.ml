module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let uri_of_string_exn (s:string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_string s with
  | Some u -> u
  | None -> failf "invalid URI: %s" s

let find_nth_substring (s:string) ~(needle:string) ~(nth:int) : int =
  let n = String.length s in
  let m = String.length needle in
  if m = 0 then failf "needle must be non-empty";
  let rec seek_from i found =
    if i + m > n then failf "substring %S occurrence #%d not found" needle nth
    else if String.sub s i m = needle then
      if found = nth then i else seek_from (i + 1) (found + 1)
    else
      seek_from (i + 1) found
  in
  seek_from 0 0

let position_of_offset (s:string) (off:int) : T.Position.t =
  if off < 0 || off > String.length s then failf "offset out of bounds: %d" off;
  let rec loop i line col =
    if i >= off then ({ line; character = col } : T.Position.t)
    else if s.[i] = '\n' then loop (i + 1) (line + 1) 0
    else loop (i + 1) line (col + 1)
  in
  loop 0 0 0

let json_assoc_find (k:string) (fields:(string * Yojson.Safe.t) list) : Yojson.Safe.t =
  match List.assoc_opt k fields with
  | Some v -> v
  | None -> failf "missing JSON key %S" k

let line_of_first_definition_location (j:Yojson.Safe.t) : int =
  match j with
  | `List (`Assoc fields :: _) ->
      (match json_assoc_find "range" fields with
       | `Assoc rf ->
           (match json_assoc_find "start" rf with
            | `Assoc sf ->
                (match json_assoc_find "line" sf with
                 | `Int line -> line
                 | _ -> failf "unexpected JSON type for range.start.line")
            | _ -> failf "unexpected JSON type for range.start")
       | _ -> failf "unexpected JSON type for range")
  | _ ->
      failf "unexpected definition JSON shape"

let hover_markdown_value (j:Yojson.Safe.t) : string =
  match j with
  | `Assoc fields ->
      (match json_assoc_find "contents" fields with
       | `Assoc c ->
           (match json_assoc_find "value" c with
            | `String s -> s
            | _ -> failf "unexpected JSON type for contents.value")
       | _ -> failf "unexpected JSON type for contents")
  | _ ->
      failf "unexpected hover JSON shape"

let expect_eq_int ~(name:string) ~(got:int) ~(want:int) : unit =
  if got <> want then failf "%s: expected %d, got %d" name want got

let expect_contains ~(name:string) ~(haystack:string) ~(needle:string) : unit =
  let rec has_at i =
    let n = String.length haystack in
    let m = String.length needle in
    i + m <= n
    && (String.sub haystack i m = needle || has_at (i + 1))
  in
  if not (has_at 0) then
    failf "%s: expected substring %S in %S" name needle haystack

let source_text =
  String.concat "\n"
    [
      "START";
      "DEFINE TWELVE \"12\";";
      "DEFINE TWELVE(A) \"12$A\";";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  ITEM NUM U 6;";
      "  NUM = TWELVE;";
      "  NUM = TWELVE(1);";
      "  NUM = I'AM'TWELVE;";
      "END";
      "TERM";
      "";
    ]

let () =
  let ws = Lib.Workspace.create () in
  let uri = uri_of_string_exn "file:///define-navigation-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let def_line_obj = 1 in
  let def_line_call = 2 in

  let pos_usage_obj =
    find_nth_substring source_text ~needle:"TWELVE" ~nth:2
    |> fun off -> position_of_offset source_text (off + 1)
  in
  let pos_usage_call =
    find_nth_substring source_text ~needle:"TWELVE" ~nth:3
    |> fun off -> position_of_offset source_text (off + 1)
  in
  let pos_usage_injected =
    find_nth_substring source_text ~needle:"TWELVE" ~nth:4
    |> fun off -> position_of_offset source_text (off + 1)
  in

  let def_obj = Lib.Workspace.definition_json_for ws ~uri ~pos:pos_usage_obj in
  let def_call = Lib.Workspace.definition_json_for ws ~uri ~pos:pos_usage_call in
  let def_injected = Lib.Workspace.definition_json_for ws ~uri ~pos:pos_usage_injected in

  expect_eq_int
    ~name:"goto object-like DEFINE"
    ~got:(line_of_first_definition_location def_obj)
    ~want:def_line_obj;
  expect_eq_int
    ~name:"goto call-like DEFINE"
    ~got:(line_of_first_definition_location def_call)
    ~want:def_line_call;
  expect_eq_int
    ~name:"goto DEFINE inside larger identifier"
    ~got:(line_of_first_definition_location def_injected)
    ~want:def_line_obj;

  let hover_injected = Lib.Workspace.hover_json_for ws ~uri ~pos:pos_usage_injected in
  let hover_text = hover_markdown_value hover_injected in
  expect_contains ~name:"hover define title" ~haystack:hover_text ~needle:"define `TWELVE`";
  expect_contains ~name:"hover define expansion" ~haystack:hover_text ~needle:"Expands to: `12`";

  print_endline "define_navigation_test: ok"
