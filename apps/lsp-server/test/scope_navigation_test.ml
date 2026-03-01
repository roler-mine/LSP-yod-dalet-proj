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

let expect_eq_int ~(name:string) ~(got:int) ~(want:int) : unit =
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
      "DEF PROC BAR(INP);";
      "BEGIN";
      "  ITEM V U 1;";
      "  V = INP;";
      "END";
      "TERM";
      "";
    ]

let () =
  let ws = Lib.Workspace.create () in
  let uri = uri_of_string_exn "file:///scope-navigation-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let pos_inp_usage_in_bar =
    find_nth_substring source_text ~needle:"INP" ~nth:3
    |> fun off -> position_of_offset source_text (off + 1)
  in
  let pos_v_usage_in_bar =
    find_nth_substring source_text ~needle:"V" ~nth:3
    |> fun off -> position_of_offset source_text (off + 1)
  in

  let def_inp = Lib.Workspace.definition_json_for ws ~uri ~pos:pos_inp_usage_in_bar in
  let def_v = Lib.Workspace.definition_json_for ws ~uri ~pos:pos_v_usage_in_bar in

  expect_eq_int
    ~name:"BAR input parameter resolves in BAR scope"
    ~got:(line_of_first_definition_location def_inp)
    ~want:6;
  expect_eq_int
    ~name:"BAR local item resolves in BAR scope"
    ~got:(line_of_first_definition_location def_v)
    ~want:8;

  print_endline "scope_navigation_test: ok"
