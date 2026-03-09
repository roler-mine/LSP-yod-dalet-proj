module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let uri_of_string_exn (s : string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_string s with
  | Some u -> u
  | None -> failf "invalid URI: %s" s

let int_field (k : string) (fields : (string * Yojson.Safe.t) list) : int =
  match List.assoc_opt k fields with
  | Some (`Int n) -> n
  | Some (`Intlit s) -> int_of_string s
  | _ -> failf "missing int field %S" k

let bool_field_default (k : string) (fields : (string * Yojson.Safe.t) list)
    ~(default : bool) : bool =
  match List.assoc_opt k fields with Some (`Bool b) -> b | _ -> default

let list_field_default (k : string) (fields : (string * Yojson.Safe.t) list) :
    Yojson.Safe.t list =
  match List.assoc_opt k fields with Some (`List xs) -> xs | _ -> []

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
  let rec loop i line character =
    if i >= off then { T.Position.line; character }
    else if s.[i] = '\n' then loop (i + 1) (line + 1) 0
    else loop (i + 1) line (character + 1)
  in
  loop 0 0 0

let change_event_of_json (json : Yojson.Safe.t) :
    T.TextDocumentContentChangeEvent.t =
  try T.TextDocumentContentChangeEvent.t_of_yojson json
  with exn ->
    failf "invalid change event JSON: %s (%s)"
      (Yojson.Safe.to_string json)
      (Printexc.to_string exn)

let ranged_change ~(start_pos : T.Position.t) ~(end_pos : T.Position.t)
    ~(text : string) : T.TextDocumentContentChangeEvent.t =
  change_event_of_json
    (`Assoc
       [
         ( "range",
           `Assoc
             [
               ( "start",
                 `Assoc
                   [
                     ("line", `Int start_pos.line);
                     ("character", `Int start_pos.character);
                   ] );
               ( "end",
                 `Assoc
                   [
                     ("line", `Int end_pos.line);
                     ("character", `Int end_pos.character);
                   ] );
             ] );
         ("text", `String text);
       ])

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
  let uri = uri_of_string_exn "file:///lsif-noop-change-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let initial_payload = Lib.Workspace.lsif_index_json ws in
  let initial_fields =
    match initial_payload with
    | `Assoc fields -> fields
    | _ -> failf "lsif_index_json should return an object"
  in
  let initial_rev = int_field "revision" initial_fields in

  let value_off = find_nth_substring source_text ~needle:"VALUE" ~nth:0 in
  let start_pos = position_of_offset source_text value_off in
  let end_pos =
    position_of_offset source_text (value_off + String.length "VALUE")
  in
  let noop_change = ranged_change ~start_pos ~end_pos ~text:"VALUE" in
  Lib.Workspace.change_doc ws ~uri ~changes:[ noop_change ];

  let after_payload = Lib.Workspace.lsif_index_json ws in
  let after_fields =
    match after_payload with
    | `Assoc fields -> fields
    | _ -> failf "lsif_index_json after no-op should return an object"
  in
  let after_rev = int_field "revision" after_fields in
  if after_rev <> initial_rev then
    failf "no-op change should preserve LSIF revision (%d <> %d)" after_rev
      initial_rev;

  let delta_payload =
    Lib.Workspace.lsif_delta_json ws ~base_revision:initial_rev
  in
  let delta_fields =
    match delta_payload with
    | `Assoc fields -> fields
    | _ -> failf "lsif_delta_json should return an object"
  in
  if bool_field_default "reset" delta_fields ~default:true then
    failf "no-op change should not force an LSIF delta reset";
  if list_field_default "deletes" delta_fields <> [] then
    failf "no-op change should not emit LSIF deletes";
  if list_field_default "upserts" delta_fields <> [] then
    failf "no-op change should not emit LSIF upserts";

  print_endline "lsif_noop_change_test: ok"
