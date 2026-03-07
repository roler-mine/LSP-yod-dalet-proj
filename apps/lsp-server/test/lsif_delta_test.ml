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

let bool_field (k : string) (fields : (string * Yojson.Safe.t) list) : bool =
  match List.assoc_opt k fields with
  | Some (`Bool b) -> b
  | _ -> failf "missing bool field %S" k

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
  let uri = uri_of_string_exn "file:///lsif-delta-test.j73" in
  Lib.Workspace.open_doc ws ~uri ~file:None ~text:source_text;

  let full = Lib.Workspace.lsif_index_json ws in
  let full_fields =
    match full with
    | `Assoc fields -> fields
    | _ -> failf "lsif_index_json should return object"
  in
  let rev = int_field "revision" full_fields in
  if rev < 0 then failf "revision must be non-negative";

  let delta_same = Lib.Workspace.lsif_delta_json ws ~base_revision:rev in
  let same_fields =
    match delta_same with
    | `Assoc fields -> fields
    | _ -> failf "lsif_delta_json should return object"
  in
  if bool_field "reset" same_fields then
    failf "delta should not reset when using the current base revision";

  let stale_base = if rev > 0 then rev - 1 else 0 in
  let delta_stale =
    Lib.Workspace.lsif_delta_json ws ~base_revision:stale_base
  in
  let stale_fields =
    match delta_stale with
    | `Assoc fields -> fields
    | _ -> failf "lsif_delta_json stale payload should return object"
  in
  if rev > 0 && not (bool_field "reset" stale_fields) then
    failf "delta should request reset for stale base revision";

  print_endline "lsif_delta_test: ok"
