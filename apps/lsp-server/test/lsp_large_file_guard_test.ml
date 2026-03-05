module Lib = Jovial_lsp_lib
module T = Lsp.Types

let failf fmt = Printf.ksprintf failwith fmt

let getenv_int (name:string) ~(default:int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
      (try int_of_string (String.trim raw) with _ -> default)

let uri_of_path (path:string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_path path with
  | Some u -> u
  | None -> failf "failed to convert path to uri: %s" path

let has_guard_diag (diags:T.Diagnostic.t list) : bool =
  List.exists (fun (d:T.Diagnostic.t) ->
    let msg =
      match d.message with
      | `String s -> String.lowercase_ascii s
      | `MarkupContent mc ->
          String.lowercase_ascii
            (match mc.value with
             | s -> s)
    in
    let contains needle =
      let n = String.length msg in
      let m = String.length needle in
      let rec loop i =
        if i + m > n then false
        else if String.sub msg i m = needle then true
        else loop (i + 1)
      in
      if m = 0 then true else loop 0
    in
    contains "parse skipped" || contains "exceeds guard"
  ) diags

let () =
  Random.self_init ();
  let hard_timeout_s =
    float_of_int (max 1 (getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget (phase:string) : unit =
    if hard_timeout_s -. (Unix.gettimeofday () -. started) <= 0.0 then
      failf "large-file guard test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  ensure_budget "before setup";
  let root = Filename.get_temp_dir_name () in
  let file_path =
    Filename.concat root
      (Printf.sprintf "jovial-large-guard-%d.j73" (Unix.getpid ()))
  in
  let uri = uri_of_path file_path in
  let parse_cap =
    max 1024 (getenv_int "JOVIAL_PARSE_FILE_MAX_BYTES" ~default:(1024 * 1024))
  in
  let bytes = parse_cap + (256 * 1024) in
  let text = String.make bytes 'A' in

  let ws = Lib.Workspace.create () in
  Lib.Workspace.set_root_path ws (Some root);
  Lib.Workspace.rescan ws;
  Lib.Workspace.open_doc ws ~uri ~file:(Some file_path) ~text;
  ensure_budget "after open";

  let diags = Lib.Workspace.diagnostics_for ws ~uri in
  if diags = [] then
    failf "expected diagnostics for oversized file guard, got none";
  if not (has_guard_diag diags) then
    failf "expected parse guard diagnostic, got: %s"
      (Yojson.Safe.to_string (T.Diagnostic.yojson_of_t (List.hd diags)));

  print_endline "lsp_large_file_guard_test: ok"
