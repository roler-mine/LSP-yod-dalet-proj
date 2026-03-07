module Lib = Jovial_lsp_lib
module T = Lsp.Types

let failf fmt = Printf.ksprintf failwith fmt

let getenv_int (name : string) ~(default : int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> ( try int_of_string (String.trim raw) with _ -> default)

let mk_temp_dir (prefix : string) : string =
  let root = Filename.get_temp_dir_name () in
  let rec pick attempts =
    if attempts <= 0 then
      failf "failed to create temporary directory for %s" prefix;
    let name =
      Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ())
        (Random.bits () land 0xFFFFFF)
    in
    let path = Filename.concat root name in
    try
      Unix.mkdir path 0o755;
      path
    with _ -> pick (attempts - 1)
  in
  pick 32

let write_text (path : string) (text : string) : unit =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let uri_of_path (path : string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_path path with
  | Some u -> u
  | None -> failf "failed to convert path to uri: %s" path

let has_error_diag (diags : T.Diagnostic.t list) : bool =
  List.exists
    (fun (d : T.Diagnostic.t) ->
      match d.severity with
      | Some T.DiagnosticSeverity.Error -> true
      | _ -> false)
    diags

let find_update_for_uri (updates : (T.DocumentUri.t * T.Diagnostic.t list) list)
    ~(uri : T.DocumentUri.t) : T.Diagnostic.t list option =
  let target = Lib.Uri_path.docuri_to_string uri in
  updates
  |> List.find_map (fun (u, diags) ->
      if Lib.Uri_path.docuri_to_string u = target then Some diags else None)

let queue_background_reparse (ws : Lib.Workspace.t) ~(path : string) : unit =
  Lib.Workspace.apply_watched_file_changes ws ~changes:[ (path, `Changed) ];
  Lib.Workspace.background_tick ws ~budget_ms:400 ~mode:Lib.Workspace.BgTickIdle
    ~idle_quiet_ms:0 ~last_message_ms:0.0

let () =
  Random.self_init ();
  let hard_timeout_s =
    float_of_int
      (max 1 (getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget (phase : string) : unit =
    let left = hard_timeout_s -. (Unix.gettimeofday () -. started) in
    if left <= 0.0 then
      failf
        "workspace diagnostics latest-wins test exceeded hard timeout (%.1fs) \
         at %s"
        hard_timeout_s phase
  in

  ensure_budget "before setup";
  let root = mk_temp_dir "jovial-lsp-diag-latest-wins" in
  let broken_path = Filename.concat root "BROKEN.j73" in
  let broken_uri = uri_of_path broken_path in
  let broken_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC BROKEN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = UNKNOWN'ERR;";
        "END";
        "TERM";
        "";
      ]
  in
  let fixed_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC BROKEN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = X + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  write_text broken_path broken_text;

  let ws = Lib.Workspace.create () in
  Lib.Workspace.set_root_path ws (Some root);
  Lib.Workspace.rescan ws;
  ensure_budget "after setup";

  (* Baseline: broken file must produce at least one error diagnostic. *)
  queue_background_reparse ws ~path:broken_path;
  ensure_budget "after baseline reparse";
  let baseline = Lib.Workspace.drain_pending_diag_updates ws ~max_items:64 in
  let baseline_diags =
    match find_update_for_uri baseline ~uri:broken_uri with
    | Some ds -> ds
    | None ->
        failf "baseline diagnostics update for broken file was not published"
  in
  if not (has_error_diag baseline_diags) then
    failf
      "expected baseline broken file diagnostics to include at least one error";

  (* Coalescing check: enqueue an old (broken) update, then overwrite with fixed content
     before draining. Drained payload must reflect the newest state (empty diagnostics). *)
  queue_background_reparse ws ~path:broken_path;
  write_text broken_path fixed_text;
  queue_background_reparse ws ~path:broken_path;
  ensure_budget "after overwrite reparse";

  let updates = Lib.Workspace.drain_pending_diag_updates ws ~max_items:64 in
  let final_diags =
    match find_update_for_uri updates ~uri:broken_uri with
    | Some ds -> ds
    | None -> failf "final diagnostics update for broken file was not published"
  in
  if final_diags <> [] then
    failf
      "expected latest-wins diagnostics payload to be empty after fix, got %d \
       diagnostics"
      (List.length final_diags);

  ensure_budget "before exit";
  print_endline "lsp_workspace_diagnostics_latest_wins_test: ok"
