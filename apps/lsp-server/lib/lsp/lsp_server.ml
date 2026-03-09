module T = Lsp.Types
module Req = Lsp_request
module Resp = Lsp_response

let json_obj = Resp.json_obj

let get_assoc (j : Yojson.Safe.t) : (string * Yojson.Safe.t) list option =
  match j with `Assoc xs -> Some xs | _ -> None

let find_field (k : string) (xs : (string * Yojson.Safe.t) list) =
  List.assoc_opt k xs

let method_of_msg (j : Yojson.Safe.t) : string option =
  match get_assoc j with
  | None -> None
  | Some xs -> (
      match find_field "method" xs with Some (`String m) -> Some m | _ -> None)

let id_of_msg (j : Yojson.Safe.t) : Yojson.Safe.t option =
  match get_assoc j with None -> None | Some xs -> find_field "id" xs

let params_of_msg (j : Yojson.Safe.t) : Yojson.Safe.t =
  match get_assoc j with
  | None -> `Null
  | Some xs -> (
      match find_field "params" xs with Some p -> p | None -> `Null)

let is_request (j : Yojson.Safe.t) : bool =
  match id_of_msg j with Some _ -> true | None -> false

let respond = Resp.respond
let respond_error = Resp.respond_error
let notify = Resp.notify
let publish_diagnostics_if_changed = Resp.publish_diagnostics_if_changed

let revalidate_and_publish_all_open_docs (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) : unit =
  Workspace.revalidate_all ws
  |> List.iter (fun uri ->
      let diags = Workspace.diagnostics_for ws ~uri in
      let published =
        publish_diagnostics_if_changed published_diags oc ~uri ~diags
      in
      if published then (
        Perf_stats.tick "diag.open.publish";
        if Workspace.open_doc_converged ws ~uri then
          Perf_stats.tick "diag.open.authoritative_publish"
        else Perf_stats.tick "diag.open.provisional_publish");
      if published && diags = [] then Perf_stats.tick "diag.open.publish_empty")

let publish_doc_diagnostics (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(provisional : bool)
    ~(uri : T.DocumentUri.t) : unit =
  let diags = Workspace.diagnostics_for ws ~uri in
  let published =
    publish_diagnostics_if_changed published_diags oc ~uri ~diags
  in
  if published then (
    Perf_stats.tick "diag.open.publish";
    if provisional || not (Workspace.open_doc_converged ws ~uri) then
      Perf_stats.tick "diag.open.provisional_publish"
    else Perf_stats.tick "diag.open.authoritative_publish");
  if published && diags = [] then Perf_stats.tick "diag.open.publish_empty"

let initialize_result_json = Resp.initialize_result_json

let file_of_uri (u : T.DocumentUri.t) : string option =
  Uri_path.file_path_of_uri u

let zero_loc_for_uri (uri : T.DocumentUri.t) : Ast.Loc.t =
  let z = { Ast.Loc.line = 1; col = 0; offset = 0 } in
  Ast.Loc.make ~file:(file_of_uri uri) ~start_pos:z ~end_pos:z

let diag_internal_server_failure ~(uri : T.DocumentUri.t) ~(phase : string)
    (exn : exn) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error ~source:"server"
    ~message:
      (Printf.sprintf
         "Internal failure during %s: %s. Showing partial diagnostics." phase
         (Printexc.to_string exn))
    (zero_loc_for_uri uri)

let publish_partial_diagnostics_on_failure (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(uri : T.DocumentUri.t)
    ~(phase : string) ~(exn : exn) : unit =
  let base = try Workspace.diagnostics_for ws ~uri with _ -> [] in
  let diag = diag_internal_server_failure ~uri ~phase exn in
  let published =
    publish_diagnostics_if_changed published_diags oc ~uri
      ~diags:(base @ [ diag ])
  in
  if published then Perf_stats.tick "diag.open.publish"

let parse_uri_arg (arg : Yojson.Safe.t) : T.DocumentUri.t option =
  match arg with
  | `String s -> Uri_path.docuri_of_string s
  | `Assoc xs -> (
      match List.assoc_opt "uri" xs with
      | Some (`String s) -> Uri_path.docuri_of_string s
      | _ -> None)
  | _ -> None

let parse_int_arg (arg : Yojson.Safe.t) : int option =
  match arg with
  | `Int n -> Some n
  | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let parse_did_open_payload (params : Yojson.Safe.t) :
    (T.DocumentUri.t * string) option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "textDocument" xs with
      | Some (`Assoc tdxs) -> (
          match (find_field "uri" tdxs, find_field "text" tdxs) with
          | Some (`String uri_s), Some (`String text) -> (
              match Uri_path.docuri_of_string uri_s with
              | Some uri -> Some (uri, text)
              | None -> None)
          | _ -> None)
      | _ -> None)

let parse_text_document_uri (params : Yojson.Safe.t) : T.DocumentUri.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "textDocument" xs with
      | Some (`Assoc tdxs) -> (
          match find_field "uri" tdxs with
          | Some (`String s) -> Uri_path.docuri_of_string s
          | _ -> None)
      | _ -> None)

let parse_position (params : Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "position" xs with
      | Some (`Assoc pxs) -> (
          match (int_field "line" pxs, int_field "character" pxs) with
          | Some line, Some character -> Some { T.Position.line; character }
          | _ -> None)
      | _ -> None)

let parse_position_obj (j : Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc j with
  | None -> None
  | Some xs -> (
      match (int_field "line" xs, int_field "character" xs) with
      | Some line, Some character -> Some { T.Position.line; character }
      | _ -> None)

let parse_range (params : Yojson.Safe.t) : T.Range.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "range" xs with
      | Some (`Assoc rxs) -> (
          match (find_field "start" rxs, find_field "end" rxs) with
          | Some s, Some e -> (
              match (parse_position_obj s, parse_position_obj e) with
              | Some start, Some end_ -> Some { T.Range.start; end_ }
              | _ -> None)
          | _ -> None)
      | _ -> None)

let parse_new_name (params : Yojson.Safe.t) : string option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "newName" xs with
      | Some (`String s) -> Some s
      | _ -> None)

let parse_include_declaration (params : Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> true
  | Some xs -> (
      match find_field "context" xs with
      | Some (`Assoc cxs) -> (
          match find_field "includeDeclaration" cxs with
          | Some (`Bool b) -> b
          | _ -> true)
      | _ -> true)

let parse_workspace_symbol_query (params : Yojson.Safe.t) : string =
  match get_assoc params with
  | None -> ""
  | Some xs -> (
      match find_field "query" xs with Some (`String s) -> s | _ -> "")

let parse_cancel_request_id (params : Yojson.Safe.t) : Yojson.Safe.t option =
  match get_assoc params with None -> None | Some xs -> find_field "id" xs

let parse_root_uri (params : Yojson.Safe.t) : T.DocumentUri.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "rootUri" xs with
      | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
      | _ -> (
          match find_field "workspaceFolders" xs with
          | Some (`List (`Assoc f0 :: _)) -> (
              match find_field "uri" f0 with
              | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
              | _ -> None)
          | _ -> None))

let parse_workspace_folder_roots (params : Yojson.Safe.t) : T.DocumentUri.t list
    =
  match get_assoc params with
  | None -> []
  | Some xs -> (
      match find_field "workspaceFolders" xs with
      | Some (`List folders) ->
          folders
          |> List.filter_map (function
            | `Assoc fxs -> (
                match find_field "uri" fxs with
                | Some (`String s) when s <> "" -> Uri_path.docuri_of_string s
                | _ -> None)
            | _ -> None)
      | _ -> [])

let parse_root_uris (params : Yojson.Safe.t) : T.DocumentUri.t list =
  let roots = parse_workspace_folder_roots params in
  if roots <> [] then roots
  else match parse_root_uri params with Some u -> [ u ] | None -> []

let parse_root_path (params : Yojson.Safe.t) : string option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "rootPath" xs with
      | Some (`String s) when s <> "" -> Some s
      | _ -> None)

let parse_semantic_tokens_refresh_support (params : Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> false
  | Some xs -> (
      match find_field "capabilities" xs with
      | Some (`Assoc cxs) -> (
          match find_field "workspace" cxs with
          | Some (`Assoc wxs) -> (
              match find_field "semanticTokens" wxs with
              | Some (`Assoc stxs) -> (
                  match find_field "refreshSupport" stxs with
                  | Some (`Bool b) -> b
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)

let parse_watched_file_changes (params : Yojson.Safe.t) :
    bool * int * (string * [ `Created | `Changed | `Deleted ]) list =
  let parse_kind = function
    | `Int 1 | `Intlit "1" -> Some `Created
    | `Int 2 | `Intlit "2" -> Some `Changed
    | `Int 3 | `Intlit "3" -> Some `Deleted
    | _ -> None
  in
  match get_assoc params with
  | None -> (false, 0, [])
  | Some xs -> (
      match find_field "changes" xs with
      | Some (`List changes) ->
          let parsed =
            changes
            |> List.filter_map (function
              | `Assoc cxs -> (
                  match (find_field "uri" cxs, find_field "type" cxs) with
                  | Some (`String u), Some kind_json -> (
                      match
                        (Uri_path.docuri_of_string u, parse_kind kind_json)
                      with
                      | Some uri, Some kind -> (
                          match file_of_uri uri with
                          | Some path -> Some (path, kind)
                          | None -> None)
                      | _ -> None)
                  | _ -> None)
              | _ -> None)
          in
          (true, List.length changes, parsed)
      | _ -> (false, 0, []))

let runtime_settings : Lsp_runtime_settings.t ref =
  ref (Lsp_runtime_settings.from_env ())

let inbox_max_depth_watermark = ref 0
let watch_recent_ms : (string, float) Hashtbl.t = Hashtbl.create 4096

let watcher_change_kind_tag = function
  | `Created -> "C"
  | `Changed -> "M"
  | `Deleted -> "D"

let filter_noisy_watcher_changes
    (changes : (string * [ `Created | `Changed | `Deleted ]) list) :
    (string * [ `Created | `Changed | `Deleted ]) list * int =
  let watch_coalesce_ttl_ms = !runtime_settings.watch_coalesce_ttl_ms in
  if watch_coalesce_ttl_ms <= 0 then (changes, 0)
  else
    let now = Perf_stats.now_ms () in
    let dropped = ref 0 in
    let kept_rev =
      List.fold_left
        (fun acc ((path, kind) as change) ->
          let key =
            String.lowercase_ascii path ^ "|" ^ watcher_change_kind_tag kind
          in
          match Hashtbl.find_opt watch_recent_ms key with
          | Some last_ms
            when now -. last_ms < float_of_int watch_coalesce_ttl_ms ->
              incr dropped;
              acc
          | _ ->
              Hashtbl.replace watch_recent_ms key now;
              change :: acc)
        [] changes
    in
    (List.rev kept_rev, !dropped)

let publish_background_diag_updates (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(max_items : int) : unit =
  let updates = Workspace.drain_pending_diag_updates ws ~max_items in
  if updates <> [] then Perf_stats.tick "bg.diag_publish_batch";
  List.iter
    (fun (uri, diags) ->
      ignore (publish_diagnostics_if_changed published_diags oc ~uri ~diags))
    updates

let publish_open_diag_revalidate_updates (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(max_items : int) : unit =
  if max_items <= 0 then ()
  else
    let uris =
      Perf_stats.time "diag.open.revalidate_batch_ms" (fun () ->
          Workspace.drain_open_diag_revalidate_uris ws ~max_items)
    in
    List.iter
      (fun uri ->
        publish_doc_diagnostics ws oc published_diags ~provisional:false ~uri)
      uris

let startup_open_diag_batch_size (ws : Workspace.t) ~(base : int) : int =
  let base = max 1 base in
  if Workspace.startup_diag_hover_ready_now ws then base else 1

let send_request (oc : out_channel) ~(id : Yojson.Safe.t) ~(method_ : string)
    ~(params : Yojson.Safe.t) : unit =
  Lsp_io.write_message oc
    (json_obj
       [
         ("jsonrpc", `String "2.0");
         ("id", id);
         ("method", `String method_);
         ("params", params);
       ])

let request_semantic_tokens_refresh (oc : out_channel)
    ~(refresh_supported : bool) (next_id : int ref) : unit =
  if refresh_supported then (
    let id = !next_id in
    incr next_id;
    send_request oc ~id:(`Int id) ~method_:"workspace/semanticTokens/refresh"
      ~params:`Null)

type root_workspace = { root_path_key : string; ws : Workspace.t }

type roots_state = {
  mutable default_ws : Workspace.t;
  mutable roots : root_workspace list;
}

let normalize_path_key (p : string) : string =
  let p = String.map (fun c -> if c = '\\' then '/' else c) p in
  if Sys.win32 then String.lowercase_ascii p else p

let path_within_root ~(path_key : string) ~(root_key : string) : bool =
  let lp = String.length path_key in
  let lr = String.length root_key in
  if lr = 0 || lp < lr then false
  else if String.sub path_key 0 lr <> root_key then false
  else if lp = lr then true
  else match path_key.[lr] with '/' -> true | _ -> false

let roots_state_create () : roots_state =
  { default_ws = Workspace.create (); roots = [] }

let all_workspaces (roots_state : roots_state) : Workspace.t list =
  let seen : (Workspace.t, bool) Hashtbl.t = Hashtbl.create 16 in
  let out = ref [] in
  let add (ws : Workspace.t) =
    if not (Hashtbl.mem seen ws) then (
      Hashtbl.replace seen ws true;
      out := ws :: !out)
  in
  add roots_state.default_ws;
  List.iter (fun r -> add r.ws) roots_state.roots;
  List.rev !out

let ws_for_path (roots_state : roots_state) (path : string) : Workspace.t =
  let key = normalize_path_key path in
  let rec pick best_len best_ws = function
    | [] -> (
        match best_ws with Some ws -> ws | None -> roots_state.default_ws)
    | r :: tl ->
        if path_within_root ~path_key:key ~root_key:r.root_path_key then
          let len = String.length r.root_path_key in
          if len > best_len then pick len (Some r.ws) tl
          else pick best_len best_ws tl
        else pick best_len best_ws tl
  in
  pick (-1) None roots_state.roots

let ws_for_uri (roots_state : roots_state) (uri : T.DocumentUri.t) : Workspace.t
    =
  match file_of_uri uri with
  | Some path -> ws_for_path roots_state path
  | None -> roots_state.default_ws

let ws_refresh_counter_get
    (pending_change_refreshes : (Workspace.t, int) Hashtbl.t) (ws : Workspace.t)
    : int =
  match Hashtbl.find_opt pending_change_refreshes ws with
  | Some n -> n
  | None -> 0

let ws_refresh_counter_set
    (pending_change_refreshes : (Workspace.t, int) Hashtbl.t) (ws : Workspace.t)
    (n : int) : unit =
  Hashtbl.replace pending_change_refreshes ws n

let reset_all_refresh_counters
    (pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    (roots_state : roots_state) : unit =
  Hashtbl.clear pending_change_refreshes;
  all_workspaces roots_state
  |> List.iter (fun ws -> Hashtbl.replace pending_change_refreshes ws 0)

let build_workspace_settings (overrides : Lsp_runtime_settings.client_overrides)
    : Workspace_settings.t =
  Workspace_settings.from_env () |> fun settings ->
  Workspace_settings.apply_client_overrides settings
    {
      Workspace_settings.workspace_diag_mode = overrides.workspace_diag_mode;
      workspace_profile_mode = overrides.workspace_profile_mode;
      root_model = overrides.root_model;
      root_manual_files = overrides.root_manual_files;
      parse_file_max_bytes = overrides.parse_file_max_bytes;
      pressure_soft_mb = overrides.pressure_soft_mb;
      pressure_critical_mb = overrides.pressure_critical_mb;
    }

let handle_initialize (roots_state : roots_state)
    ~(pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    ~(semantic_refresh_supported : bool ref) (oc : out_channel)
    (id : Yojson.Safe.t) (params : Yojson.Safe.t) =
  semantic_refresh_supported := parse_semantic_tokens_refresh_support params;
  let client_overrides = Req.parse_client_overrides params in
  runtime_settings :=
    Lsp_runtime_settings.apply_client_overrides
      (Lsp_runtime_settings.from_env ())
      client_overrides;
  let workspace_settings = build_workspace_settings client_overrides in
  let do_sync_rescan =
    workspace_settings.Workspace_settings.workspace_diag_mode
    <> Workspace_settings.WorkspaceDiagsOff
  in
  roots_state.default_ws <- Workspace.create ~settings:workspace_settings ();
  let roots = parse_root_uris params in
  if roots = [] then (
    roots_state.roots <- [];
    (match parse_root_uri params with
    | Some ru -> Workspace.set_root_uri roots_state.default_ws (Some ru)
    | None ->
        Workspace.set_root_path roots_state.default_ws (parse_root_path params));
    if do_sync_rescan then Workspace.rescan roots_state.default_ws)
  else (
    roots_state.roots <-
      roots
      |> List.filter_map (fun root_uri ->
          match file_of_uri root_uri with
          | None -> None
          | Some root_path ->
              let ws = Workspace.create ~settings:workspace_settings () in
              Workspace.set_root_uri ws (Some root_uri);
              if do_sync_rescan then Workspace.rescan ws;
              Some { root_path_key = normalize_path_key root_path; ws });
    Workspace.set_root_path roots_state.default_ws None;
    ());
  reset_all_refresh_counters pending_change_refreshes roots_state;
  respond oc ~id ~result:initialize_result_json

let handle_shutdown oc id = respond oc ~id ~result:`Null

let handle_execute_command (roots_state : roots_state)
    ~(all_workspaces : unit -> Workspace.t list) (oc : out_channel)
    (id : Yojson.Safe.t) (params : Yojson.Safe.t) =
  try
    let p = T.ExecuteCommandParams.t_of_yojson params in
    match p.command with
    | "jovial.dumpAst" -> (
        let uri_opt =
          match p.arguments with
          | None -> None
          | Some (a0 :: _) -> parse_uri_arg a0
          | Some [] -> None
        in
        match uri_opt with
        | None ->
            respond_error oc ~id ~code:(-32602)
              ~message:"dumpAst: missing uri argument"
        | Some uri -> (
            let ws = ws_for_uri roots_state uri in
            match Workspace.ast_dump_for ws ~uri with
            | None -> respond oc ~id ~result:`Null
            | Some s -> respond oc ~id ~result:(`String s)))
    | "jovial.dumpCst" -> (
        let uri_opt =
          match p.arguments with
          | None -> None
          | Some (a0 :: _) -> parse_uri_arg a0
          | Some [] -> None
        in
        match uri_opt with
        | None ->
            respond_error oc ~id ~code:(-32602)
              ~message:"dumpCst: missing uri argument"
        | Some uri -> (
            let ws = ws_for_uri roots_state uri in
            match Workspace.cst_dump_for ws ~uri with
            | None -> respond oc ~id ~result:`Null
            | Some s -> respond oc ~id ~result:(`String s)))
    | "jovial.rescanWorkspace" ->
        let total =
          all_workspaces ()
          |> List.fold_left
               (fun acc ws ->
                 Workspace.rescan ws;
                 acc + Workspace.compool_count ws)
               0
        in
        respond oc ~id ~result:(`Assoc [ ("compoolCount", `Int total) ])
    | "jovial.dumpLsifIndex" -> (
        let uri_opt =
          match p.arguments with
          | Some (a0 :: _) -> parse_uri_arg a0
          | _ -> None
        in
        match uri_opt with
        | None ->
            respond_error oc ~id ~code:(-32602)
              ~message:"dumpLsifIndex: missing uri argument"
        | Some uri ->
            let ws = ws_for_uri roots_state uri in
            respond oc ~id ~result:(Workspace.lsif_index_json ws))
    | "jovial.dumpLsifDelta" -> (
        let uri_opt, base_revision_opt =
          match p.arguments with
          | Some (a0 :: a1 :: _) ->
              let uri = parse_uri_arg a0 in
              let base =
                match parse_int_arg a1 with
                | Some n when n >= 0 -> Some n
                | _ -> None
              in
              (uri, base)
          | _ -> (None, None)
        in
        match (uri_opt, base_revision_opt) with
        | Some uri, Some base_revision ->
            let ws = ws_for_uri roots_state uri in
            respond oc ~id ~result:(Workspace.lsif_delta_json ws ~base_revision)
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"dumpLsifDelta: missing uri/baseRevision arguments")
    | "jovial.debugReport" -> (
        let uri_opt, max_tokens =
          match p.arguments with
          | None -> (None, 200)
          | Some [] -> (None, 200)
          | Some (a0 :: rest) ->
              let uri = parse_uri_arg a0 in
              let mt =
                match rest with
                | a1 :: _ -> (
                    match parse_int_arg a1 with Some n -> n | None -> 200)
                | [] -> 200
              in
              (uri, mt)
        in
        match uri_opt with
        | None ->
            respond_error oc ~id ~code:(-32602)
              ~message:"debugReport: missing uri argument"
        | Some uri ->
            let ws = ws_for_uri roots_state uri in
            if
              (not (Workspace.startup_is_ready_now ws))
              && Workspace.open_doc_count ws >= 8
            then (
              try
                ignore (Workspace.finish_last_open_doc_now_if_needed ws);
                Workspace.background_tick ws ~budget_ms:20
                  ~mode:Workspace.BgTickInteractive ~idle_quiet_ms:0
                  ~last_message_ms:(Perf_stats.now_ms ())
              with exn ->
                Perf_stats.tick "loop.bg_exception";
                prerr_endline
                  (Printf.sprintf "debugReport maintenance tick failed: %s"
                     (Printexc.to_string exn)));
            let j = Workspace.debug_report_for ws ~uri ~max_tokens in
            let j =
              match j with
              | `Assoc fields ->
                  `Assoc
                    (( "server",
                       `Assoc
                         [
                           ( "inboxMaxItems",
                             `Int !runtime_settings.inbox_max_items );
                           ("inboxMaxDepthSeen", `Int !inbox_max_depth_watermark);
                         ] )
                    :: fields)
              | _ -> j
            in
            respond oc ~id ~result:j)
    | "jovial.debugPerfStats" ->
        respond oc ~id
          ~result:(Workspace.perf_stats_json roots_state.default_ws)
    | _ -> respond_error oc ~id ~code:(-32601) ~message:"Unknown command"
  with _ ->
    respond_error oc ~id ~code:(-32602) ~message:"Invalid executeCommand params"

let handle_notification (roots_state : roots_state)
    ~(semantic_refresh_supported : bool ref) ~(next_server_request_id : int ref)
    ~(pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    ~(published_diags : (string, string) Hashtbl.t)
    ~(mark_cancelled : Yojson.Safe.t -> unit) (oc : out_channel)
    (method_ : string) (params : Yojson.Safe.t) =
  match method_ with
  | "initialized" -> ()
  | "$/cancelRequest" -> (
      match parse_cancel_request_id params with
      | Some id -> mark_cancelled id
      | None -> ())
  | "textDocument/didOpen" -> (
      try
        match parse_did_open_payload params with
        | None -> raise (Failure "invalid didOpen payload")
        | Some (uri, text) -> (
            let file = file_of_uri uri in
            let ws = ws_for_uri roots_state uri in
            Perf_stats.tick "diag.open.request";
            try
              Hashtbl.remove published_diags (Uri_path.docuri_to_string uri);
              (match Workspace.preview_open_doc_diags ws ~uri ~file ~text with
              | None -> ()
              | Some diags ->
                  let published =
                    publish_diagnostics_if_changed published_diags oc ~uri
                      ~diags
                  in
                  if published then (
                    Perf_stats.tick "diag.open.preview_publish";
                    flush oc));
              Workspace.open_doc ~inline_catch_up:false ws ~uri ~file ~text;
              publish_doc_diagnostics ws oc published_diags ~provisional:true
                ~uri;
              ws_refresh_counter_set pending_change_refreshes ws 0;
              request_semantic_tokens_refresh oc
                ~refresh_supported:!semantic_refresh_supported
                next_server_request_id
            with exn ->
              Perf_stats.tick "diag.open.handler_exception";
              prerr_endline
                (Printf.sprintf "notification didOpen failed for %s: %s"
                   (Uri_path.docuri_to_string uri)
                   (Printexc.to_string exn));
              publish_partial_diagnostics_on_failure ws oc published_diags ~uri
                ~phase:"didOpen" ~exn)
      with exn ->
        Perf_stats.tick "diag.open.decode_error";
        prerr_endline
          (Printf.sprintf "notification didOpen decode failed: %s"
             (Printexc.to_string exn)))
  | "textDocument/didChange" -> (
      try
        let p = T.DidChangeTextDocumentParams.t_of_yojson params in
        let uri = p.textDocument.uri in
        let ws = ws_for_uri roots_state uri in
        Perf_stats.tick "diag.open.request";
        try
          Workspace.change_doc ws ~uri ~changes:p.contentChanges;
          publish_doc_diagnostics ws oc published_diags ~provisional:true ~uri;
          let next = ws_refresh_counter_get pending_change_refreshes ws + 1 in
          ws_refresh_counter_set pending_change_refreshes ws next;
          if next >= !runtime_settings.sem_refresh_every_didchange then (
            ws_refresh_counter_set pending_change_refreshes ws 0;
            request_semantic_tokens_refresh oc
              ~refresh_supported:!semantic_refresh_supported
              next_server_request_id)
        with exn ->
          Perf_stats.tick "diag.open.handler_exception";
          prerr_endline
            (Printf.sprintf "notification didChange failed for %s: %s"
               (Uri_path.docuri_to_string uri)
               (Printexc.to_string exn));
          publish_partial_diagnostics_on_failure ws oc published_diags ~uri
            ~phase:"didChange" ~exn
      with exn ->
        Perf_stats.tick "diag.open.decode_error";
        prerr_endline
          (Printf.sprintf "notification didChange decode failed: %s"
             (Printexc.to_string exn)))
  | "textDocument/didClose" -> (
      try
        let p = T.DidCloseTextDocumentParams.t_of_yojson params in
        let uri = p.textDocument.uri in
        let ws = ws_for_uri roots_state uri in
        Workspace.close_doc ws ~uri;
        ignore
          (publish_diagnostics_if_changed published_diags oc ~uri ~diags:[]);
        revalidate_and_publish_all_open_docs ws oc published_diags;
        ws_refresh_counter_set pending_change_refreshes ws 0;
        request_semantic_tokens_refresh oc
          ~refresh_supported:!semantic_refresh_supported
          next_server_request_id
      with _ -> ())
  | "workspace/didChangeWatchedFiles" ->
      let has_changes_field, raw_changes, changes =
        parse_watched_file_changes params
      in
      let changes, coalesced_ignored = filter_noisy_watcher_changes changes in
      if coalesced_ignored > 0 then
        Perf_stats.tick "watch.filtered_changes_ignored";
      let changed_any =
        if not has_changes_field then (
          all_workspaces roots_state |> List.iter Workspace.rescan;
          true)
        else if changes = [] then (
          if raw_changes > 0 then
            Perf_stats.tick "watch.filtered_changes_ignored";
          false)
        else
          let grouped :
              ( Workspace.t,
                (string * [ `Created | `Changed | `Deleted ]) list )
              Hashtbl.t =
            Hashtbl.create 16
          in
          let push_group ws change =
            let prev =
              match Hashtbl.find_opt grouped ws with
              | Some xs -> xs
              | None -> []
            in
            Hashtbl.replace grouped ws (change :: prev)
          in
          List.iter
            (fun ((path, _kind) as change) ->
              let ws = ws_for_path roots_state path in
              push_group ws change)
            changes;
          Hashtbl.iter
            (fun ws ws_changes ->
              Workspace.apply_watched_file_changes ws
                ~changes:(List.rev ws_changes);
              revalidate_and_publish_all_open_docs ws oc published_diags;
              ws_refresh_counter_set pending_change_refreshes ws 0)
            grouped;
          true
      in
      if changed_any then
        request_semantic_tokens_refresh oc
          ~refresh_supported:!semantic_refresh_supported
          next_server_request_id
  | "exit" -> exit 0
  | _ -> ()

let merge_workspace_symbols (results : T.SymbolInformation.t list list) :
    T.SymbolInformation.t list =
  let seen = Hashtbl.create 2048 in
  let out = ref [] in
  let add_item (symbol : T.SymbolInformation.t) =
    let k = Yojson.Safe.to_string (T.SymbolInformation.yojson_of_t symbol) in
    if not (Hashtbl.mem seen k) then (
      Hashtbl.replace seen k true;
      out := symbol :: !out)
  in
  results |> List.iter (List.iter add_item);
  List.rev !out

let respond_cancelled (oc : out_channel) ~(id : Yojson.Safe.t) : unit =
  respond_error oc ~id ~code:(-32800) ~message:"Request cancelled"

let handle_request (roots_state : roots_state)
    ~(all_workspaces : unit -> Workspace.t list)
    ~(pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    ~(semantic_refresh_supported : bool ref) ~(is_cancelled : unit -> bool)
    (oc : out_channel) (method_ : string) (id : Yojson.Safe.t)
    (params : Yojson.Safe.t) =
  let respond_result (result : Yojson.Safe.t) : unit =
    if is_cancelled () then respond_cancelled oc ~id else respond oc ~id ~result
  in
  let with_cancel_ws (ws : Workspace.t) f =
    Workspace.with_request_cancel_checker ws is_cancelled f
  in
  if is_cancelled () then respond_cancelled oc ~id
  else
    match method_ with
    | "initialize" ->
        handle_initialize roots_state ~pending_change_refreshes
          ~semantic_refresh_supported oc id params
    | "shutdown" -> handle_shutdown oc id
    | "workspace/executeCommand" ->
        handle_execute_command roots_state ~all_workspaces oc id params
    | "textDocument/documentSymbol" -> (
        match parse_text_document_uri params with
        | None ->
            respond_error oc ~id ~code:(-32602)
              ~message:"documentSymbol: missing textDocument.uri"
        | Some uri ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.document_symbols_for ws ~uri
                  |> Resp.yojson_of_document_symbols)
            in
            respond_result j)
    | "textDocument/definition" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.definition_locations_for ws ~uri ~pos
                  |> Resp.yojson_of_locations)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"definition: invalid textDocument or position")
    | "textDocument/declaration" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.declaration_locations_for ws ~uri ~pos
                  |> Resp.yojson_of_locations)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"declaration: invalid textDocument or position")
    | "textDocument/typeDefinition" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.type_definition_locations_for ws ~uri ~pos
                  |> Resp.yojson_of_locations)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"typeDefinition: invalid textDocument or position")
    | "textDocument/implementation" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.implementation_locations_for ws ~uri ~pos
                  |> Resp.yojson_of_locations)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"implementation: invalid textDocument or position")
    | "textDocument/references" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let include_decl = parse_include_declaration params in
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.references_locations_for ws ~uri ~pos ~include_decl
                  |> Resp.yojson_of_locations)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"references: invalid textDocument or position")
    | "workspace/symbol" ->
        let query = parse_workspace_symbol_query params in
        let merged =
          all_workspaces ()
          |> List.map (fun ws ->
              with_cancel_ws ws (fun () ->
                  Workspace.workspace_symbols_for ws ~query))
          |> merge_workspace_symbols |> Resp.yojson_of_symbol_infos
        in
        respond_result merged
    | "textDocument/hover" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.hover_for ws ~uri ~pos |> Resp.yojson_of_hover_opt)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"hover: invalid textDocument or position")
    | "textDocument/signatureHelp" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.signature_help_for ws ~uri ~pos
                  |> Resp.yojson_of_signature_help_opt)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"signatureHelp: invalid textDocument or position")
    | "textDocument/prepareRename" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.prepare_rename_for ws ~uri ~pos
                  |> Resp.yojson_of_prepare_rename_opt)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"prepareRename: invalid textDocument or position")
    | "textDocument/rename" -> (
        match
          ( parse_text_document_uri params,
            parse_position params,
            parse_new_name params )
        with
        | Some uri, Some pos, Some new_name ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.rename_for ws ~uri ~pos ~new_name
                  |> Resp.yojson_of_workspace_edit_opt)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"rename: invalid textDocument, position, or newName")
    | "textDocument/completion" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.completion_items_for ws ~uri ~pos
                  |> Resp.yojson_of_completion_items)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"completion: invalid textDocument or position")
    | "textDocument/codeAction" -> (
        match (parse_text_document_uri params, parse_range params) with
        | Some uri, Some range ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.code_actions_for ws ~uri ~range
                  |> Resp.yojson_of_code_actions)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"codeAction: invalid textDocument or range")
    | "textDocument/inlayHint" -> (
        match (parse_text_document_uri params, parse_range params) with
        | Some uri, Some range ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.inlay_hints_for ws ~uri ~range
                  |> Resp.yojson_of_inlay_hints)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"inlayHint: invalid textDocument or range")
    | "textDocument/semanticTokens/full" -> (
        match parse_text_document_uri params with
        | Some uri ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.semantic_tokens_full_for ws ~uri
                  |> Resp.yojson_of_semantic_tokens_opt)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"semanticTokens/full: invalid textDocument")
    | "textDocument/semanticTokens/range" -> (
        match (parse_text_document_uri params, parse_range params) with
        | Some uri, Some range ->
            let ws = ws_for_uri roots_state uri in
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.semantic_tokens_range_for ws ~uri ~range
                  |> Resp.yojson_of_semantic_tokens_opt)
            in
            respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"semanticTokens/range: invalid textDocument or range")
    | _ ->
        respond_error oc ~id ~code:(-32601)
          ~message:("Method not found: " ^ method_)

type inbox_item = InboxMsg of string | InboxInvalidFrame of string | InboxEof

type inbox = {
  q : inbox_item Queue.t;
  mtx : Mutex.t;
  not_full : Condition.t;
  max_items : int;
  mutable max_depth_seen : int;
}

let inbox_create ~(max_items : int) () : inbox =
  {
    q = Queue.create ();
    mtx = Mutex.create ();
    not_full = Condition.create ();
    max_items;
    max_depth_seen = 0;
  }

let inbox_push (box : inbox) (item : inbox_item) : unit =
  Mutex.lock box.mtx;
  let rec wait_not_full () =
    if Queue.length box.q >= box.max_items then (
      ignore
        (Perf_stats.time "inbox.block_wait_ms" (fun () ->
             Condition.wait box.not_full box.mtx));
      wait_not_full ())
  in
  wait_not_full ();
  Queue.add item box.q;
  let depth = Queue.length box.q in
  if depth > box.max_depth_seen then (
    box.max_depth_seen <- depth;
    if depth > !inbox_max_depth_watermark then
      inbox_max_depth_watermark := depth;
    Perf_stats.tick "inbox.max_depth_seen");
  Mutex.unlock box.mtx

let inbox_drain (box : inbox) : inbox_item list =
  Mutex.lock box.mtx;
  let had_items = not (Queue.is_empty box.q) in
  let rec loop acc =
    if Queue.is_empty box.q then (
      if had_items then Condition.broadcast box.not_full;
      Mutex.unlock box.mtx;
      List.rev acc)
    else loop (Queue.pop box.q :: acc)
  in
  loop []

let inbox_is_empty (box : inbox) : bool =
  Mutex.lock box.mtx;
  let empty = Queue.is_empty box.q in
  Mutex.unlock box.mtx;
  empty

let write_wake_signal (wake_w : Unix.file_descr) : unit =
  try ignore (Unix.write_substring wake_w "\001" 0 1) with
  | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
  | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
  | _ -> ()

let drain_wake_pipe ~(nonblock : bool) (wake_r : Unix.file_descr) : unit =
  let buf = Bytes.create 256 in
  if not nonblock then
    try ignore (Unix.read wake_r buf 0 (Bytes.length buf)) with _ -> ()
  else
    let rec loop () =
      try
        let n = Unix.read wake_r buf 0 (Bytes.length buf) in
        if n = Bytes.length buf then loop ()
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()
      | _ -> ()
    in
    loop ()

let run (ic : in_channel) (oc : out_channel) : unit =
  (* critical on Windows: LSP framing expects binary-accurate reads/writes *)
  if Sys.win32 then (
    set_binary_mode_in ic true;
    set_binary_mode_out oc true);

  let roots_state = roots_state_create () in
  let semantic_refresh_supported = ref false in
  let next_server_request_id = ref 1 in
  let pending_change_refreshes : (Workspace.t, int) Hashtbl.t =
    Hashtbl.create 16
  in
  reset_all_refresh_counters pending_change_refreshes roots_state;
  let published_diags : (string, string) Hashtbl.t = Hashtbl.create 128 in
  let cancelled_request_ids : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  let in_flight_request_ids : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  let box = inbox_create ~max_items:!runtime_settings.inbox_max_items () in
  let reader_done = ref false in
  let last_message_ms = ref (Perf_stats.now_ms ()) in
  let max_inbound_msg_bytes = 16 * 1024 * 1024 in
  let wake_r, wake_w = Unix.pipe () in
  let wake_nonblock =
    let ok = ref true in
    (try Unix.set_nonblock wake_r with _ -> ok := false);
    (try Unix.set_nonblock wake_w with _ -> ok := false);
    !ok
  in

  let request_id_key (id : Yojson.Safe.t) : string = Yojson.Safe.to_string id in
  let mark_cancelled (id : Yojson.Safe.t) : unit =
    Perf_stats.tick "cancel.received";
    Hashtbl.replace cancelled_request_ids (request_id_key id) true
  in

  ignore
    (Thread.create
       (fun () ->
         let rec read_loop () =
           match
             Lsp_io.read_message_with_limit ic ~max_len:max_inbound_msg_bytes
           with
           | `Eof -> inbox_push box InboxEof
           | `Message msg ->
               inbox_push box (InboxMsg msg);
               write_wake_signal wake_w;
               read_loop ()
           | `Oversize len ->
               Perf_stats.tick "io.oversize_drop";
               prerr_endline
                 (Printf.sprintf
                    "dropped oversized inbound LSP frame (%d bytes, cap=%d \
                     bytes)"
                    len max_inbound_msg_bytes);
               write_wake_signal wake_w;
               read_loop ()
           | `Invalid msg ->
               inbox_push box (InboxInvalidFrame msg);
               inbox_push box InboxEof;
               write_wake_signal wake_w;
               ()
         in
         try
           read_loop ();
           write_wake_signal wake_w
         with _ ->
           inbox_push box InboxEof;
           write_wake_signal wake_w)
       ());

  let all_workspaces_now () = all_workspaces roots_state in

  let handle_raw_message (msg : string) : unit =
    try
      let json = try Yojson.Safe.from_string msg with _ -> `Null in
      match method_of_msg json with
      | None -> ()
      | Some m -> (
          if is_request json then (
            match id_of_msg json with
            | None -> ()
            | Some id ->
                let req_key = request_id_key id in
                Hashtbl.replace in_flight_request_ids req_key true;
                let is_cancelled () =
                  Hashtbl.mem cancelled_request_ids req_key
                in
                (try
                   Perf_stats.time ("req." ^ m) (fun () ->
                       handle_request roots_state
                         ~all_workspaces:all_workspaces_now
                         ~pending_change_refreshes ~semantic_refresh_supported
                         ~is_cancelled oc m id (params_of_msg json))
                 with exn ->
                   if is_cancelled () then respond_cancelled oc ~id
                   else
                     respond_error oc ~id ~code:(-32603)
                       ~message:
                         (Printf.sprintf "%s failed: %s" m
                            (Printexc.to_string exn)));
                Hashtbl.remove in_flight_request_ids req_key;
                Hashtbl.remove cancelled_request_ids req_key)
          else
            try
              Perf_stats.time ("notif." ^ m) (fun () ->
                  handle_notification roots_state ~semantic_refresh_supported
                    ~next_server_request_id ~pending_change_refreshes
                    ~published_diags ~mark_cancelled oc m (params_of_msg json))
            with exn ->
              prerr_endline
                (Printf.sprintf "notification %s failed: %s" m
                   (Printexc.to_string exn)))
    with exn -> (
      match exn with
      | Sys_error _
      | Unix.Unix_error (Unix.EPIPE, _, _)
      | Unix.Unix_error (Unix.EBADF, _, _)
      | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
          Perf_stats.tick "loop.flush_exception";
          prerr_endline
            (Printf.sprintf
               "server outbound channel failure; terminating session: %s"
               (Printexc.to_string exn));
          reader_done := true
      | _ ->
          prerr_endline
            (Printf.sprintf "server loop recovered from internal failure: %s"
               (Printexc.to_string exn)))
  in

  let run_background_tick () : unit =
    let wss = all_workspaces_now () in
    let n = max 1 (List.length wss) in
    let settings = !runtime_settings in
    let base_budget = max 1 (settings.bg_tick_budget_ms / n) in
    List.iter
      (fun ws ->
        try
          let startup_budget =
            Workspace.startup_background_budget_ms ws
              ~base_budget_ms:base_budget
          in
          let per_ws_budget =
            Workspace.effective_bg_tick_budget_ms ws
              ~base_budget_ms:startup_budget
          in
          Workspace.background_tick ws ~budget_ms:per_ws_budget
            ~mode:Workspace.BgTickIdle
            ~idle_quiet_ms:settings.bg_large_parse_idle_quiet_ms
            ~last_message_ms:!last_message_ms
        with exn ->
          Perf_stats.tick "loop.bg_exception";
          prerr_endline
            (Printf.sprintf "background tick failed: %s"
               (Printexc.to_string exn)))
      wss
  in

  let run_startup_fair_tick_if_needed () : unit =
    let pending =
      all_workspaces_now ()
      |> List.filter (fun ws -> not (Workspace.startup_is_ready_now ws))
    in
    if pending = [] then ()
    else
      let settings = !runtime_settings in
      let diag_pending =
        List.exists
          (fun ws -> not (Workspace.startup_diag_hover_ready_now ws))
          pending
      in
      let total_budget =
        if diag_pending then
          max settings.startup_fair_tick_ms settings.diag_min_fair_tick_ms
        else settings.startup_fair_tick_ms
      in
      if total_budget <= 0 then ()
      else
        let n = max 1 (List.length pending) in
        let base_budget = max 1 (total_budget / n) in
        List.iter
          (fun ws ->
            try
              let startup_budget =
                Workspace.startup_background_budget_ms ws
                  ~base_budget_ms:base_budget
              in
              let per_ws_budget =
                Workspace.effective_bg_tick_budget_ms ws
                  ~base_budget_ms:startup_budget
              in
              Workspace.background_tick ws ~budget_ms:per_ws_budget
                ~mode:Workspace.BgTickInteractive
                ~idle_quiet_ms:settings.bg_large_parse_idle_quiet_ms
                ~last_message_ms:!last_message_ms
            with exn ->
              Perf_stats.tick "loop.bg_exception";
              prerr_endline
                (Printf.sprintf "startup fair tick failed: %s"
                   (Printexc.to_string exn)))
          pending
  in

  let publish_background_for_all () : unit =
    all_workspaces_now ()
    |> List.iter (fun ws ->
        try
          let open_diag_batch_size =
            startup_open_diag_batch_size ws
              ~base:!runtime_settings.open_diag_revalidate_batch_size
          in
          publish_open_diag_revalidate_updates ws oc published_diags
            ~max_items:open_diag_batch_size;
          publish_background_diag_updates ws oc published_diags
            ~max_items:!runtime_settings.bg_diag_batch_size
        with exn ->
          Perf_stats.tick "loop.publish_exception";
          prerr_endline
            (Printf.sprintf "background publish failed: %s"
               (Printexc.to_string exn)))
  in

  let notify_workspace_ready_events () : unit =
    all_workspaces_now ()
    |> List.iter (fun ws ->
        match Workspace.workspace_ready_event_json ws with
        | None -> ()
        | Some payload -> (
            try notify oc ~method_:"jovial/workspaceReady" ~params:payload
            with exn ->
              Perf_stats.tick "loop.publish_exception";
              prerr_endline
                (Printf.sprintf "workspaceReady notification failed: %s"
                   (Printexc.to_string exn))))
  in

  let notify_startup_phase_events () : unit =
    all_workspaces_now ()
    |> List.iter (fun ws ->
        match Workspace.startup_phase_event_json ws with
        | None -> ()
        | Some payload -> (
            try
              notify oc ~method_:"jovial/workspaceStartupPhase" ~params:payload
            with exn ->
              Perf_stats.tick "loop.publish_exception";
              prerr_endline
                (Printf.sprintf "workspaceStartupPhase notification failed: %s"
                   (Printexc.to_string exn))))
  in

  let notify_startup_miss_events () : unit =
    all_workspaces_now ()
    |> List.iter (fun ws ->
        match Workspace.startup_miss_event_json ws with
        | None -> ()
        | Some payload -> (
            try notify oc ~method_:"jovial/workspaceStartupMiss" ~params:payload
            with exn ->
              Perf_stats.tick "loop.publish_exception";
              prerr_endline
                (Printf.sprintf "workspaceStartupMiss notification failed: %s"
                   (Printexc.to_string exn))))
  in

  let rec loop () =
    if !reader_done && inbox_is_empty box then ()
    else
      let timeout_s = float_of_int !runtime_settings.idle_sleep_ms /. 1000.0 in
      let readable, _, _ =
        Perf_stats.time "idle.select_wait" (fun () ->
            Unix.select [ wake_r ] [] [] timeout_s)
      in
      let items =
        Perf_stats.time "idle.msg_drain" (fun () ->
            if readable <> [] then
              drain_wake_pipe ~nonblock:wake_nonblock wake_r;
            inbox_drain box)
      in
      if items <> [] then last_message_ms := Perf_stats.now_ms ();
      (if items = [] then (
         if readable = [] then (
           Perf_stats.tick "idle.bg_tick";
           run_background_tick ()))
       else
         let pending = ref [] in
         List.iter
           (function
             | InboxMsg msg -> (
                 let cancel_id_opt =
                   try
                     let j = Yojson.Safe.from_string msg in
                     match method_of_msg j with
                     | Some "$/cancelRequest" when not (is_request j) ->
                         parse_cancel_request_id (params_of_msg j)
                     | _ -> None
                   with _ -> None
                 in
                 match cancel_id_opt with
                 | Some cancel_id -> mark_cancelled cancel_id
                 | None -> pending := InboxMsg msg :: !pending)
             | other -> pending := other :: !pending)
           items;
         List.iter
           (function
             | InboxMsg msg -> handle_raw_message msg
             | InboxInvalidFrame msg ->
                 prerr_endline
                   (Printf.sprintf
                      "invalid LSP frame received; terminating session: %s" msg);
                 reader_done := true
             | InboxEof -> reader_done := true)
           (List.rev !pending));
      (if items <> [] then
         try flush oc
         with exn -> (
           match exn with
           | Sys_error _
           | Unix.Unix_error (Unix.EPIPE, _, _)
           | Unix.Unix_error (Unix.EBADF, _, _)
           | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
               Perf_stats.tick "loop.flush_exception";
               prerr_endline
                 (Printf.sprintf
                    "flush failed after request handling, terminating session: \
                     %s"
                    (Printexc.to_string exn));
               reader_done := true
           | _ ->
               Perf_stats.tick "loop.flush_exception";
               prerr_endline
                 (Printf.sprintf "flush failed after request handling: %s"
                    (Printexc.to_string exn));
               reader_done := true));
      if not !reader_done then (
        if items = [] then (
          run_startup_fair_tick_if_needed ();
          publish_background_for_all ();
          notify_startup_phase_events ();
          notify_startup_miss_events ();
          notify_workspace_ready_events ());
        let can_continue =
          try
            flush oc;
            true
          with exn -> (
            match exn with
            | Sys_error _
            | Unix.Unix_error (Unix.EPIPE, _, _)
            | Unix.Unix_error (Unix.EBADF, _, _)
            | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
                Perf_stats.tick "loop.flush_exception";
                prerr_endline
                  (Printf.sprintf "flush failed, terminating session: %s"
                     (Printexc.to_string exn));
                false
            | _ ->
                Perf_stats.tick "loop.flush_exception";
                prerr_endline
                  (Printf.sprintf "flush failed with unexpected error: %s"
                     (Printexc.to_string exn));
                false)
        in
        if can_continue then loop ())
  in
  loop ();
  (try Unix.close wake_r with _ -> ());
  try Unix.close wake_w with _ -> ()
