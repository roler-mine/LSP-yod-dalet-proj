(* Module overview: Top-level LSP event loop that dispatches client messages into workspace operations. *)

module T = Lsp.Types
module Req = Lsp_request
module Resp = Lsp_response
module Perf_stats = Workspace_foundation.Perf_stats

let json_obj = Resp.json_obj
let index_health_check_interval_ms = 15_000.0
let interactive_background_grace_ms = 50.0

let get_assoc = Req.get_assoc
let find_field = Req.find_field
let method_of_msg = Req.method_of_msg
let id_of_msg = Req.id_of_msg
let params_of_msg = Req.params_of_msg
let is_request = Req.is_request

let respond = Resp.respond
let respond_error = Resp.respond_error
let notify = Resp.notify
let publish_diagnostics_if_changed = Resp.publish_diagnostics_if_changed

let record_diagnostics_if_changed
    (published_diags : (string, string) Hashtbl.t) ~(version : int option)
    ~(uri : T.DocumentUri.t) ~(diags : T.Diagnostic.t list) : bool =
  let uri_s = Uri_path.docuri_to_string uri in
  let digest = Resp.diagnostics_digest ?version diags in
  match Hashtbl.find_opt published_diags uri_s with
  | Some prev when prev = digest -> false
  | _ ->
      Hashtbl.replace published_diags uri_s digest;
      true

let sync_diagnostics_if_changed
    (published_diags : (string, string) Hashtbl.t) (oc : out_channel)
    ~(push : bool) ~(version : int option) ~(uri : T.DocumentUri.t)
    ~(diags : T.Diagnostic.t list) : bool =
  if push then
    publish_diagnostics_if_changed published_diags oc ~uri ~version ~diags
  else record_diagnostics_if_changed published_diags ~uri ~version ~diags

let sync_diagnostics_if_current
    (published_diags : (string, string) Hashtbl.t) (oc : out_channel)
    ~(push : bool) ~(uri : T.DocumentUri.t) ~(computed_version : int option)
    ~(current_version : int option) ~(diags : T.Diagnostic.t list) : bool =
  match (computed_version, current_version) with
  | Some computed, Some current when computed <> current -> false
  | _ ->
      sync_diagnostics_if_changed published_diags oc ~push ~uri
        ~version:computed_version ~diags

let diagnostics_enabled (ws : Workspace.t) : bool =
  (Workspace.feature_flags ws).Workspace_settings.diagnostics

let revalidate_and_publish_all_open_docs (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(push : bool) : bool =
  if diagnostics_enabled ws then
    Workspace.revalidate_all ws
    |> List.fold_left
         (fun any_changed uri ->
        let version, diags = Workspace.diagnostics_snapshot_for ws ~uri in
        let changed =
          sync_diagnostics_if_changed published_diags oc ~push ~uri ~version
            ~diags
        in
        if changed then (
          Perf_stats.tick "diag.open.publish";
          if Workspace.open_doc_converged ws ~uri then
            Perf_stats.tick "diag.open.authoritative_publish"
          else Perf_stats.tick "diag.open.provisional_publish");
        if changed && diags = [] then Perf_stats.tick "diag.open.publish_empty";
        any_changed || changed)
         false
  else false

let publish_doc_diagnostics (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(push : bool)
    ~(provisional : bool) ~(uri : T.DocumentUri.t) : bool =
  if diagnostics_enabled ws then (
    let version, diags = Workspace.diagnostics_snapshot_for ws ~uri in
    let changed =
      sync_diagnostics_if_changed published_diags oc ~push ~uri ~version ~diags
    in
    if changed then (
      Perf_stats.tick "diag.open.publish";
      if provisional || not (Workspace.open_doc_converged ws ~uri) then
        Perf_stats.tick "diag.open.provisional_publish"
      else Perf_stats.tick "diag.open.authoritative_publish");
    if changed && diags = [] then Perf_stats.tick "diag.open.publish_empty";
    changed)
  else false

let finish_document_diagnostics_now_if_needed (ws : Workspace.t)
    ~(uri : T.DocumentUri.t) : bool =
  if Workspace.open_doc_converged ws ~uri then false
  else Workspace.finish_open_doc_now_if_needed ws ~uri

let should_finish_document_diagnostics_inline (ws : Workspace.t) ~(text_len : int)
    : bool =
  text_len <= 256_000 && Workspace.startup_diag_hover_ready_now ws

let maybe_finish_document_diagnostics_inline (ws : Workspace.t)
    ~(uri : T.DocumentUri.t) ~(text_len : int) : bool =
  if should_finish_document_diagnostics_inline ws ~text_len then
    finish_document_diagnostics_now_if_needed ws ~uri
  else (
    Perf_stats.tick "diag.open.finish_deferred";
    false)

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
    (published_diags : (string, string) Hashtbl.t) ~(push : bool)
    ~(uri : T.DocumentUri.t) ~(phase : string) ~(exn : exn) : bool =
  if diagnostics_enabled ws then (
    let version, base =
      try Workspace.diagnostics_snapshot_for ws ~uri with _ -> (None, [])
    in
    let diag = diag_internal_server_failure ~uri ~phase exn in
    let changed =
      sync_diagnostics_if_changed published_diags oc ~push ~uri ~version
        ~diags:(base @ [ diag ])
    in
    if changed then Perf_stats.tick "diag.open.publish";
    changed)
  else false

let parse_uri_arg = Req.parse_uri_arg
let parse_int_arg = Req.parse_int_arg

let parse_position_arg (arg : Yojson.Safe.t) : T.Position.t option =
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match get_assoc arg with
  | None -> None
  | Some xs -> (
      let fields =
        match find_field "position" xs with
        | Some (`Assoc pxs) -> pxs
        | _ -> xs
      in
      match (int_field "line" fields, int_field "character" fields) with
      | Some line, Some character -> Some { T.Position.line; character }
      | _ -> None)

let parse_did_open_payload (params : Yojson.Safe.t) :
    (T.DocumentUri.t * int option * string) option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "textDocument" xs with
      | Some (`Assoc tdxs) -> (
          let version =
            match find_field "version" tdxs with
            | Some (`Int v) -> Some v
            | Some (`Intlit s) -> int_of_string_opt s
            | _ -> None
          in
          match (find_field "uri" tdxs, find_field "text" tdxs) with
          | Some (`String uri_s), Some (`String text) -> (
              match Uri_path.docuri_of_string uri_s with
              | Some uri -> Some (uri, version, text)
              | None -> None)
          | _ -> None)
      | _ -> None)

let parse_text_document_uri = Req.parse_text_document_uri
let parse_position = Req.parse_position
let parse_range = Req.parse_range

let parse_formatting_options (params : Yojson.Safe.t) :
    Workspace_formatting.options =
  let default = Workspace_formatting.default_options in
  let int_field key xs =
    match find_field key xs with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  let bool_field key xs =
    match find_field key xs with Some (`Bool b) -> Some b | _ -> None
  in
  match get_assoc params with
  | None -> default
  | Some xs -> (
      match find_field "options" xs with
      | Some (`Assoc options) ->
          {
            Workspace_formatting.tab_size =
              Option.value (int_field "tabSize" options)
                ~default:default.Workspace_formatting.tab_size;
            insert_spaces =
              Option.value (bool_field "insertSpaces" options)
                ~default:default.insert_spaces;
          }
      | _ -> default)

let parse_new_name = Req.parse_new_name
let parse_include_declaration = Req.parse_include_declaration
let parse_workspace_symbol_query = Req.parse_workspace_symbol_query

let progress_token_of_json = function
  | `String s -> Some (`String s)
  | `Int n -> Some (`Int n)
  | `Intlit s -> Option.map (fun n -> `Int n) (int_of_string_opt s)
  | _ -> None

let parse_partial_result_token (params : Yojson.Safe.t) :
    T.ProgressToken.t option =
  match get_assoc params with
  | None -> None
  | Some xs -> (
      match find_field "partialResultToken" xs with
      | None -> None
      | Some token -> progress_token_of_json token)

let parse_cancel_request_id = Req.parse_cancel_request_id
let parse_root_uri = Req.parse_root_uri
let parse_root_uris = Req.parse_root_uris
let parse_root_path = Req.parse_root_path
let parse_semantic_tokens_refresh_support =
  Req.parse_semantic_tokens_refresh_support

let parse_diagnostic_pull_support (params : Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> false
  | Some xs -> (
      match find_field "capabilities" xs with
      | Some (`Assoc cxs) -> (
          match find_field "textDocument" cxs with
          | Some (`Assoc txs) -> (
              match find_field "diagnostic" txs with
              | Some (`Assoc _) -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)

let parse_diagnostic_refresh_support (params : Yojson.Safe.t) : bool =
  match get_assoc params with
  | None -> false
  | Some xs -> (
      match find_field "capabilities" xs with
      | Some (`Assoc cxs) -> (
          match find_field "workspace" cxs with
          | Some (`Assoc wxs) -> (
              match find_field "diagnostics" wxs with
              | Some (`Assoc dxs) -> (
                  match find_field "refreshSupport" dxs with
                  | Some (`Bool b) -> b
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)

let parse_watched_file_changes = Req.parse_watched_file_changes

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
    (published_diags : (string, string) Hashtbl.t) ~(push : bool)
    ~(max_items : int) : bool =
  if diagnostics_enabled ws then (
    let updates = Workspace.drain_pending_diag_updates ws ~max_items in
    if updates <> [] then Perf_stats.tick "bg.diag_publish_batch";
    updates
    |> List.fold_left
         (fun any_changed (uri, version, diags) ->
        let current_version = Workspace.document_version ws ~uri in
        let changed =
          sync_diagnostics_if_current published_diags oc ~push ~uri
            ~computed_version:version ~current_version ~diags
        in
        any_changed || changed)
         false)
  else false

let publish_open_diag_revalidate_updates (ws : Workspace.t) (oc : out_channel)
    (published_diags : (string, string) Hashtbl.t) ~(push : bool)
    ~(max_items : int) : bool =
  if max_items <= 0 || not (diagnostics_enabled ws) then false
  else
    let uris =
      Perf_stats.time "diag.open.revalidate_batch_ms" (fun () ->
          Workspace.drain_open_diag_revalidate_uris ws ~max_items)
    in
    uris
    |> List.fold_left
         (fun any_changed uri ->
        publish_doc_diagnostics ws oc published_diags ~push ~provisional:false
          ~uri
        || any_changed)
         false

let diagnostic_pull_result_id ~(uri : T.DocumentUri.t)
    ~(diags : T.Diagnostic.t list) : string =
  Digest.to_hex
    (Digest.string
       (Uri_path.docuri_to_string uri ^ ":" ^ Resp.diagnostics_digest diags))

let diagnostic_pull_report_json (ws : Workspace.t) ~(uri : T.DocumentUri.t)
    ~(previous_result_id : string option) : Yojson.Safe.t =
  let diags = Workspace.diagnostics_for ws ~uri in
  let result_id = diagnostic_pull_result_id ~uri ~diags in
  match previous_result_id with
  | Some prev when prev = result_id ->
      json_obj [ ("kind", `String "unchanged"); ("resultId", `String result_id) ]
  | _ ->
      json_obj
        [
          ("kind", `String "full");
          ("resultId", `String result_id);
          ("items", Resp.yojson_of_diagnostics diags);
        ]

let parse_diagnostic_refresh_uris (params : Yojson.Safe.t) :
    T.DocumentUri.t list =
  let add_uri seen out raw =
    match Uri_path.docuri_of_string raw with
    | None -> out
    | Some uri ->
        let key = Uri_path.docuri_to_string uri in
        if Hashtbl.mem seen key then out
        else (
          Hashtbl.replace seen key true;
          uri :: out)
  in
  let seen = Hashtbl.create 64 in
  match get_assoc params with
  | None -> []
  | Some xs ->
      let out =
        match find_field "uris" xs with
        | Some (`List uris) ->
            List.fold_left
              (fun acc -> function
                | `String raw -> add_uri seen acc raw
                | _ -> acc)
              [] uris
        | _ -> []
      in
      let out =
        match find_field "textDocument" xs with
        | Some (`Assoc tdxs) -> (
            match find_field "uri" tdxs with
            | Some (`String raw) -> add_uri seen out raw
            | _ -> out)
        | _ -> out
      in
      List.rev out

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

let send_partial_progress (oc : out_channel) ~(token : T.ProgressToken.t)
    ~(value : Yojson.Safe.t) : unit =
  notify oc ~method_:"$/progress"
    ~params:
      (json_obj
         [
           ("token", T.ProgressToken.yojson_of_t token);
           ("value", value);
         ])

let send_partial_list_chunks (oc : out_channel) ~(token : T.ProgressToken.t)
    ~(item_to_json : 'a -> Yojson.Safe.t) (items : 'a list) : unit =
  let chunk_size = 128 in
  let rec take n acc rest =
    if n <= 0 then (List.rev acc, rest)
    else
      match rest with
      | [] -> (List.rev acc, [])
      | x :: xs -> take (n - 1) (x :: acc) xs
  in
  let rec loop rest =
    match rest with
    | [] -> ()
    | _ ->
        let chunk, tail = take chunk_size [] rest in
        send_partial_progress oc ~token
          ~value:(`List (List.map item_to_json chunk));
        loop tail
  in
  loop items

let request_semantic_tokens_refresh (oc : out_channel)
    ~(refresh_supported : bool) (next_id : int ref) : unit =
  if refresh_supported then (
    let id = !next_id in
    incr next_id;
    send_request oc ~id:(`Int id) ~method_:"workspace/semanticTokens/refresh"
      ~params:`Null)

let request_diagnostic_refresh (oc : out_channel) ~(refresh_supported : bool)
    (next_id : int ref) : unit =
  if refresh_supported then (
    let id = !next_id in
    incr next_id;
    send_request oc ~id:(`Int id) ~method_:"workspace/diagnostic/refresh"
      ~params:`Null)

type root_workspace = { root_path_key : string; ws : Workspace.t }

type roots_state = {
  mutable default_ws : Workspace.t;
  mutable roots : root_workspace list;
  open_doc_workspaces : (string, Workspace.t) Hashtbl.t;
}

let normalize_path_key = Uri_path.normalize_path_key

let path_within_root ~(path_key : string) ~(root_key : string) : bool =
  let lp = String.length path_key in
  let lr = String.length root_key in
  if lr = 0 || lp < lr then false
  else if String.sub path_key 0 lr <> root_key then false
  else if lp = lr then true
  else if root_key.[lr - 1] = '/' then true
  else match path_key.[lr] with '/' -> true | _ -> false

let roots_state_create () : roots_state =
  {
    default_ws = Workspace.create ();
    roots = [];
    open_doc_workspaces = Hashtbl.create 128;
  }

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
  Hashtbl.iter (fun _ ws -> add ws) roots_state.open_doc_workspaces;
  List.rev !out

let open_doc_key (uri : T.DocumentUri.t) : string = Uri_path.docuri_to_string uri

let bind_open_document (roots_state : roots_state) (uri : T.DocumentUri.t)
    (ws : Workspace.t) : unit =
  Hashtbl.replace roots_state.open_doc_workspaces (open_doc_key uri) ws

let unbind_open_document (roots_state : roots_state) (uri : T.DocumentUri.t) :
    unit =
  Hashtbl.remove roots_state.open_doc_workspaces (open_doc_key uri)

let ws_for_open_document (roots_state : roots_state) (uri : T.DocumentUri.t) :
    Workspace.t option =
  Hashtbl.find_opt roots_state.open_doc_workspaces (open_doc_key uri)

let reusable_open_doc_workspace_for_set (roots_state : roots_state)
    ~(used_workspaces : (Workspace.t, bool) Hashtbl.t)
    (set : Req.source_file_set) : Workspace.t option =
  let rec pick = function
    | [] -> None
    | uri :: rest -> (
        match
          Hashtbl.find_opt roots_state.open_doc_workspaces (open_doc_key uri)
        with
        | Some ws when not (Hashtbl.mem used_workspaces ws) -> Some ws
        | _ -> pick rest)
  in
  pick set.Req.source_file_uris

let ws_reusable_for_source_root (roots_state : roots_state)
    ~(used_root_keys : (string, bool) Hashtbl.t)
    ~(used_workspaces : (Workspace.t, bool) Hashtbl.t) ~(root_key : string) :
    root_workspace option =
  let unused r =
    (not (Hashtbl.mem used_root_keys r.root_path_key))
    && not (Hashtbl.mem used_workspaces r.ws)
  in
  match
    List.find_opt
      (fun r -> unused r && r.root_path_key = root_key)
      roots_state.roots
  with
  | Some r -> Some r
  | None -> (
      match
        List.find_opt
          (fun r ->
            unused r
            && (path_within_root ~path_key:root_key ~root_key:r.root_path_key
               || path_within_root ~path_key:r.root_path_key ~root_key))
          roots_state.roots
      with
      | Some r -> Some r
      | None -> None)

let apply_source_file_sets (roots_state : roots_state)
    (source_sets : Req.source_file_set list) : unit =
  let used_root_keys = Hashtbl.create (max 4 (List.length source_sets)) in
  let used_workspaces = Hashtbl.create (max 4 (List.length source_sets)) in
  let had_roots = roots_state.roots <> [] in
  let new_roots =
    source_sets
    |> List.filter_map (fun set ->
           match file_of_uri set.Req.source_root_uri with
           | None -> None
           | Some root_path ->
               let root_key = normalize_path_key root_path in
               if root_key = "" then None
               else
                 let is_new_ws = ref false in
                 let ws =
                   match
                     ws_reusable_for_source_root roots_state ~used_root_keys
                       ~used_workspaces
                       ~root_key
                   with
                   | Some r ->
                       Hashtbl.replace used_root_keys r.root_path_key true;
                       Hashtbl.replace used_workspaces r.ws true;
                       r.ws
                   | None -> (
                       match
                         reusable_open_doc_workspace_for_set roots_state
                           ~used_workspaces set
                       with
                       | Some ws ->
                           Hashtbl.replace used_workspaces ws true;
                           ws
                       | None ->
                           Hashtbl.replace used_root_keys root_key true;
                           let ws = Workspace.create () in
                           is_new_ws := true;
                           Hashtbl.replace used_workspaces ws true;
                           ws)
                 in
                 Hashtbl.replace used_root_keys root_key true;
                 Workspace.set_root_uri ws (Some set.Req.source_root_uri);
                 let source_changed =
                   Workspace.set_source_files ws
                     (Req.source_paths_for_root source_sets
                        set.Req.source_root_uri)
                 in
                 if !is_new_ws || source_changed then Workspace.rescan ws;
                 if set.Req.source_search_truncated then
                   prerr_endline
                     (Printf.sprintf
                        "source-file-set notification for %s was truncated; \
                         indexing %d supplied files."
                        (Uri_path.docuri_to_string set.Req.source_root_uri)
                        (List.length set.Req.source_file_uris));
                 prerr_endline
                   (Printf.sprintf
                      "[JOVIAL] inferred source root: %s\n\
                       [JOVIAL] reason: minimal-common-container\n\
                       [JOVIAL] files: %d"
                      root_path
                      (List.length set.Req.source_file_uris));
                 Some { root_path_key = root_key; ws })
  in
  roots_state.roots <- new_roots;
  Workspace.set_root_path roots_state.default_ws None;
  let default_changed = Workspace.set_source_files roots_state.default_ws [] in
  if new_roots = [] && (had_roots || default_changed) then
    Workspace.rescan roots_state.default_ws

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

let ws_for_document_uri (roots_state : roots_state) (uri : T.DocumentUri.t) :
    Workspace.t =
  match ws_for_open_document roots_state uri with
  | Some ws -> ws
  | None -> ws_for_uri roots_state uri

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
      source_extensions = overrides.source_extensions;
      feature_profile = overrides.feature_profile;
      feature_flags = overrides.feature_flags;
      parse_file_max_bytes = overrides.parse_file_max_bytes;
      large_file_threshold_bytes = overrides.large_file_threshold_bytes;
      huge_file_threshold_bytes = overrides.huge_file_threshold_bytes;
      full_semantic_tokens_max_bytes = overrides.full_semantic_tokens_max_bytes;
      full_parse_max_bytes = overrides.full_parse_max_bytes;
      enable_huge_file_full_parse = overrides.enable_huge_file_full_parse;
      background_parse_worker_count = overrides.background_parse_worker_count;
      pressure_soft_mb = overrides.pressure_soft_mb;
      pressure_critical_mb = overrides.pressure_critical_mb;
      startup_priority_mode = overrides.startup_priority_mode;
      implementation_config = overrides.implementation_config;
    }

let handle_initialize (roots_state : roots_state)
    ~(pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    ~(semantic_refresh_supported : bool ref)
    ~(diagnostic_push_enabled : bool ref)
    ~(diagnostic_refresh_supported : bool ref) (oc : out_channel)
    (id : Yojson.Safe.t) (params : Yojson.Safe.t) =
  let init_t0 = Perf_log.now_ms () in
  Perf_log.log_event "server_initialize_start";
  semantic_refresh_supported := parse_semantic_tokens_refresh_support params;
  let diagnostic_pull_supported = parse_diagnostic_pull_support params in
  diagnostic_push_enabled := true;
  diagnostic_refresh_supported := parse_diagnostic_refresh_support params;
  let client_overrides = Req.parse_client_overrides params in
  runtime_settings :=
    Lsp_runtime_settings.apply_client_overrides
      (Lsp_runtime_settings.from_env ())
      client_overrides;
  let workspace_settings = build_workspace_settings client_overrides in
  let source_sets = Req.parse_source_file_sets params in
  let requested_roots = parse_root_uris params in
  let roots =
    match Req.source_set_roots source_sets with
    | [] -> requested_roots
    | roots -> roots
  in
  let discovery_t0 = Perf_log.now_ms () in
  Perf_log.log_event "workspace_discovery_start"
    ~queue:(List.length source_sets);
  if source_sets = [] then
    prerr_endline
      "initialize: no jovial.workspace.sourceFileSets supplied; waiting for \
       jovial/sourceFileSets notification.";
  List.iter
    (fun set ->
      if set.Req.source_search_truncated then
        prerr_endline
          (Printf.sprintf
             "initialize: source discovery for %s was truncated; indexing %d \
              supplied files."
             (Uri_path.docuri_to_string set.Req.source_root_uri)
             (List.length set.Req.source_file_uris)))
    source_sets;
  roots_state.default_ws <- Workspace.create ~settings:workspace_settings ();
  if roots = [] then (
    roots_state.roots <- [];
    (match parse_root_uri params with
    | Some ru ->
        Workspace.set_root_uri roots_state.default_ws (Some ru);
        ignore
          (Workspace.set_source_files roots_state.default_ws
             (Req.source_paths_for_root source_sets ru))
    | None ->
        Workspace.set_root_path roots_state.default_ws (parse_root_path params));
    Workspace.rescan roots_state.default_ws)
    else (
    roots_state.roots <-
      roots
      |> List.filter_map (fun root_uri ->
          match file_of_uri root_uri with
          | None -> None
          | Some root_path ->
              let ws = Workspace.create ~settings:workspace_settings () in
              Workspace.set_root_uri ws (Some root_uri);
              ignore
                (Workspace.set_source_files ws
                   (Req.source_paths_for_root source_sets root_uri));
              Workspace.rescan ws;
              Some { root_path_key = normalize_path_key root_path; ws });
    Workspace.set_root_path roots_state.default_ws None;
    ());
  Perf_log.log_event "workspace_discovery_end"
    ~queue:(List.length roots)
    ~ms:(max 0.0 (Perf_log.now_ms () -. discovery_t0));
  reset_all_refresh_counters pending_change_refreshes roots_state;
  respond oc ~id
    ~result:
      (initialize_result_json ~feature_flags:workspace_settings.feature_flags
         ~diagnostic_pull:diagnostic_pull_supported);
  Perf_log.log_event "server_initialize_end"
    ~ms:(max 0.0 (Perf_log.now_ms () -. init_t0))

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
    | "jovial.explainSymbolResolution" -> (
        let uri_opt, pos_opt =
          match p.arguments with
          | Some (a0 :: a1 :: a2 :: _) ->
              let pos =
                match (parse_int_arg a1, parse_int_arg a2) with
                | Some line, Some character ->
                    Some { T.Position.line; character }
                | _ -> parse_position_arg a1
              in
              (parse_uri_arg a0, pos)
          | Some (a0 :: a1 :: _) ->
              (parse_uri_arg a0, parse_position_arg a1)
          | Some (a0 :: _) ->
              (parse_uri_arg a0, parse_position_arg a0)
          | _ -> (None, None)
        in
        match (uri_opt, pos_opt) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            respond oc ~id
              ~result:(Workspace.explain_symbol_resolution_json ws ~uri ~pos)
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:
                "explainSymbolResolution: missing uri, line, and character \
                 arguments")
    | "jovial.debugPerfStats" ->
        respond oc ~id
          ~result:(Workspace.perf_stats_json roots_state.default_ws)
    | "jovial.debugScheduler" ->
        let ws =
          match p.arguments with
          | Some (a0 :: _) -> (
              match parse_uri_arg a0 with
              | Some uri -> ws_for_uri roots_state uri
              | None -> roots_state.default_ws)
          | _ -> roots_state.default_ws
        in
        respond oc ~id ~result:(Workspace.debug_scheduler_json ws)
    | "jovial.debugMemory" ->
        let ws =
          match p.arguments with
          | Some (a0 :: _) -> (
              match parse_uri_arg a0 with
              | Some uri -> ws_for_uri roots_state uri
              | None -> roots_state.default_ws)
          | _ -> roots_state.default_ws
        in
        respond oc ~id ~result:(Workspace.debug_memory_json ws)
    | _ -> respond_error oc ~id ~code:(-32601) ~message:"Unknown command"
  with _ ->
    respond_error oc ~id ~code:(-32602) ~message:"Invalid executeCommand params"

let handle_notification (roots_state : roots_state)
    ~(semantic_refresh_supported : bool ref) ~(next_server_request_id : int ref)
    ~(diagnostic_push_enabled : bool ref)
    ~(diagnostic_refresh_supported : bool ref)
    ~(pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    ~(published_diags : (string, string) Hashtbl.t)
    ~(mark_cancelled : Yojson.Safe.t -> unit) (oc : out_channel)
    (method_ : string) (params : Yojson.Safe.t) =
  let request_diagnostic_refresh_if_needed changed =
    if changed && not !diagnostic_push_enabled then
      request_diagnostic_refresh oc
        ~refresh_supported:!diagnostic_refresh_supported next_server_request_id
  in
  match method_ with
  | "initialized" -> ()
  | "jovial/sourceFileSets" ->
      let source_sets = Req.parse_source_file_sets_notification params in
      let discovery_t0 = Perf_log.now_ms () in
      Perf_log.log_event "workspace_discovery_start"
        ~queue:(List.length source_sets);
      if source_sets = [] then
        prerr_endline
          "jovial/sourceFileSets supplied no roots; clearing inferred source \
           roots.";
      apply_source_file_sets roots_state source_sets;
      Perf_log.log_event "workspace_discovery_end"
        ~queue:(List.length source_sets)
        ~ms:(max 0.0 (Perf_log.now_ms () -. discovery_t0));
      reset_all_refresh_counters pending_change_refreshes roots_state;
      request_semantic_tokens_refresh oc
        ~refresh_supported:!semantic_refresh_supported
        next_server_request_id
  | "$/cancelRequest" -> (
      match parse_cancel_request_id params with
      | Some id -> mark_cancelled id
      | None -> ())
  | "jovial/refreshDiagnostics" ->
      (* VS Code already receives diagnostics through publishDiagnostics. This
         extension-owned notification is the cheap catch-up path: it forces
         revalidation and publishes only changed diagnostics, avoiding the
         duplicate response payload from textDocument/diagnostic. *)
      parse_diagnostic_refresh_uris params
      |> List.iter (fun uri ->
             let ws = ws_for_document_uri roots_state uri in
             try
               let refreshed =
                 if finish_document_diagnostics_now_if_needed ws ~uri then true
                 else Workspace.refresh_closed_doc_diagnostics_now ws ~uri
               in
               let open_doc = Workspace.document_text_length ws ~uri <> None in
               let changed =
                 publish_doc_diagnostics ws oc published_diags
                   ~push:!diagnostic_push_enabled
                   ~provisional:
                     (open_doc && not (Workspace.open_doc_converged ws ~uri))
                   ~uri
               in
               if refreshed && not changed then
                 Perf_stats.tick "diag.refresh.no_publish_change";
               request_diagnostic_refresh_if_needed changed
             with exn ->
               Perf_stats.tick "diag.refresh.handler_exception";
               prerr_endline
                 (Printf.sprintf "diagnostic refresh failed for %s: %s"
                    (Uri_path.docuri_to_string uri)
                    (Printexc.to_string exn));
               publish_partial_diagnostics_on_failure ws oc published_diags
                 ~push:!diagnostic_push_enabled ~uri
                 ~phase:"refreshDiagnostics" ~exn
               |> request_diagnostic_refresh_if_needed)
  | "textDocument/didOpen" -> (
      try
        match parse_did_open_payload params with
        | None -> raise (Failure "invalid didOpen payload")
        | Some (uri, lsp_version, text) -> (
            let file = file_of_uri uri in
            let ws = ws_for_document_uri roots_state uri in
            let open_t0 = Perf_log.now_ms () in
            Perf_log.log_event "did_open_received"
              ~uri:(Uri_path.docuri_to_string uri)
              ~bytes:(String.length text);
            Perf_stats.tick "diag.open.request";
            try
              Hashtbl.remove published_diags (Uri_path.docuri_to_string uri);
              (match Workspace.preview_open_doc_diags ws ~uri ~file ~text with
              | None -> ()
              | Some diags ->
                  let changed =
                    sync_diagnostics_if_changed published_diags oc
                      ~push:!diagnostic_push_enabled ~uri ~version:lsp_version
                      ~diags
                  in
                  if changed then (
                    Perf_stats.tick "diag.open.preview_publish";
                    if !diagnostic_push_enabled then flush oc
                    else request_diagnostic_refresh_if_needed true));
              Workspace.open_doc ?lsp_version ~inline_catch_up:false
                ~defer_cross_module_semantics:true ws ~uri ~file ~text;
              (if Workspace.open_doc_converged ws ~uri then
                 Perf_stats.tick "didopen.readiness_kick_skipped_ready"
               else
                 try
                   Workspace.background_tick ws ~budget_ms:16
                     ~mode:Workspace.BgTickInteractive ~idle_quiet_ms:0
                     ~last_message_ms:(Perf_stats.now_ms ())
                 with exn ->
                   Perf_stats.tick "loop.bg_exception";
                   prerr_endline
                     (Printf.sprintf "didOpen readiness kick failed: %s"
                        (Printexc.to_string exn)));
              bind_open_document roots_state uri ws;
              ignore
                (maybe_finish_document_diagnostics_inline ws ~uri
                   ~text_len:(String.length text));
              publish_doc_diagnostics ws oc published_diags
                ~push:!diagnostic_push_enabled
                ~provisional:(not (Workspace.open_doc_converged ws ~uri))
                ~uri
              |> request_diagnostic_refresh_if_needed;
              ws_refresh_counter_set pending_change_refreshes ws 0;
              request_semantic_tokens_refresh oc
                ~refresh_supported:!semantic_refresh_supported
                next_server_request_id;
              Perf_log.log_event "did_open_processed"
                ~uri:(Uri_path.docuri_to_string uri)
                ~bytes:(String.length text)
                ~ms:(max 0.0 (Perf_log.now_ms () -. open_t0))
            with exn ->
              Perf_stats.tick "diag.open.handler_exception";
              prerr_endline
                (Printf.sprintf "notification didOpen failed for %s: %s"
                   (Uri_path.docuri_to_string uri)
                   (Printexc.to_string exn));
              publish_partial_diagnostics_on_failure ws oc published_diags
                ~push:!diagnostic_push_enabled ~uri ~phase:"didOpen" ~exn
              |> request_diagnostic_refresh_if_needed)
      with exn ->
        Perf_stats.tick "diag.open.decode_error";
        prerr_endline
          (Printf.sprintf "notification didOpen decode failed: %s"
             (Printexc.to_string exn)))
  | "textDocument/didChange" -> (
      try
        let p = T.DidChangeTextDocumentParams.t_of_yojson params in
        let uri = p.textDocument.uri in
        let ws = ws_for_document_uri roots_state uri in
        Perf_stats.tick "diag.open.request";
        try
          Workspace.change_doc ~lsp_version:p.textDocument.version ws ~uri
            ~changes:p.contentChanges;
          let text_len =
            Option.value (Workspace.document_text_length ws ~uri)
              ~default:(256_001)
          in
          ignore
            (maybe_finish_document_diagnostics_inline ws ~uri ~text_len);
          publish_doc_diagnostics ws oc published_diags
            ~push:!diagnostic_push_enabled
            ~provisional:(not (Workspace.open_doc_converged ws ~uri))
            ~uri
          |> request_diagnostic_refresh_if_needed;
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
          publish_partial_diagnostics_on_failure ws oc published_diags
            ~push:!diagnostic_push_enabled ~uri ~phase:"didChange" ~exn
          |> request_diagnostic_refresh_if_needed
      with exn ->
        Perf_stats.tick "diag.open.decode_error";
        prerr_endline
          (Printf.sprintf "notification didChange decode failed: %s"
             (Printexc.to_string exn)))
  | "textDocument/didClose" -> (
      try
        let p = T.DidCloseTextDocumentParams.t_of_yojson params in
        let uri = p.textDocument.uri in
        let ws = ws_for_document_uri roots_state uri in
        let _, close_diags = Workspace.diagnostics_snapshot_for ws ~uri in
        Workspace.close_doc ws ~uri;
        unbind_open_document roots_state uri;
        let close_changed =
          sync_diagnostics_if_changed published_diags oc
            ~push:!diagnostic_push_enabled ~uri ~version:None ~diags:close_diags
        in
        let revalidate_changed =
          revalidate_and_publish_all_open_docs ws oc published_diags
            ~push:!diagnostic_push_enabled
        in
        request_diagnostic_refresh_if_needed
          (close_changed || revalidate_changed);
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
              revalidate_and_publish_all_open_docs ws oc published_diags
                ~push:!diagnostic_push_enabled
              |> request_diagnostic_refresh_if_needed;
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

let uri_string (uri : T.DocumentUri.t) : string =
  Uri_path.docuri_to_string uri

let snapshot_document_symbols_for (ws : Workspace.t) ~(uri : T.DocumentUri.t)
    :
    [ `DocumentSymbol of T.DocumentSymbol.t
    | `SymbolInformation of T.SymbolInformation.t ]
    list =
  match Workspace.cached_snapshot ws with
  | None -> []
  | Some snap ->
      Ide_query.document_symbols snap ~uri:(uri_string uri)
      |> List.map (fun symbol -> `DocumentSymbol symbol)

let snapshot_workspace_symbols_for (ws : Workspace.t) ~(query : string) :
    T.SymbolInformation.t list =
  match Workspace.cached_snapshot ws with
  | None -> []
  | Some snap -> Ide_query.workspace_symbols snap ~query

let snapshot_hover_for (ws : Workspace.t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Hover.t option =
  Perf_stats.tick "query.snapshot.hover_fallback";
  match Workspace.cached_snapshot ws with
  | None -> None
  | Some snap -> Ide_query.hover snap ~uri:(uri_string uri) ~pos

let snapshot_completion_items_for (ws : Workspace.t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.CompletionItem.t list =
  match Workspace.cached_snapshot ws with
  | None -> []
  | Some snap -> Ide_query.completion snap ~uri:(uri_string uri) ~pos

let snapshot_definition_locations_for (ws : Workspace.t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  Perf_stats.tick "query.snapshot.definition_fallback";
  match Workspace.cached_snapshot ws with
  | None -> []
  | Some snap -> Ide_query.definition snap ~uri:(uri_string uri) ~pos

let snapshot_reference_locations_for (ws : Workspace.t) ~(uri : T.DocumentUri.t)
    ~(pos : T.Position.t) : T.Location.t list =
  Perf_stats.tick "query.snapshot.references_fallback";
  match Workspace.cached_snapshot ws with
  | None -> []
  | Some snap -> Ide_query.references snap ~uri:(uri_string uri) ~pos |> List.of_seq

let respond_cancelled (oc : out_channel) ~(id : Yojson.Safe.t) : unit =
  respond_error oc ~id ~code:(-32800) ~message:"Request cancelled"

type request_priority =
  | Critical
  | Interactive
  | Visible
  | Medium
  | Background

let request_priority_rank = function
  | Critical -> 0
  | Interactive -> 1
  | Visible -> 2
  | Medium -> 3
  | Background -> 4

let request_priority_of_method = function
  | "initialize" | "shutdown" -> Critical
  | "textDocument/completion" | "textDocument/hover"
  | "textDocument/signatureHelp" | "textDocument/definition"
  | "textDocument/declaration" | "textDocument/typeDefinition"
  | "textDocument/implementation" ->
      Interactive
  | "textDocument/semanticTokens/range" | "textDocument/documentSymbol"
  | "textDocument/codeLens" | "textDocument/inlayHint"
  | "textDocument/formatting" | "textDocument/rangeFormatting"
  | "textDocument/diagnostic" ->
      Visible
  | "textDocument/references" | "textDocument/prepareRename"
  | "textDocument/rename" | "textDocument/codeAction" | "codeLens/resolve" ->
      Medium
  | "workspace/symbol" | "textDocument/semanticTokens/full"
  | "textDocument/semanticTokens/full/delta" ->
      Background
  | _ -> Medium

let incoming_preempts_active ~(active : request_priority)
    ~(incoming : request_priority) : bool =
  request_priority_rank incoming < request_priority_rank active

let handle_request (roots_state : roots_state)
    ~(all_workspaces : unit -> Workspace.t list)
    ~(pending_change_refreshes : (Workspace.t, int) Hashtbl.t)
    ~(semantic_refresh_supported : bool ref)
    ~(diagnostic_push_enabled : bool ref)
    ~(diagnostic_refresh_supported : bool ref) ~(is_cancelled : unit -> bool)
    (oc : out_channel) (method_ : string) (id : Yojson.Safe.t)
    (params : Yojson.Safe.t) =
  let respond_result (result : Yojson.Safe.t) : unit =
    if is_cancelled () then respond_cancelled oc ~id else respond oc ~id ~result
  in
  let with_cancel_ws (ws : Workspace.t) f =
    Workspace.drain_open_parse_results_now ws;
    Workspace.with_request_cancel_checker ws is_cancelled f
  in
  if is_cancelled () then respond_cancelled oc ~id
  else
    match method_ with
    | "initialize" ->
        handle_initialize roots_state ~pending_change_refreshes
          ~semantic_refresh_supported ~diagnostic_push_enabled
          ~diagnostic_refresh_supported oc id params
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
            let flags = Workspace.feature_flags ws in
            if not flags.document_symbols then respond_result (`List [])
            else
              let j =
                with_cancel_ws ws (fun () ->
                    let primary = Workspace.document_symbols_for ws ~uri in
                    let symbols =
                      if primary <> [] then primary
                      else snapshot_document_symbols_for ws ~uri
                    in
                    Resp.yojson_of_document_symbols symbols)
              in
              respond_result j)
    | "textDocument/definition" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.definition then respond_result (`List [])
            else
              let j =
                with_cancel_ws ws (fun () ->
                    let primary =
                      Workspace.definition_locations_for ws ~uri ~pos
                    in
                    let locs =
                      if primary <> [] then primary
                      else snapshot_definition_locations_for ws ~uri ~pos
                    in
                    Resp.yojson_of_locations locs)
              in
              respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"definition: invalid textDocument or position")
    | "textDocument/declaration" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.declaration then respond_result (`List [])
            else
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
            let flags = Workspace.feature_flags ws in
            if not flags.type_definition then respond_result (`List [])
            else
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
            let flags = Workspace.feature_flags ws in
            if not flags.implementation then respond_result (`List [])
            else
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
            let partial_token = parse_partial_result_token params in
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.references then respond_result (`List [])
            else (
              match partial_token with
              | Some token ->
                  let emit locs =
                    if locs <> [] && not (is_cancelled ()) then
                      send_partial_list_chunks oc ~token
                        ~item_to_json:T.Location.yojson_of_t locs
                  in
                  ignore
                    (with_cancel_ws ws (fun () ->
                         Workspace.references_locations_stream ws ~uri ~pos
                           ~include_decl ~emit));
                  respond_result (`List [])
              | None ->
                  let j =
                    with_cancel_ws ws (fun () ->
                        let primary =
                          Workspace.references_locations_for ws ~uri ~pos
                            ~include_decl
                        in
                        let locs =
                          if primary <> [] then primary
                          else snapshot_reference_locations_for ws ~uri ~pos
                        in
                        Resp.yojson_of_locations locs)
                  in
                  respond_result j)
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"references: invalid textDocument or position")
    | "workspace/symbol" ->
        let query = parse_workspace_symbol_query params in
        let enabled_workspaces =
          all_workspaces ()
          |> List.filter (fun ws ->
              (Workspace.feature_flags ws).Workspace_settings.workspace_symbols)
        in
        if enabled_workspaces = [] then respond_result (`List [])
        else (
          match parse_partial_result_token params with
          | Some token ->
              let seen = Hashtbl.create 2048 in
              let emit symbols =
                let fresh =
                  symbols
                  |> List.filter (fun symbol ->
                         let key =
                           Yojson.Safe.to_string
                             (T.SymbolInformation.yojson_of_t symbol)
                         in
                         if Hashtbl.mem seen key then false
                         else (
                           Hashtbl.replace seen key true;
                           true))
                in
                if fresh <> [] && not (is_cancelled ()) then
                  send_partial_list_chunks oc ~token
                    ~item_to_json:T.SymbolInformation.yojson_of_t fresh
              in
              enabled_workspaces
              |> List.iter (fun ws ->
                     ignore
                       (with_cancel_ws ws (fun () ->
                            Workspace.workspace_symbols_stream ws ~query ~emit)));
              respond_result (`List [])
          | None ->
              let merged =
                enabled_workspaces
                |> List.map (fun ws ->
                    with_cancel_ws ws (fun () ->
                        let primary =
                          Workspace.workspace_symbols_for ws ~query
                        in
                        let snapshot_hits =
                          snapshot_workspace_symbols_for ws ~query
                        in
                        if snapshot_hits = [] then primary
                        else primary @ snapshot_hits))
                |> merge_workspace_symbols |> Resp.yojson_of_symbol_infos
              in
              respond_result merged)
    | "textDocument/hover" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.hover then respond_result `Null
            else
              let j =
                Perf_stats.time "hover.request_compute_json_ms" (fun () ->
                    with_cancel_ws ws (fun () ->
                    let hover =
                      match Workspace.hover_for ws ~uri ~pos with
                      | Some _ as hit -> hit
                      | None -> snapshot_hover_for ws ~uri ~pos
                    in
                    Resp.yojson_of_hover_opt hover))
              in
              Perf_stats.time "hover.response_write_ms" (fun () ->
                  respond_result j)
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"hover: invalid textDocument or position")
    | "textDocument/signatureHelp" -> (
        match (parse_text_document_uri params, parse_position params) with
        | Some uri, Some pos ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.signature_help then respond_result `Null
            else
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
            let flags = Workspace.feature_flags ws in
            if not flags.rename then respond_result `Null
            else
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
            let flags = Workspace.feature_flags ws in
            if not flags.rename then respond_result `Null
            else
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
            let flags = Workspace.feature_flags ws in
            if not flags.completion then respond_result (`List [])
            else
              let j =
                with_cancel_ws ws (fun () ->
                    let primary =
                      Workspace.completion_items_for ws ~uri ~pos
                    in
                    let items =
                      if primary <> [] then primary
                      else snapshot_completion_items_for ws ~uri ~pos
                    in
                    Resp.yojson_of_completion_items items)
              in
              respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"completion: invalid textDocument or position")
    | "textDocument/codeAction" -> (
        match (parse_text_document_uri params, parse_range params) with
        | Some uri, Some range ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.code_actions then respond_result (`List [])
            else
              let j =
                with_cancel_ws ws (fun () ->
                    Workspace.code_actions_for ws ~uri ~range
                    |> Resp.yojson_of_code_actions)
              in
              respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"codeAction: invalid textDocument or range")
    | "textDocument/codeLens" -> (
        try
          let p = T.CodeLensParams.t_of_yojson params in
          let uri = p.textDocument.uri in
          let ws = ws_for_uri roots_state uri in
          let flags = Workspace.feature_flags ws in
          if not flags.code_lens then respond_result (`List [])
          else
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.code_lenses_for ws ~uri
                  |> Resp.yojson_of_code_lenses)
            in
            respond_result j
        with _ ->
          respond_error oc ~id ~code:(-32602)
            ~message:"codeLens: invalid textDocument")
    | "codeLens/resolve" -> (
        try
          let lens = T.CodeLens.t_of_yojson params in
          let uri_opt =
            match lens.data with
            | Some (`Assoc fields) -> (
                match List.assoc_opt "uri" fields with
                | Some (`String s) -> Uri_path.docuri_of_string s
                | _ -> None)
            | _ -> None
          in
          match uri_opt with
          | None -> respond_result (T.CodeLens.yojson_of_t lens)
          | Some uri ->
              let ws = ws_for_uri roots_state uri in
              let flags = Workspace.feature_flags ws in
              if not flags.code_lens then respond_result (T.CodeLens.yojson_of_t lens)
              else
                let j =
                  with_cancel_ws ws (fun () ->
                      Workspace.resolve_code_lens ws lens
                      |> T.CodeLens.yojson_of_t)
                in
                respond_result j
        with _ ->
          respond_error oc ~id ~code:(-32602)
            ~message:"codeLens/resolve: invalid CodeLens")
    | "textDocument/inlayHint" -> (
        try
          let p = T.InlayHintParams.t_of_yojson params in
          let uri = p.textDocument.uri in
          let ws = ws_for_uri roots_state uri in
          let flags = Workspace.feature_flags ws in
          if not flags.inlay_hints then respond_result (`List [])
          else
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.inlay_hints_for ws ~uri ~range:p.range
                  |> Resp.yojson_of_inlay_hints)
            in
            respond_result j
        with _ ->
          respond_error oc ~id ~code:(-32602)
            ~message:"inlayHint: invalid params")
    | "textDocument/formatting" -> (
        match parse_text_document_uri params with
        | Some uri ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.formatting then respond_result (`List [])
            else
              let options = parse_formatting_options params in
              let j =
                with_cancel_ws ws (fun () ->
                    Workspace.formatting_edits_for ws ~uri ~options
                    |> Resp.yojson_of_text_edits)
              in
              respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"formatting: invalid textDocument")
    | "textDocument/rangeFormatting" -> (
        match (parse_text_document_uri params, parse_range params) with
        | Some uri, Some range ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.formatting then respond_result (`List [])
            else
              let options = parse_formatting_options params in
              let j =
                with_cancel_ws ws (fun () ->
                    Workspace.range_formatting_edits_for ws ~uri ~range
                      ~options
                    |> Resp.yojson_of_text_edits)
              in
              respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"rangeFormatting: invalid textDocument or range")
    | "textDocument/diagnostic" -> (
        try
          let p = T.DocumentDiagnosticParams.t_of_yojson params in
          let uri = p.textDocument.uri in
          let ws = ws_for_document_uri roots_state uri in
          let flags = Workspace.feature_flags ws in
          if not flags.diagnostics then
            respond_result
              (json_obj
                 [
                   ("kind", `String "full");
                   ("resultId", `String "disabled");
                   ("items", `List []);
                 ])
          else
            let j =
              with_cancel_ws ws (fun () ->
                  if finish_document_diagnostics_now_if_needed ws ~uri then ()
                  else ignore (Workspace.refresh_closed_doc_diagnostics_now ws ~uri);
                  if !diagnostic_push_enabled then (
                    let version, diags =
                      Workspace.diagnostics_snapshot_for ws ~uri
                    in
                    Resp.publish_diagnostics oc ~uri ~version ~diags;
                    Perf_stats.tick "diag.pull.side_publish");
                  diagnostic_pull_report_json ws ~uri
                    ~previous_result_id:p.previousResultId)
            in
            respond_result j
        with _ ->
          respond_error oc ~id ~code:(-32602)
            ~message:"diagnostic: invalid params")
    | "textDocument/semanticTokens/full" -> (
        match parse_text_document_uri params with
        | Some uri ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.semantic_tokens then respond_result `Null
            else
              let j =
                with_cancel_ws ws (fun () ->
                    Workspace.semantic_tokens_full_for ws ~uri
                    |> Resp.yojson_of_semantic_tokens_opt)
              in
              respond_result j
        | _ ->
            respond_error oc ~id ~code:(-32602)
              ~message:"semanticTokens/full: invalid textDocument")
    | "textDocument/semanticTokens/full/delta" -> (
        try
          let p = T.SemanticTokensDeltaParams.t_of_yojson params in
          let uri = p.textDocument.uri in
          let ws = ws_for_uri roots_state uri in
          let flags = Workspace.feature_flags ws in
          if not flags.semantic_tokens then respond_result `Null
          else
            let j =
              with_cancel_ws ws (fun () ->
                  Workspace.semantic_tokens_delta_for ws ~uri
                    ~previous_result_id:p.previousResultId
                  |> Resp.yojson_of_semantic_tokens_delta_opt)
            in
            respond_result j
        with _ ->
          respond_error oc ~id ~code:(-32602)
            ~message:"semanticTokens/full/delta: invalid params")
    | "textDocument/semanticTokens/range" -> (
        match (parse_text_document_uri params, parse_range params) with
        | Some uri, Some range ->
            let ws = ws_for_uri roots_state uri in
            let flags = Workspace.feature_flags ws in
            if not flags.semantic_tokens then respond_result `Null
            else
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

let request_rank_of_raw_message (msg : string) : int option =
  try
    let json = Yojson.Safe.from_string msg in
    if not (is_request json) then None
    else
      match method_of_msg json with
      | None -> None
      | Some method_ ->
          Some (request_priority_rank (request_priority_of_method method_))
  with _ -> None

type scheduled_request = {
  sr_msg : string;
  sr_rank : int;
  sr_ordinal : int;
}

type request_scheduler = {
  mutable rs_next_ordinal : int;
  mutable rs_pending : scheduled_request list;
}

type active_request = {
  ar_key : string;
  ar_method : string;
  ar_priority : request_priority;
  mutable ar_preempted : bool;
}

let request_scheduler_create () =
  { rs_next_ordinal = 0; rs_pending = [] }

let request_scheduler_is_empty scheduler = scheduler.rs_pending = []

let request_scheduler_enqueue scheduler ~(rank : int) ~(msg : string) : unit =
  let item =
    {
      sr_msg = msg;
      sr_rank = rank;
      sr_ordinal = scheduler.rs_next_ordinal;
    }
  in
  scheduler.rs_next_ordinal <- scheduler.rs_next_ordinal + 1;
  scheduler.rs_pending <- item :: scheduler.rs_pending;
  Perf_stats.tick "scheduler.enqueue"

let request_scheduler_pop scheduler : string option =
  match
    List.sort
      (fun a b ->
        let c = compare a.sr_rank b.sr_rank in
        if c <> 0 then c else compare a.sr_ordinal b.sr_ordinal)
      scheduler.rs_pending
  with
  | [] -> None
  | item :: _ ->
      scheduler.rs_pending <-
        List.filter
          (fun pending -> pending.sr_ordinal <> item.sr_ordinal)
          scheduler.rs_pending;
      Some item.sr_msg

let request_scheduler_order_for_test (messages : string list) : string list =
  let scheduler = request_scheduler_create () in
  messages
  |> List.iter (fun msg ->
         match request_rank_of_raw_message msg with
         | None -> ()
         | Some rank -> request_scheduler_enqueue scheduler ~rank ~msg);
  let rec drain acc =
    match request_scheduler_pop scheduler with
    | None -> List.rev acc
    | Some msg -> drain (msg :: acc)
  in
  drain []

let reorder_request_runs_for_dispatch (items : inbox_item list) :
    inbox_item list =
  let flush_run run acc =
    let sorted =
      List.sort
        (fun (rank_a, ordinal_a, _) (rank_b, ordinal_b, _) ->
          let c = compare rank_a rank_b in
          if c <> 0 then c else compare ordinal_a ordinal_b)
        (List.rev run)
    in
    acc @ List.map (fun (_, _, item) -> item) sorted
  in
  let rec loop ordinal run acc = function
    | [] -> flush_run run acc
    | item :: rest -> (
        match item with
        | InboxMsg msg -> (
            match request_rank_of_raw_message msg with
            | Some rank -> loop (ordinal + 1) ((rank, ordinal, item) :: run) acc rest
            | None ->
                let acc = flush_run run acc in
                loop (ordinal + 1) [] (acc @ [ item ]) rest)
        | InboxInvalidFrame _ | InboxEof ->
            let acc = flush_run run acc in
            loop (ordinal + 1) [] (acc @ [ item ]) rest)
  in
  loop 0 [] [] items

module Private_for_tests = struct
  let reorder_raw_messages_for_dispatch (messages : string list) :
      string list =
    messages
    |> List.map (fun msg -> InboxMsg msg)
    |> reorder_request_runs_for_dispatch
    |> List.filter_map (function InboxMsg msg -> Some msg | _ -> None)

  let priority_queue_order (messages : string list) : string list =
    request_scheduler_order_for_test messages

  let incoming_preempts_active_method ~(active : string) ~(incoming : string) :
      bool =
    incoming_preempts_active
      ~active:(request_priority_of_method active)
      ~incoming:(request_priority_of_method incoming)

  let open_document_workspace_survives_source_set_replacement
      ~(workspace_root : string) ~(source_root : string) ~(source_file : string)
      : bool =
    let uri_of_path label path =
      match Uri_path.docuri_of_path path with
      | Some uri -> uri
      | None -> failwith ("invalid " ^ label ^ " path: " ^ path)
    in
    let roots_state = roots_state_create () in
    let initial_ws = Workspace.create () in
    let file_uri = uri_of_path "source file" source_file in
    roots_state.roots <-
      [
        {
          root_path_key = normalize_path_key workspace_root;
          ws = initial_ws;
        };
      ];
    bind_open_document roots_state file_uri initial_ws;
    let before = ws_for_document_uri roots_state file_uri == initial_ws in
    apply_source_file_sets roots_state [];
    let after_clear = ws_for_document_uri roots_state file_uri == initial_ws in
    let source_root_uri = uri_of_path "source root" source_root in
    let set : Req.source_file_set =
      {
        source_root_uri;
        source_file_uris = [ file_uri ];
        source_search_truncated = false;
      }
    in
    apply_source_file_sets roots_state [ set ];
    let after_replace_owner =
      ws_for_document_uri roots_state file_uri == initial_ws
    in
    let after_replace_route = ws_for_uri roots_state file_uri == initial_ws in
    before && after_clear && after_replace_owner && after_replace_route
end

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
  let diagnostic_push_enabled = ref true in
  let diagnostic_refresh_supported = ref false in
  let next_server_request_id = ref 1 in
  let pending_change_refreshes : (Workspace.t, int) Hashtbl.t =
    Hashtbl.create 16
  in
  reset_all_refresh_counters pending_change_refreshes roots_state;
  let published_diags : (string, string) Hashtbl.t = Hashtbl.create 128 in
  let cancelled_request_ids : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  let cancel_mtx = Mutex.create () in
  let in_flight_request_ids : (string, bool) Hashtbl.t = Hashtbl.create 64 in
  let box = inbox_create ~max_items:!runtime_settings.inbox_max_items () in
  let scheduler = request_scheduler_create () in
  let active_request : active_request option ref = ref None in
  let active_request_mtx = Mutex.create () in
  let reader_done = ref false in
  let last_message_ms = ref (Perf_stats.now_ms ()) in
  let next_index_health_check_ms =
    ref (Perf_stats.now_ms () +. index_health_check_interval_ms)
  in
  let max_inbound_msg_bytes = 128 * 1024 * 1024 in
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
    Mutex.lock cancel_mtx;
    Hashtbl.replace cancelled_request_ids (request_id_key id) true;
    Mutex.unlock cancel_mtx
  in
  let request_is_cancelled (req_key : string) : bool =
    Mutex.lock cancel_mtx;
    let hit = Hashtbl.mem cancelled_request_ids req_key in
    Mutex.unlock cancel_mtx;
    hit
  in
  let clear_cancelled (req_key : string) : unit =
    Mutex.lock cancel_mtx;
    Hashtbl.remove cancelled_request_ids req_key;
    Mutex.unlock cancel_mtx
  in
  let set_active_request (active : active_request) : unit =
    Mutex.lock active_request_mtx;
    active_request := Some active;
    Mutex.unlock active_request_mtx
  in
  let clear_active_request (req_key : string) : unit =
    Mutex.lock active_request_mtx;
    (match !active_request with
    | Some active when active.ar_key = req_key -> active_request := None
    | _ -> ());
    Mutex.unlock active_request_mtx
  in
  let active_request_preempted (req_key : string) : bool =
    Mutex.lock active_request_mtx;
    let out =
      match !active_request with
      | Some active when active.ar_key = req_key -> active.ar_preempted
      | _ -> false
    in
    Mutex.unlock active_request_mtx;
    out
  in
  let maybe_preempt_active (incoming : request_priority) : unit =
    Mutex.lock active_request_mtx;
    (match !active_request with
    | Some active
      when (not active.ar_preempted)
           && incoming_preempts_active ~active:active.ar_priority ~incoming ->
        active.ar_preempted <- true;
        Perf_stats.tick "scheduler.preempt";
        Perf_log.log_event "request_preempted"
          ~uri:active.ar_method
          ~queue:(request_priority_rank incoming)
    | _ -> ());
    Mutex.unlock active_request_mtx
  in
  let cancel_id_of_raw_message (msg : string) : Yojson.Safe.t option =
    try
      let j = Yojson.Safe.from_string msg in
      match method_of_msg j with
      | Some "$/cancelRequest" when not (is_request j) ->
          parse_cancel_request_id (params_of_msg j)
      | _ -> None
    with _ -> None
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
               (match cancel_id_of_raw_message msg with
               | Some cancel_id -> mark_cancelled cancel_id
               | None ->
                   (try
                      let json = Yojson.Safe.from_string msg in
                      match method_of_msg json with
                      | Some method_ when is_request json ->
                          maybe_preempt_active
                            (request_priority_of_method method_)
                      | _ -> ()
                    with _ -> ());
                   inbox_push box (InboxMsg msg));
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

  let background_should_stop () =
    !reader_done || (not (inbox_is_empty box))
    || not (request_scheduler_is_empty scheduler)
  in

  let background_grace_elapsed () =
    Perf_stats.now_ms () -. !last_message_ms >= interactive_background_grace_ms
  in

  let request_diagnostic_refresh_if_needed changed =
    if changed && not !diagnostic_push_enabled then
      request_diagnostic_refresh oc
        ~refresh_supported:!diagnostic_refresh_supported next_server_request_id
  in

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
                let priority = request_priority_of_method m in
                set_active_request
                  {
                    ar_key = req_key;
                    ar_method = m;
                    ar_priority = priority;
                    ar_preempted = false;
                  };
                let is_cancelled () =
                  request_is_cancelled req_key
                  || active_request_preempted req_key
                in
                (try
                   Perf_log.with_request_kind_opt
                     (Perf_log.request_kind_of_lsp_method m)
                     (fun () ->
                       Perf_stats.time ("req." ^ m) (fun () ->
                           handle_request roots_state
                             ~all_workspaces:all_workspaces_now
                             ~pending_change_refreshes
                             ~semantic_refresh_supported
                             ~diagnostic_push_enabled
                             ~diagnostic_refresh_supported ~is_cancelled oc m
                             id (params_of_msg json)))
                 with exn ->
                   if is_cancelled () then respond_cancelled oc ~id
                   else
                     respond_error oc ~id ~code:(-32603)
                       ~message:
                        (Printf.sprintf "%s failed: %s" m
                           (Printexc.to_string exn)));
                Hashtbl.remove in_flight_request_ids req_key;
                clear_active_request req_key;
                clear_cancelled req_key)
          else
            try
              Perf_stats.time ("notif." ^ m) (fun () ->
                  handle_notification roots_state ~semantic_refresh_supported
                    ~next_server_request_id ~diagnostic_push_enabled
                    ~diagnostic_refresh_supported ~pending_change_refreshes
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
    let wss =
      all_workspaces_now ()
      |> List.filter Workspace.background_work_pending
    in
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
            ~should_stop:background_should_stop
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
      |> List.filter (fun ws ->
             Workspace.background_work_pending ws
             && not (Workspace.startup_is_ready_now ws))
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
                ~should_stop:background_should_stop
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

  let run_index_health_check_if_due () : unit =
    let now = Perf_stats.now_ms () in
    if now < !next_index_health_check_ms then ()
    else (
      next_index_health_check_ms := now +. index_health_check_interval_ms;
      all_workspaces_now ()
      |> List.iter (fun ws ->
             try
               if Workspace.ensure_index_health ws then
                 Perf_stats.tick "index.health_watchdog_repair"
             with exn ->
               Perf_stats.tick "loop.index_health_exception";
               prerr_endline
                 (Printf.sprintf "index health check failed: %s"
                    (Printexc.to_string exn))))
  in

  let publish_background_for_all () : unit =
    all_workspaces_now ()
    |> List.iter (fun ws ->
        let stop_requested = background_should_stop () in
        if stop_requested then Perf_stats.tick "loop.publish_deferred";
        try
          let open_diag_batch_size =
            startup_open_diag_batch_size ws
              ~base:!runtime_settings.open_diag_revalidate_batch_size
          in
          let open_changed =
            if stop_requested then false
            else
              publish_open_diag_revalidate_updates ws oc published_diags
                ~push:!diagnostic_push_enabled ~max_items:open_diag_batch_size
          in
          let bg_diag_batch_size =
            if stop_requested then
              max 1 (min 8 !runtime_settings.bg_diag_batch_size)
            else !runtime_settings.bg_diag_batch_size
          in
          let bg_changed =
            publish_background_diag_updates ws oc published_diags
              ~push:!diagnostic_push_enabled ~max_items:bg_diag_batch_size
          in
          request_diagnostic_refresh_if_needed (open_changed || bg_changed);
          if background_should_stop () then
            Perf_stats.tick "loop.publish_snapshot_deferred"
          else if
            Workspace.open_doc_count ws > 0
            && Workspace.startup_is_ready_now ws
            && Workspace.cached_snapshot ws = None
          then
            Perf_stats.tick "loop.publish_snapshot_skipped_eager"
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

  let enqueue_or_handle_message (msg : string) : unit =
    match cancel_id_of_raw_message msg with
    | Some cancel_id -> mark_cancelled cancel_id
    | None -> (
        match request_rank_of_raw_message msg with
        | Some rank ->
            request_scheduler_enqueue scheduler ~rank ~msg;
            Perf_stats.tick "scheduler.queued_request"
        | None -> handle_raw_message msg)
  in

  let dispatch_one_scheduled_request () : bool =
    match request_scheduler_pop scheduler with
    | None -> false
    | Some msg ->
        Perf_stats.tick "scheduler.dispatch";
        handle_raw_message msg;
        true
  in

  let rec loop () =
    if
      !reader_done && inbox_is_empty box
      && request_scheduler_is_empty scheduler
    then ()
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
      let items =
        if items <> [] then items
        else (
          Thread.delay 0.001;
          Perf_stats.time "idle.msg_redrain_after_yield" (fun () ->
              inbox_drain box))
      in
      let items = ref items in
      let ran_idle_background = ref false in
      if !items <> [] then last_message_ms := Perf_stats.now_ms ();
      (if !items = [] then (
         if
           readable = [] && request_scheduler_is_empty scheduler
           && background_grace_elapsed ()
         then (
           Perf_stats.tick "idle.bg_tick";
           run_background_tick ();
           ran_idle_background := true;
           Thread.delay 0.005;
           (try drain_wake_pipe ~nonblock:wake_nonblock wake_r with _ -> ());
           let late_items =
             Perf_stats.time "idle.msg_drain_after_bg" (fun () ->
                 inbox_drain box)
           in
           if late_items <> [] then (
             items := late_items;
             last_message_ms := Perf_stats.now_ms ())))
       else
         List.iter
           (function
             | InboxMsg msg -> enqueue_or_handle_message msg
             | InboxInvalidFrame msg ->
                 prerr_endline
                   (Printf.sprintf
                      "invalid LSP frame received; terminating session: %s" msg);
                 reader_done := true
             | InboxEof -> reader_done := true)
           !items);
      let dispatched =
        if inbox_is_empty box then dispatch_one_scheduled_request () else false
      in
      if !items <> [] || dispatched then last_message_ms := Perf_stats.now_ms ();
      (if !items <> [] || dispatched then
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
      let should_continue =
        (not !reader_done)
        || (not (inbox_is_empty box))
        || not (request_scheduler_is_empty scheduler)
      in
      if should_continue then (
        if
          (not !reader_done) && !items = [] && not dispatched
          && inbox_is_empty box && request_scheduler_is_empty scheduler
          && background_grace_elapsed ()
        then (
          if not !ran_idle_background then (
            run_index_health_check_if_due ();
            run_startup_fair_tick_if_needed ());
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
