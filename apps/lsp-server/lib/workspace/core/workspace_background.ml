module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_index_graph
open Workspace_imports
open Workspace_semantics
open Workspace_tuning

type semantic_validation_mode =
  | SemanticFull
  | SemanticRangeSemi of T.Range.t

let store_doc
    ?(import_lookup_pump:bool=true)
    ?(semantic_mode:semantic_validation_mode=SemanticFull)
    (ws:t)
    (uri:T.DocumentUri.t)
    (doc:Document.t)
  : unit =
  let old_doc = Hashtbl.find_opt ws.docs uri in
  let old_compool_key = Option.bind old_doc compool_key_of_doc in
  let import_diags =
    try
      validate_imports ~pump_lookup:import_lookup_pump ws doc
    with exn ->
      [diag_internal_phase_failure ~phase:"import" doc exn]
  in
  let semantic_diags =
    if doc.Document.ast = None || doc.Document.parse_diags <> [] then
      []
    else
      (match semantic_mode with
       | SemanticFull ->
           (try
              validate_semantics ws doc
            with exn ->
              [diag_internal_phase_failure ~phase:"semantic" doc exn])
       | SemanticRangeSemi _ ->
           [])
  in
  let doc = Document.with_import_diags (import_diags @ semantic_diags) doc in
  mark_graph_dirty ws;
  let new_compool_key = compool_key_of_doc doc in
  let has_compool (d:Document.t) =
    match d.Document.compool_def with
    | None -> false
    | Some name -> normalize_name name <> ""
  in
  if has_compool doc || Option.fold ~none:false ~some:has_compool old_doc then
    invalidate_symbol_hints ws;
  Hashtbl.replace ws.docs uri doc;
  let revalidate_importers_for_compool (key:string option) : unit =
    match key with
    | None -> ()
    | Some key ->
        let importers = importer_uris_for_compool_key ws ~compool_key:key in
        invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
        List.iter
          (fun importer_uri ->
            enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
          importers
  in
  revalidate_importers_for_compool old_compool_key;
  revalidate_importers_for_compool new_compool_key;
  if ws.sem_store_enabled then (
    Semantic_store.remove_uri ws.semantic_store ~uri;
  );
  match doc.Document.file with
  | None -> ()
  | Some f ->
      let path_key = normalize_path_key f in
      Hashtbl.replace ws.files path_key doc;
      Hashtbl.replace ws.bg_parsed path_key true;
      Hashtbl.remove ws.closed_doc_last_touch path_key

let store_doc_fast (ws:t) (uri:T.DocumentUri.t) (doc:Document.t) : unit =
  mark_graph_dirty ws;
  Hashtbl.replace ws.docs uri doc;
  match doc.Document.file with
  | None -> ()
  | Some f ->
      let path_key = normalize_path_key f in
      Hashtbl.replace ws.files path_key doc;
      if doc.Document.parse_rev = doc.Document.rev then
        Hashtbl.replace ws.bg_parsed path_key true
      else
        Hashtbl.remove ws.bg_parsed path_key;
      Hashtbl.remove ws.closed_doc_last_touch path_key

let direct_import_paths (ws:t) (doc:Document.t) : string list =
  let acc = Hashtbl.create 16 in
  let resolve_compool_path (name:string) : string option =
    let key = normalize_name name in
    if key = "" then None
    else
      let from_index =
        match ws.index with
        | Some idx -> Workspace_index.find_compool idx ~name:key
        | None -> None
      in
      match from_index with
      | Some _ as p -> p
      | None ->
          if allow_fallback_scan ws then find_compool_path_fallback ws ~key else None
  in
  List.iter (fun (imp:Preprocess.import) ->
    match imp.kind with
    | Preprocess.Compool ->
        (match resolve_compool_path imp.name with
         | None -> ()
         | Some p -> Hashtbl.replace acc (normalize_path_key p) p)
  ) (Document.imports doc);
  Hashtbl.fold (fun _ p xs -> p :: xs) acc []

let enqueue_doc_imports_high (ws:t) (doc:Document.t) : unit =
  direct_import_paths ws doc
  |> List.iter (fun p -> enqueue_bg_path ws ~lane:LaneOpen ~reason_group:"open_import" ~high:true p)

let background_doc_with_diags (ws:t) (doc:Document.t) : Document.t =
  let import_diags =
    try
      validate_imports ~pump_lookup:false ws doc
    with exn ->
      [diag_internal_phase_failure ~phase:"import" doc exn]
  in
  let semantic_diags =
    if doc.Document.ast = None || doc.Document.parse_diags <> [] then
      []
    else
      (try
         validate_semantics ws doc
       with exn ->
         [diag_internal_phase_failure ~phase:"semantic" doc exn])
  in
  Document.with_import_diags (import_diags @ semantic_diags) doc

let queue_workspace_diag_update_for_doc (ws:t) (doc:Document.t) : unit =
  match doc.Document.file with
  | None -> ()
  | Some path ->
      (match find_open_doc_for_path ws ~path with
      | Some _ ->
          ()
      | None ->
           if bg_diag_allowed ws then (
             let uri_s = Uri_path.docuri_to_string doc.Document.uri in
             let filtered = filter_workspace_diags ws (Document.diagnostics doc) in
             let prev =
               match Hashtbl.find_opt ws.bg_closed_diags uri_s with
               | Some ds -> ds
               | None -> []
             in
             if prev <> filtered then (
               if filtered = [] then Hashtbl.remove ws.bg_closed_diags uri_s
               else Hashtbl.replace ws.bg_closed_diags uri_s filtered;
               enqueue_bg_diag_update ws ~uri:doc.Document.uri ~diags:filtered
             )
           ))

let refresh_bg_seed_paths (ws:t) : unit =
  ensure_graph_fresh ws;
  match ws.index with
  | None ->
      ws.bg_seed_paths <- [||];
      ws.bg_seed_cursor <- 0;
      Hashtbl.clear ws.quick_nav_index;
      Hashtbl.clear ws.quick_nav_done_set;
      ws.quick_nav_index_done <- 0;
      ws.quick_nav_index_total <- 0;
      Hashtbl.clear ws.quick_nav_pending_set;
      while not (Queue.is_empty ws.quick_nav_pending_paths) do
        ignore (Queue.pop ws.quick_nav_pending_paths)
      done;
      ws.bg_seed_needs_refresh <- false
  | Some idx ->
      ws.bg_seed_paths <- Array.of_list (Workspace_index.all_source_paths idx);
      ws.bg_seed_cursor <- 0;
      let total_unique = ref 0 in
      let seen = Hashtbl.create (max 16 (Array.length ws.bg_seed_paths)) in
      Array.iter (fun p ->
        let key = normalize_path_key p in
        if key <> "" && not (Hashtbl.mem seen key) then (
          Hashtbl.replace seen key true;
          incr total_unique;
          if not (Hashtbl.mem ws.quick_nav_done_set key)
             && not (Hashtbl.mem ws.quick_nav_pending_set key)
          then (
            Hashtbl.replace ws.quick_nav_pending_set key true;
            Queue.add p ws.quick_nav_pending_paths
          )
        )
      ) ws.bg_seed_paths;
      ws.quick_nav_index_total <- !total_unique;
      ws.quick_nav_index_done <- Hashtbl.length ws.quick_nav_done_set;
      ws.bg_seed_needs_refresh <- false

let seed_bg_paths_from_index (ws:t) : unit =
  ensure_graph_fresh ws;
  if ws.bg_seed_needs_refresh then
    refresh_bg_seed_paths ws;
  let per_tick =
    match workspace_pressure_mode ws with
    | PressureNormal -> bg_seed_paths_per_tick
    | PressureSoft -> max 1 (bg_seed_paths_per_tick / 4)
    | PressureCritical -> 0
  in
  if per_tick <= 0 then ()
  else (
    let added = ref 0 in
    while !added < per_tick
          && ws.graph_root_closure_cursor < Array.length ws.graph_root_closure_paths
    do
      let p = ws.graph_root_closure_paths.(ws.graph_root_closure_cursor) in
      ws.graph_root_closure_cursor <- ws.graph_root_closure_cursor + 1;
      enqueue_bg_path ws ~lane:LaneRoot ~reason_group:"root_closure" ~high:false p;
      incr added
    done;
    while !added < per_tick
          && ws.bg_seed_cursor < Array.length ws.bg_seed_paths
    do
      let p = ws.bg_seed_paths.(ws.bg_seed_cursor) in
      ws.bg_seed_cursor <- ws.bg_seed_cursor + 1;
      enqueue_bg_path ws ~lane:LaneSweep ~reason_group:"seed_sweep" ~high:false p;
      incr added
    done
  )

let quick_nav_proc_kind = 12

let parse_doc_worker
    ~(max_bytes:int)
    ~(uri:T.DocumentUri.t)
    ~(file:string option)
    ~(text:string)
  : Document.t =
  if is_parse_guard_exceeded ~max_bytes ~text_len:(String.length text) then
    Document.make_unparsed
      ~uri
      ~file
      ~text
      ~parse_diags:[diag_parse_guard ~file ~max_bytes ~actual_bytes:(String.length text)]
  else
    try
      Document.make ~uri ~file ~text
    with exn ->
      let fallback = Document.make ~uri ~file ~text:"" in
      with_internal_phase_diag fallback ~phase:"worker-parse" ~exn

let parse_job_path_key (job:parse_job) : string =
  match job.pj_payload with
  | ParseJobOpen x -> x.path_key
  | ParseJobPath x -> x.path_key

let parse_job_kind_of_queue_kind = function
  | BgQueueHighLarge -> Some ParseJobHighLarge
  | BgQueueRootLarge -> Some ParseJobRootLarge
  | BgQueueNormalLarge -> Some ParseJobNormalLarge
  | BgQueueHighSmall | BgQueueRootSmall | BgQueueNormalSmall -> None

let parse_worker_loop (ws:t) () : unit =
  let wait_job () =
    Mutex.lock ws.parse_worker_mtx;
    while (not ws.parse_worker_stop) && Queue.is_empty ws.parse_worker_jobs do
      Condition.wait ws.parse_worker_cv ws.parse_worker_mtx
    done;
    let job_opt =
      if ws.parse_worker_stop then None
      else Some (Queue.pop ws.parse_worker_jobs)
    in
    Mutex.unlock ws.parse_worker_mtx;
    job_opt
  in
  let push_result (res:parse_result) =
    Mutex.lock ws.parse_worker_mtx;
    Queue.add res ws.parse_worker_results;
    Mutex.unlock ws.parse_worker_mtx
  in
  let rec loop () =
    match wait_job () with
    | None -> ()
    | Some job ->
        let result =
          match job.pj_payload with
          | ParseJobOpen { path_key; uri; file; text; generation } ->
              let doc =
                parse_doc_worker
                  ~max_bytes:ws.parse_file_max_bytes
                  ~uri
                  ~file
                  ~text
              in
              ParseResultOpen
                {
                  pr_kind = job.pj_kind;
                  pr_epoch = job.pj_epoch;
                  path_key;
                  uri;
                  generation;
                  doc;
                }
          | ParseJobPath { path; path_key } ->
              let uri =
                match Uri_path.docuri_of_path path with
                | Some u -> u
                | None ->
                    (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
                     | u -> u
                     | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
              in
              let doc_opt =
                match read_file_text path with
                | None -> None
                | Some text ->
                    Some
                      (parse_doc_worker
                         ~max_bytes:ws.parse_file_max_bytes
                         ~uri
                         ~file:(Some path)
                         ~text)
              in
              ParseResultPath
                {
                  pr_kind = job.pj_kind;
                  pr_epoch = job.pj_epoch;
                  path;
                  path_key;
                  doc_opt;
                }
        in
        push_result result;
        loop ()
  in
  loop ()

let ensure_parse_worker_started (ws:t) : unit =
  if ws.parse_worker_started then ()
  else (
    ws.parse_worker_started <- true;
    ignore (Thread.create (parse_worker_loop ws) ())
  )

let try_submit_large_parse_job
    (ws:t)
    ~(path:string)
    ~(queue_kind:bg_queue_kind)
  : bool =
  match parse_job_kind_of_queue_kind queue_kind with
  | None -> false
  | Some pj_kind ->
      let path_key = normalize_path_key path in
      if path_key = "" then true
      else if Hashtbl.mem ws.parse_worker_inflight path_key then
        true
      else if Hashtbl.length ws.parse_worker_inflight >= ws.parse_worker_max_inflight then
        false
      else
        let payload =
          match find_open_doc_for_path ws ~path with
          | Some doc ->
              let generation =
                match Hashtbl.find_opt ws.open_parse_generation (Uri_path.docuri_to_string doc.Document.uri) with
                | Some g -> g
                | None -> 0
              in
              ParseJobOpen
                {
                  path_key;
                  uri = doc.Document.uri;
                  file = doc.Document.file;
                  text = doc.Document.text;
                  generation;
                }
          | None ->
              ParseJobPath { path; path_key }
        in
        let job = { pj_kind; pj_epoch = ws.parse_epoch; pj_payload = payload } in
        Mutex.lock ws.parse_worker_mtx;
        Queue.add job ws.parse_worker_jobs;
        Condition.signal ws.parse_worker_cv;
        Mutex.unlock ws.parse_worker_mtx;
        Hashtbl.replace ws.parse_worker_inflight path_key pj_kind;
        true

let quick_nav_entry_add (ws:t) ~(key:string) (entry:quick_nav_entry) : unit =
  let k = normalize_name key in
  if k = "" then ()
  else
    let prev =
      match Hashtbl.find_opt ws.quick_nav_index k with
      | Some xs -> xs
      | None -> []
    in
    if List.exists (fun x ->
         Uri_path.docuri_to_string x.qn_uri = Uri_path.docuri_to_string entry.qn_uri
         && x.qn_loc = entry.qn_loc) prev
    then ()
    else
      Hashtbl.replace ws.quick_nav_index k (entry :: prev)

let quick_nav_entries_of_path_prefix (path:string) ~(max_bytes:int) : quick_nav_entry list =
  match read_file_prefix_text path ~max_bytes with
  | None -> []
  | Some text ->
      let upper = String.uppercase_ascii text in
      let n = String.length upper in
      let idx = Text_index.of_string text in
      let uri =
        match Uri_path.docuri_of_path path with
        | Some u -> u
        | None ->
            (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
             | u -> u
             | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
      in
      let rec scan i acc =
        if i + 5 > n then List.rev acc
        else if String.sub upper i 5 = "PROC " then
          let s = i + 5 in
          let j = ref s in
          while !j < n && is_nav_ident_char upper.[!j] do
            incr j
          done;
          let key = String.sub upper s (!j - s) |> normalize_name in
          let acc =
            if key = "" then acc
            else
              let l0, c0 = Text_index.line_col_of_offset idx s in
              let l1, c1 = Text_index.line_col_of_offset idx !j in
              let loc =
                Ast.Loc.make
                  ~file:(Some path)
                  ~start_pos:{ line = l0 + 1; col = c0; offset = s }
                  ~end_pos:{ line = l1 + 1; col = c1; offset = !j }
              in
              {
                qn_uri = uri;
                qn_name = key;
                qn_key = key;
                qn_loc = loc;
                qn_kind = quick_nav_proc_kind;
                qn_container = None;
              } :: acc
          in
          scan (i + 1) acc
        else
          scan (i + 1) acc
      in
      scan 0 []

let quick_nav_index_step (ws:t) ~(budget_ms:int) : unit =
  if budget_ms <= 0 then ()
  else
    let deadline = Perf_stats.now_ms () +. float_of_int budget_ms in
    let rec loop () =
      if Perf_stats.now_ms () >= deadline then ()
      else if Queue.is_empty ws.quick_nav_pending_paths then ()
      else
        let path = Queue.pop ws.quick_nav_pending_paths in
        let path_key = normalize_path_key path in
        Hashtbl.remove ws.quick_nav_pending_set path_key;
        if path_key <> "" then (
          let entries =
            quick_nav_entries_of_path_prefix path ~max_bytes:nav_quick_scan_per_file_bytes
          in
          List.iter (fun e -> quick_nav_entry_add ws ~key:e.qn_key e) entries;
          if not (Hashtbl.mem ws.quick_nav_done_set path_key) then (
            Hashtbl.replace ws.quick_nav_done_set path_key true;
            ws.quick_nav_index_done <- ws.quick_nav_index_done + 1
          )
        );
        loop ()
    in
    loop ()

let apply_parse_result_open
    (ws:t)
    ~(pr_kind:parse_job_kind)
    ~(pr_epoch:int)
    ~(path_key:string)
    ~(uri:T.DocumentUri.t)
    ~(generation:int)
    ~(doc:Document.t)
  : unit =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else
  let latest_generation =
    match Hashtbl.find_opt ws.open_parse_generation (Uri_path.docuri_to_string uri) with
    | Some g -> g
    | None -> 0
  in
  if latest_generation <> generation then
    Perf_stats.tick "diag.open.stale_generation_drop"
  else (
    ignore pr_kind;
    store_doc ~import_lookup_pump:false ws uri doc;
    let uri_key = Uri_path.docuri_to_string uri in
    if doc.Document.parse_rev = doc.Document.rev then (
      enqueue_open_diag_revalidate ws ~uri ~reason:"open_parse";
      (match Hashtbl.find_opt ws.open_provisional_since_ms uri_key with
       | None -> ()
       | Some t0 ->
           let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
           Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
           Hashtbl.remove ws.open_provisional_since_ms uri_key);
      Hashtbl.remove ws.open_parse_generation uri_key
    ) else
      Hashtbl.replace ws.open_provisional_since_ms uri_key (Perf_stats.now_ms ());
    (match doc.Document.file with
     | Some p ->
         let pk = normalize_path_key p in
         Hashtbl.replace ws.files pk doc;
         Hashtbl.replace ws.bg_parsed pk true;
         Hashtbl.remove ws.closed_doc_last_touch pk
     | None -> ());
    enqueue_doc_imports_high ws doc;
    (match compool_key_of_doc doc with
     | None -> ()
     | Some key ->
         let importers = importer_uris_for_compool_key ws ~compool_key:key in
         invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
         List.iter
           (fun importer_uri ->
             enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
           importers);
    invalidate_lsif_snapshot ws
  )

let apply_parse_result_path
    (ws:t)
    ~(pr_kind:parse_job_kind)
    ~(pr_epoch:int)
    ~(path_key:string)
    ~(doc_opt:Document.t option)
  : unit =
  Hashtbl.remove ws.parse_worker_inflight path_key;
  if pr_epoch <> ws.parse_epoch then
    Perf_stats.tick "bg.parse.stale_result_drop"
  else
    match doc_opt with
    | None -> ()
    | Some doc ->
        let doc =
          match pr_kind with
          | ParseJobHighLarge ->
              background_doc_with_diags ws doc
          | ParseJobRootLarge ->
              background_doc_with_diags ws doc
          | ParseJobNormalLarge ->
              if ws.startup_fully_nav_ready_ms <> None then
                background_doc_with_diags ws doc
              else
                doc
        in
        invalidate_lsif_snapshot ws;
        Hashtbl.replace ws.files path_key doc;
        Hashtbl.replace ws.bg_parsed path_key true;
        touch_closed_doc_path ws ~path_key;
        evict_closed_docs_if_needed ws;
        (match compool_key_of_doc doc with
         | None -> ()
         | Some key ->
             let importers = importer_uris_for_compool_key ws ~compool_key:key in
             invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
             List.iter
               (fun importer_uri ->
                 enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
               importers);
        queue_workspace_diag_update_for_doc ws doc

let drain_parse_worker_results (ws:t) ~(max_items:int) : unit =
  if max_items <= 0 then ()
  else
    let rec loop n =
      if n <= 0 then ()
      else (
        Mutex.lock ws.parse_worker_mtx;
        let next =
          if Queue.is_empty ws.parse_worker_results then None
          else Some (Queue.pop ws.parse_worker_results)
        in
        Mutex.unlock ws.parse_worker_mtx;
        match next with
        | None -> ()
        | Some (ParseResultOpen x) ->
            apply_parse_result_open ws
              ~pr_kind:x.pr_kind
              ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key
              ~uri:x.uri
              ~generation:x.generation
              ~doc:x.doc;
            loop (n - 1)
        | Some (ParseResultPath x) ->
            apply_parse_result_path ws
              ~pr_kind:x.pr_kind
              ~pr_epoch:x.pr_epoch
              ~path_key:x.path_key
              ~doc_opt:x.doc_opt;
            loop (n - 1))
    in
    loop max_items

let background_parse_path (ws:t) (path:string) : unit =
  let path_key = normalize_path_key path in
  if path_key = "" || Hashtbl.mem ws.bg_parsed path_key then ()
  else
    match find_open_doc_for_path ws ~path with
    | Some open_doc ->
        let uri = open_doc.Document.uri in
        let uri_key = Uri_path.docuri_to_string uri in
        if open_doc.Document.parse_rev = open_doc.Document.rev
           && not (Hashtbl.mem ws.open_parse_generation uri_key)
        then (
          Hashtbl.replace ws.bg_parsed path_key true;
          Hashtbl.remove ws.closed_doc_last_touch path_key
        ) else (
          let parse_generation =
            match Hashtbl.find_opt ws.open_parse_generation uri_key with
            | Some g -> g
            | None -> 0
          in
          let parsed_doc =
            if open_doc.Document.parse_rev = open_doc.Document.rev then
              open_doc
            else
              (try
                 Perf_stats.time "parse.open_doc_deferred" (fun () ->
                 parse_guarded_document_make ws
                     ~uri:open_doc.Document.uri
                     ~file:open_doc.Document.file
                     ~text:open_doc.Document.text)
               with exn ->
                 with_internal_phase_diag open_doc ~phase:"open-doc-deferred" ~exn)
          in
          let latest_generation =
            match Hashtbl.find_opt ws.open_parse_generation uri_key with
            | Some g -> g
            | None -> 0
          in
          if latest_generation <> parse_generation then
            Perf_stats.tick "diag.open.stale_generation_drop"
          else (
            store_doc ~import_lookup_pump:false ws uri parsed_doc;
            if parsed_doc.Document.parse_rev = parsed_doc.Document.rev then (
              let key = Uri_path.docuri_to_string uri in
              enqueue_open_diag_revalidate ws ~uri ~reason:"open_parse";
              (match Hashtbl.find_opt ws.open_provisional_since_ms key with
               | None -> ()
               | Some t0 ->
                   let lag = max 0.0 (Perf_stats.now_ms () -. t0) in
                   Perf_stats.observe_ms "diag.open.authoritative_lag_ms" lag;
                   Hashtbl.remove ws.open_provisional_since_ms key);
              Hashtbl.remove ws.open_parse_generation key
            ) else
              Hashtbl.replace ws.open_provisional_since_ms
                (Uri_path.docuri_to_string uri)
                (Perf_stats.now_ms ());
            invalidate_lsif_snapshot ws;
            Hashtbl.replace ws.files path_key parsed_doc;
            Hashtbl.replace ws.bg_parsed path_key true;
            Hashtbl.remove ws.closed_doc_last_touch path_key;
            enqueue_doc_imports_high ws parsed_doc;
            (match compool_key_of_doc parsed_doc with
             | None -> ()
             | Some key ->
                 let importers = importer_uris_for_compool_key ws ~compool_key:key in
                 invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
                 List.iter
                   (fun importer_uri ->
                     enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
                   importers)
          )
        )
    | None ->
        let uri =
          match Uri_path.docuri_of_path path with
          | Some u -> u
          | None ->
              (match T.DocumentUri.t_of_yojson (`String (Uri_path.file_uri_of_path path)) with
               | u -> u
               | exception _ -> T.DocumentUri.t_of_yojson (`String "file:///"))
        in
        let doc_opt =
          match file_size_bytes path with
          | Some n when is_parse_guard_exceeded ~max_bytes:ws.parse_file_max_bytes ~text_len:n ->
              Some (make_doc_with_parse_guard ws ~uri ~file:(Some path) ~text:"" ~actual_bytes:n)
          | _ ->
              (match read_file_text path with
               | None -> None
               | Some text ->
                   let doc0 =
                     try parse_guarded_document_make ws ~uri ~file:(Some path) ~text
                     with exn ->
                       let fallback = Document.make ~uri ~file:(Some path) ~text:"" in
                       with_internal_phase_diag fallback ~phase:"bg-open-doc" ~exn
                   in
                   Some (background_doc_with_diags ws doc0))
        in
        (match doc_opt with
         | None -> ()
         | Some doc ->
             invalidate_lsif_snapshot ws;
             Hashtbl.replace ws.files path_key doc;
             Hashtbl.replace ws.bg_parsed path_key true;
             touch_closed_doc_path ws ~path_key;
             evict_closed_docs_if_needed ws;
             (match compool_key_of_doc doc with
              | None -> ()
             | Some key ->
                  let importers = importer_uris_for_compool_key ws ~compool_key:key in
                  invalidate_importer_nav_state_for_compool_key ws ~compool_key:key;
                  List.iter
                    (fun importer_uri ->
                      enqueue_open_diag_revalidate ws ~uri:importer_uri ~reason:"compool_change")
                    importers);
             queue_workspace_diag_update_for_doc ws doc)

let revalidate_closed_docs_for_hint_readiness (ws:t) : unit =
  Hashtbl.iter (fun path_key doc ->
    if has_open_doc_for_path_key ws ~path_key then ()
    else
      let updated = background_doc_with_diags ws doc in
      if Document.diagnostics updated <> Document.diagnostics doc then (
        Hashtbl.replace ws.files path_key updated;
        queue_workspace_diag_update_for_doc ws updated
      )
  ) ws.files

let maybe_build_symbol_hints_background (ws:t) : unit =
  if ws.symbol_hints <> None then ()
  else
    match ws.index with
    | None -> ()
    | Some idx ->
        if Queue.is_empty ws.bg_high_small_queue
           && Queue.is_empty ws.bg_norm_small_queue
           && Queue.is_empty ws.bg_high_large_queue
           && Queue.is_empty ws.bg_norm_large_queue
          && Workspace_index.is_complete idx
          && workspace_pressure_mode ws <> PressureCritical
        then (
          let idx =
            Perf_stats.time "bg.hint_build" (fun () -> symbol_hint_index ws)
          in
          ws.symbol_hints <- Some idx;
          revalidate_closed_docs_for_hint_readiness ws;
          enqueue_all_open_diag_revalidate ws ~reason:"hint_ready";
          Perf_stats.tick "diag.warmup_revalidate"
        )

let background_tick
    (ws:t)
    ~(budget_ms:int)
    ~(mode:bg_tick_mode)
    ~(idle_quiet_ms:int)
    ~(last_message_ms:float)
  : unit =
  if budget_ms <= 0 then ()
  else (
    ensure_parse_worker_started ws;
    Perf_stats.tick "bg.tick";
    Perf_stats.tick "startup.phase_tick";
    update_pressure_state ws;
    pump_index_background ws;
    if workspace_pressure_mode ws <> PressureCritical then
      seed_bg_paths_from_index ws;
    let diag_stage_pending =
      ws.startup_diag_hover_ready_ms = None
      || Hashtbl.length ws.open_parse_generation > 0
      || not (startup_open_diag_revalidate_empty ws)
    in
    let parse_result_budget =
      match mode with
      | BgTickInteractive ->
          if diag_stage_pending then 10
          else if ws.startup_fully_nav_ready_ms = None then 4
          else 2
      | BgTickIdle -> 6
    in
    drain_parse_worker_results ws ~max_items:parse_result_budget;
    let quick_nav_budget =
      match mode with
      | BgTickInteractive ->
          if diag_stage_pending then
            max 1 (budget_ms / 6)
          else if ws.startup_fully_nav_ready_ms = None then
            max 2 (budget_ms / 2)
          else
            max 1 (budget_ms / 4)
      | BgTickIdle -> max 1 (budget_ms / 3)
    in
    quick_nav_index_step ws ~budget_ms:quick_nav_budget;
    let budget =
      match workspace_pressure_mode ws with
      | PressureNormal -> float_of_int budget_ms
      | PressureSoft -> float_of_int (max 1 (budget_ms / 2))
      | PressureCritical -> float_of_int (max 1 (budget_ms / 4))
    in
    let required_idle_quiet_ms =
      max 0 (max idle_quiet_ms ws.bg_large_parse_idle_quiet_ms)
    in
    let allow_normal_large =
      match mode with
      | BgTickInteractive ->
          ws.startup_fully_nav_ready_ms = None
          && Queue.is_empty ws.bg_high_large_queue
          && Hashtbl.length ws.parse_worker_inflight < ws.parse_worker_max_inflight
      | BgTickIdle ->
          let since_last_msg = Perf_stats.now_ms () -. last_message_ms in
          since_last_msg >= float_of_int required_idle_quiet_ms
    in
    let allow_root_large =
      match mode with
      | BgTickInteractive ->
          ws.startup_diag_hover_ready_ms = None
          || Hashtbl.length ws.open_parse_generation > 0
      | BgTickIdle ->
          let since_last_msg = Perf_stats.now_ms () -. last_message_ms in
          since_last_msg >= float_of_int required_idle_quiet_ms
    in
    let prefer_open_base =
      ws.startup_diag_hover_ready_ms = None
      || Hashtbl.length ws.open_parse_generation > 0
    in
    let processed_total = ref 0 in
    let processed_open = ref 0 in
    let required_open_share =
      let pct = max 0 (min 100 ws.sched_open_doc_min_share_pct) in
      float_of_int pct /. 100.0
    in
    if mode = BgTickIdle
       && (not allow_normal_large)
       && not (Queue.is_empty ws.bg_norm_large_queue)
    then
      Perf_stats.tick "bg.parse.large_deferred_busy";
    let deadline = Perf_stats.now_ms () +. budget in
    let keep = ref true in
    while !keep && Perf_stats.now_ms () < deadline do
      let prefer_open =
        if not prefer_open_base then false
        else if !processed_total <= 0 then true
        else
          let share = float_of_int !processed_open /. float_of_int !processed_total in
          share < required_open_share
      in
      match dequeue_bg_path ws ~mode ~allow_normal_large ~allow_root_large ~prefer_open with
      | None ->
          keep := false
      | Some (path, prio) ->
          incr processed_total;
          let path_key = normalize_path_key path in
          if has_open_doc_for_path_key ws ~path_key then incr processed_open;
          (match lane_of_bg_queue_kind prio with
           | LaneOpen -> Perf_stats.tick "sched.lane_a_ticks"
           | LaneRoot -> Perf_stats.tick "sched.lane_b_ticks"
           | LaneSweep -> Perf_stats.tick "sched.lane_c_ticks");
          if prio = BgQueueNormalSmall
             && workspace_pressure_mode ws = PressureCritical
          then (
            enqueue_bg_path ws ~lane:LaneSweep ~reason_group:"pressure_backoff" ~high:false path;
            keep := false
          ) else (
            (match prio with
             | BgQueueHighSmall -> Perf_stats.tick "bg.queue_high"
             | BgQueueRootSmall -> Perf_stats.tick "bg.queue_root"
             | BgQueueNormalSmall -> Perf_stats.tick "bg.queue_normal"
             | BgQueueHighLarge | BgQueueRootLarge | BgQueueNormalLarge ->
                 Perf_stats.tick "bg.parse.large_started");
            (match prio with
             | BgQueueHighLarge | BgQueueRootLarge | BgQueueNormalLarge ->
                 if try_submit_large_parse_job ws ~path ~queue_kind:prio then
                   ()
                 else (
                   let lane =
                     match prio with
                     | BgQueueRootLarge -> LaneRoot
                     | BgQueueHighLarge ->
                         if has_open_doc_for_path_key ws ~path_key:(normalize_path_key path) then
                           LaneOpen
                         else
                           LaneSweep
                     | _ -> LaneSweep
                   in
                   enqueue_bg_path ws ~lane ~high:(prio = BgQueueHighLarge || prio = BgQueueRootLarge) path;
                   keep := false
                 )
             | BgQueueHighSmall | BgQueueRootSmall | BgQueueNormalSmall ->
                 ignore
                   (Perf_stats.time "bg.parse_doc" (fun () ->
                      background_parse_path ws path)))
          )
    done
    ;
    maybe_build_symbol_hints_background ws;
    update_startup_ready_state ws
  )

let drain_pending_diag_updates
    (ws:t)
    ~(max_items:int)
  : (T.DocumentUri.t * T.Diagnostic.t list) list =
  if max_items <= 0 then []
  else if not (bg_diag_allowed ws) then []
  else
    let rec loop n acc =
      if n <= 0 || Queue.is_empty ws.bg_pending_diag_updates then
        List.rev acc
      else
        let key = Queue.pop ws.bg_pending_diag_updates in
        Hashtbl.remove ws.bg_pending_diag_set key;
        (match Hashtbl.find_opt ws.bg_pending_diag_payloads key with
         | None ->
             loop n acc
         | Some (uri, diags) ->
             Hashtbl.remove ws.bg_pending_diag_payloads key;
             if Hashtbl.mem ws.docs uri then (
               Perf_stats.tick "diag.open.stale_generation_drop";
               loop n acc
             ) else
               loop (n - 1) ((uri, diags) :: acc))
    in
    loop max_items []

let drain_open_diag_revalidate_uris
    (ws:t)
    ~(max_items:int)
  : T.DocumentUri.t list =
  if max_items <= 0 then []
  else
    let rec loop n acc =
      if n <= 0 || Queue.is_empty ws.open_diag_revalidate_updates then
        List.rev acc
      else
        let key = Queue.pop ws.open_diag_revalidate_updates in
        Hashtbl.remove ws.open_diag_revalidate_set key;
        match Hashtbl.find_opt ws.open_diag_revalidate_payloads key with
        | None ->
            loop n acc
        | Some (uri, _reason) ->
            Hashtbl.remove ws.open_diag_revalidate_payloads key;
            (match Hashtbl.find_opt ws.docs uri with
             | None ->
                 loop n acc
             | Some doc ->
                 store_doc ~import_lookup_pump:false ws uri doc;
                 Perf_stats.tick "diag.open.revalidate_drained";
                 loop (n - 1) (uri :: acc))
    in
    let out = loop max_items [] in
    if out <> [] then update_startup_ready_state ws;
    out

let startup_diag_hover_ready_now (ws:t) : bool =
  update_startup_ready_state ws;
  ws.startup_diag_hover_ready_ms <> None

let startup_is_ready_now (ws:t) : bool =
  update_startup_ready_state ws;
  ws.startup_ready_ms <> None

let startup_readiness_json_for_report (ws:t) : Yojson.Safe.t =
  update_startup_ready_state ws;
  startup_readiness_json ws

