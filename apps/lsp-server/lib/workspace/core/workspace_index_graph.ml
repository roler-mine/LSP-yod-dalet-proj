module T = Lsp.Types
open Workspace_foundation
open Workspace_state
open Workspace_runtime
open Workspace_tuning

let is_network_root (ws : t) : bool =
  match ws.root_path with Some p -> is_probably_network_path p | None -> false

let allow_fallback_scan (ws : t) : bool = not (is_network_root ws)

let lookup_scan_budget (ws : t) : int * int =
  if is_network_root ws then
    (index_lookup_dirs_network, index_lookup_files_network)
  else (index_lookup_dirs, index_lookup_files)

let ensure_index_started (ws : t) : unit =
  match (ws.root_path, ws.index) with
  | Some root, None ->
      let idx = Workspace_index.start ~root in
      if Workspace_index.checkpoint_loaded idx then
        Perf_stats.tick "index.checkpoint_loaded";
      ws.index <- Some idx
  | _ -> ()

let pump_index (ws : t) ~(max_dirs : int) ~(max_files : int) : unit =
  ensure_index_started ws;
  match ws.index with
  | None -> ()
  | Some idx -> (
      try
        let was_reconciling = Workspace_index.reconcile_pending idx in
        let stale_before = Workspace_index.reconcile_stale_pruned idx in
        let _dirs, files = Workspace_index.scan_step idx ~max_dirs ~max_files in
        if files > 0 then ws.bg_seed_needs_refresh <- true;
        if was_reconciling && not (Workspace_index.reconcile_pending idx) then (
          Perf_stats.tick "index.reconcile_completed";
          if Workspace_index.reconcile_stale_pruned idx > stale_before then
            Perf_stats.tick "index.reconcile_pruned_stale";
          ws.bg_seed_needs_refresh <- true;
          enqueue_all_open_diag_revalidate ws ~reason:"reconcile")
      with _ -> ())

let pump_index_background (ws : t) : unit =
  if is_network_root ws then
    pump_index ws ~max_dirs:index_background_dirs_network
      ~max_files:index_background_files_network
  else
    pump_index ws ~max_dirs:index_background_dirs
      ~max_files:index_background_files

let pump_index_lookup (ws : t) : unit =
  let max_dirs, max_files = lookup_scan_budget ws in
  pump_index ws ~max_dirs ~max_files

let index_checkpoint_loaded_for_report (ws : t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.checkpoint_loaded idx

let index_reconcile_pending_for_report (ws : t) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.reconcile_pending idx

let index_reconcile_epoch_for_report (ws : t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_epoch idx

let index_reconcile_sources_before_for_report (ws : t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_sources_before idx

let index_reconcile_sources_after_for_report (ws : t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_sources_after idx

let index_reconcile_stale_pruned_for_report (ws : t) : int =
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.reconcile_stale_pruned idx

let is_import_word_char = function
  | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let tokenize_upper_words_prefix ~(max_chars : int) (text : string) : string list
    =
  let n = min (String.length text) (max 0 max_chars) in
  let upper =
    if n = String.length text then String.uppercase_ascii text
    else String.uppercase_ascii (String.sub text 0 n)
  in
  let out = ref [] in
  let rec scan i =
    if i >= n then ()
    else if is_import_word_char upper.[i] then (
      let j = ref (i + 1) in
      while !j < n && is_import_word_char upper.[!j] do
        incr j
      done;
      out := String.sub upper i (!j - i) :: !out;
      scan !j)
    else scan (i + 1)
  in
  scan 0;
  List.rev !out

let quick_imports_from_deferred_doc_text (doc : Document.t) :
    Preprocess.import list =
  let tokens =
    tokenize_upper_words_prefix ~max_chars:nav_miss_import_scan_max_chars
      (Document.text doc)
  in
  let seen = Hashtbl.create 16 in
  let out = ref [] in
  let loc = Ast.Loc.none in
  let push_name (raw : string) =
    let name = normalize_name raw in
    if name <> "" && not (Hashtbl.mem seen name) then (
      Hashtbl.replace seen name true;
      out := { Preprocess.kind = Preprocess.Compool; name; loc } :: !out)
  in
  let rec collect = function
    | ("COMPOOL" | "ICOMPOOL") :: name :: tl ->
        push_name name;
        collect tl
    | _ :: tl -> collect tl
    | [] -> ()
  in
  collect tokens;
  List.rev !out

let best_effort_doc_imports_for_scheduling (doc : Document.t) :
    Preprocess.import list =
  let imports = Document.imports doc in
  if imports <> [] then imports
  else if doc.Document.parse_rev = doc.Document.rev then []
  else quick_imports_from_deferred_doc_text doc

let uniq_norm_strings (xs : string list) : string list =
  let seen = Hashtbl.create (max 16 (List.length xs)) in
  let out = ref [] in
  List.iter
    (fun x ->
      let k = normalize_name x in
      if k <> "" && not (Hashtbl.mem seen k) then (
        Hashtbl.replace seen k true;
        out := k :: !out))
    xs;
  List.rev !out

let resolve_manual_root_file (ws : t) (raw : string) : string =
  if raw = "" then raw
  else if Filename.is_relative raw then
    match ws.root_path with
    | Some root -> Filename.concat root raw
    | None -> raw
  else raw

let graph_entry_hint_for_path (ws : t) ~(path : string) : bool =
  match ws.index with
  | None -> false
  | Some idx -> Workspace_index.source_entry_hint idx ~path

let graph_import_hints_for_path (ws : t) ~(path : string)
    ~(doc_opt : Document.t option) : string list =
  match doc_opt with
  | Some doc ->
      best_effort_doc_imports_for_scheduling doc
      |> List.filter_map (fun (imp : Preprocess.import) ->
          match imp.kind with
          | Preprocess.Compool ->
              let key = normalize_name imp.name in
              if key = "" then None else Some key)
      |> uniq_norm_strings
  | None -> (
      match ws.index with
      | None -> []
      | Some idx -> Workspace_index.source_import_hints idx ~path)

let graph_parse_quality_for_path (ws : t) ~(path_key : string)
    ~(doc_opt : Document.t option) : parse_quality =
  match doc_opt with
  | Some doc when doc.Document.parse_rev = doc.Document.rev -> ParseQualityFull
  | _ ->
      if
        Hashtbl.mem ws.quick_nav_done_set path_key
        || Hashtbl.mem ws.bg_parsed path_key
      then ParseQualitySkeleton
      else ParseQualityNone

let graph_file_class_for_path (ws : t) ~(path : string)
    ~(doc_opt : Document.t option) : file_class =
  match doc_opt with
  | Some _ -> FileClassOpen
  | None ->
      if graph_entry_hint_for_path ws ~path then FileClassEntry
      else FileClassNormal

let graph_queue_empty (ws : t) : bool =
  Queue.is_empty ws.bg_root_small_queue && Queue.is_empty ws.bg_root_large_queue

let graph_closure_seed_complete (ws : t) : bool =
  ws.graph_root_closure_cursor >= Array.length ws.graph_root_closure_paths

let graph_path_of_key (ws : t) (path_key : string) : string option =
  match Hashtbl.find_opt ws.graph_nodes path_key with
  | Some node -> Some node.gn_path
  | None -> None

let graph_neighbors_for_key (ws : t) ~(closure : (string, bool) Hashtbl.t)
    (path_key : string) : string list =
  match Hashtbl.find_opt ws.graph_nodes path_key with
  | None -> []
  | Some node ->
      let out = ref [] in
      let seen = Hashtbl.create 16 in
      let push_key (k : string) =
        if k <> "" && Hashtbl.mem closure k && not (Hashtbl.mem seen k) then (
          Hashtbl.replace seen k true;
          out := k :: !out)
      in
      List.iter (fun p -> push_key (normalize_path_key p)) node.gn_import_paths;
      List.iter push_key node.gn_rev_importers;
      List.rev !out

let graph_refresh (ws : t) : unit =
  if not ws.graph_needs_refresh then ()
  else (
    ws.graph_epoch <- ws.graph_epoch + 1;
    Hashtbl.clear ws.graph_nodes;
    Hashtbl.clear ws.graph_root_reason;
    Hashtbl.clear ws.graph_root_closure_set;
    ws.graph_root_closure_paths <- [||];
    ws.graph_root_closure_cursor <- 0;
    ws.graph_scc_count <- 0;

    let path_seen = Hashtbl.create 8192 in
    let add_path (acc : string list ref) (path : string) =
      let key = normalize_path_key path in
      if key <> "" && not (Hashtbl.mem path_seen key) then (
        Hashtbl.replace path_seen key true;
        acc := path :: !acc)
    in
    let candidates = ref [] in
    (match ws.index with
    | None -> ()
    | Some idx ->
        Workspace_index.all_source_paths idx |> List.iter (add_path candidates));
    Hashtbl.iter
      (fun _uri doc ->
        match doc.Document.file with
        | Some path -> add_path candidates path
        | None -> ())
      ws.docs;
    ws.root_manual_files
    |> List.iter (fun raw ->
        let path = resolve_manual_root_file ws raw in
        if path <> "" then add_path candidates path);
    let paths = List.rev !candidates in

    List.iter
      (fun path ->
        let path_key = normalize_path_key path in
        if path_key <> "" then
          let doc_opt =
            match find_open_doc_for_path ws ~path with
            | Some d -> Some d
            | None -> Hashtbl.find_opt ws.files path_key
          in
          let import_compools = graph_import_hints_for_path ws ~path ~doc_opt in
          let import_paths =
            import_compools
            |> List.filter_map (fun compool ->
                match ws.index with
                | Some idx -> Workspace_index.find_compool idx ~name:compool
                | None -> None)
            |> List.sort_uniq String.compare
          in
          let node =
            {
              gn_path = path;
              gn_path_key = path_key;
              gn_import_compools = import_compools;
              gn_import_paths = import_paths;
              gn_rev_importers = [];
              gn_file_class = graph_file_class_for_path ws ~path ~doc_opt;
              gn_size_class = size_class_of_path ws path;
              gn_parse_quality =
                graph_parse_quality_for_path ws ~path_key ~doc_opt;
              gn_epoch = ws.graph_epoch;
            }
          in
          Hashtbl.replace ws.graph_nodes path_key node)
      paths;

    let reverse_sets : (string, (string, bool) Hashtbl.t) Hashtbl.t =
      Hashtbl.create 1024
    in
    Hashtbl.iter
      (fun importer_key node ->
        node.gn_import_paths
        |> List.iter (fun provider_path ->
            let provider_key = normalize_path_key provider_path in
            if provider_key <> "" then
              let set =
                match Hashtbl.find_opt reverse_sets provider_key with
                | Some s -> s
                | None ->
                    let s = Hashtbl.create 8 in
                    Hashtbl.replace reverse_sets provider_key s;
                    s
              in
              Hashtbl.replace set importer_key true))
      ws.graph_nodes;
    Hashtbl.iter
      (fun path_key node ->
        let revs =
          match Hashtbl.find_opt reverse_sets path_key with
          | None -> []
          | Some set -> Hashtbl.fold (fun k _ acc -> k :: acc) set []
        in
        node.gn_rev_importers <- revs)
      ws.graph_nodes;

    let open_roots =
      Hashtbl.fold
        (fun path_key node acc ->
          if node.gn_file_class = FileClassOpen then path_key :: acc else acc)
        ws.graph_nodes []
    in
    let entry_roots =
      Hashtbl.fold
        (fun path_key node acc ->
          if node.gn_file_class = FileClassEntry then path_key :: acc else acc)
        ws.graph_nodes []
    in
    let heuristic_roots =
      Hashtbl.fold
        (fun path_key node acc ->
          if is_main_boot_heuristic node.gn_path then path_key :: acc else acc)
        ws.graph_nodes []
    in
    let manual_roots =
      ws.root_manual_files
      |> List.filter_map (fun raw ->
          let path = resolve_manual_root_file ws raw in
          let key = normalize_path_key path in
          if key <> "" && Hashtbl.mem ws.graph_nodes key then Some key else None)
    in

    let roots =
      match ws.root_model with
      | RootModelManual ->
          if manual_roots <> [] then open_roots @ manual_roots
          else open_roots @ entry_roots @ heuristic_roots
      | RootModelHeuristic -> open_roots @ heuristic_roots
      | RootModelAuto ->
          if entry_roots <> [] then open_roots @ entry_roots
          else if ws.root_heuristic_fallback then open_roots @ heuristic_roots
          else open_roots
    in
    let roots =
      roots
      |> List.sort_uniq String.compare
      |> List.sort (fun a b ->
          match
            ( Hashtbl.find_opt ws.graph_nodes a,
              Hashtbl.find_opt ws.graph_nodes b )
          with
          | Some na, Some nb ->
              compare
                (file_class_rank na.gn_file_class)
                (file_class_rank nb.gn_file_class)
          | _ -> String.compare a b)
    in
    List.iter
      (fun root_key ->
        let reason =
          if List.mem root_key open_roots then "open"
          else if List.mem root_key entry_roots then "entry"
          else if List.mem root_key manual_roots then "manual"
          else if List.mem root_key heuristic_roots then "heuristic"
          else "fallback"
        in
        Hashtbl.replace ws.graph_root_reason root_key reason)
      roots;

    let closure = Hashtbl.create (max 256 (ws.root_closure_target_files * 2)) in
    let q : (string * int) Queue.t = Queue.create () in
    roots |> List.iter (fun key -> Queue.add (key, 0) q);
    while
      (not (Queue.is_empty q))
      && Hashtbl.length closure < ws.root_closure_target_files
    do
      let key, depth = Queue.pop q in
      if key <> "" && not (Hashtbl.mem closure key) then (
        Hashtbl.replace closure key true;
        if depth < ws.root_closure_max_depth then
          match Hashtbl.find_opt ws.graph_nodes key with
          | None -> ()
          | Some node ->
              node.gn_import_paths
              |> List.iter (fun p ->
                  let k = normalize_path_key p in
                  if k <> "" && Hashtbl.mem ws.graph_nodes k then
                    Queue.add (k, depth + 1) q);
              node.gn_rev_importers
              |> List.iter (fun k ->
                  if k <> "" && Hashtbl.mem ws.graph_nodes k then
                    Queue.add (k, depth + 1) q))
    done;
    Hashtbl.iter
      (fun key _ -> Hashtbl.replace ws.graph_root_closure_set key true)
      closure;

    let closure_paths =
      Hashtbl.fold
        (fun key _ acc ->
          match graph_path_of_key ws key with
          | Some path -> path :: acc
          | None -> acc)
        closure []
      |> List.sort String.compare |> Array.of_list
    in
    ws.graph_root_closure_paths <- closure_paths;
    ws.graph_root_closure_cursor <- 0;

    let index_tbl : (string, int) Hashtbl.t = Hashtbl.create 1024 in
    let low_tbl : (string, int) Hashtbl.t = Hashtbl.create 1024 in
    let on_stack : (string, bool) Hashtbl.t = Hashtbl.create 1024 in
    let stack : string Stack.t = Stack.create () in
    let next_index = ref 0 in
    let scc_count = ref 0 in
    let rec strongconnect (v : string) : unit =
      let idx = !next_index in
      incr next_index;
      Hashtbl.replace index_tbl v idx;
      Hashtbl.replace low_tbl v idx;
      Stack.push v stack;
      Hashtbl.replace on_stack v true;
      graph_neighbors_for_key ws ~closure v
      |> List.iter (fun w ->
          if not (Hashtbl.mem index_tbl w) then (
            strongconnect w;
            let low_v =
              Option.value (Hashtbl.find_opt low_tbl v) ~default:idx
            in
            let low_w =
              Option.value (Hashtbl.find_opt low_tbl w) ~default:idx
            in
            Hashtbl.replace low_tbl v (min low_v low_w))
          else if Hashtbl.mem on_stack w then
            let low_v =
              Option.value (Hashtbl.find_opt low_tbl v) ~default:idx
            in
            let idx_w =
              Option.value (Hashtbl.find_opt index_tbl w) ~default:idx
            in
            Hashtbl.replace low_tbl v (min low_v idx_w));
      let low_v = Option.value (Hashtbl.find_opt low_tbl v) ~default:idx in
      let idx_v = Option.value (Hashtbl.find_opt index_tbl v) ~default:idx in
      if low_v = idx_v then (
        incr scc_count;
        let continue = ref true in
        while !continue && not (Stack.is_empty stack) do
          let w = Stack.pop stack in
          Hashtbl.remove on_stack w;
          if w = v then continue := false
        done)
    in
    Hashtbl.iter
      (fun key _ -> if not (Hashtbl.mem index_tbl key) then strongconnect key)
      closure;
    ws.graph_scc_count <- !scc_count;
    ws.graph_needs_refresh <- false;
    Perf_stats.tick "graph.refreshed";
    Perf_stats.observe_ms "graph.root_candidates"
      (float_of_int (List.length roots));
    Perf_stats.observe_ms "graph.root_closure_size"
      (float_of_int (Array.length ws.graph_root_closure_paths));
    Perf_stats.observe_ms "graph.scc_count" (float_of_int ws.graph_scc_count))

let ensure_graph_fresh (ws : t) : unit =
  if ws.graph_needs_refresh then graph_refresh ws

let maybe_escalate_index_reconcile ?(reason : string = "unknown")
    ?(has_imports_override : bool option) (ws : t) ~(doc : Document.t option) :
    bool =
  let has_imports =
    match (has_imports_override, doc) with
    | Some b, _ -> b
    | None, None -> false
    | None, Some d -> best_effort_doc_imports_for_scheduling d <> []
  in
  match ws.index with
  | None -> false
  | Some idx ->
      let compools = Workspace_index.compool_count idx in
      let sources = Workspace_index.source_count idx in
      if (not has_imports) || compools > 0 || sources <= 0 then false
      else
        let now = Perf_stats.now_ms () in
        if
          now -. ws.index_reconcile_escalate_last_ms
          < float_of_int index_stale_reconcile_min_interval_ms
        then false
        else (
          ws.index_reconcile_escalate_last_ms <- now;
          ws.index_reconcile_escalations <- ws.index_reconcile_escalations + 1;
          if Workspace_index.force_reconcile idx then
            Perf_stats.tick "index.reconcile_started";
          ws.bg_seed_needs_refresh <- true;
          pump_index ws ~max_dirs:index_reconcile_escalate_dirs
            ~max_files:index_reconcile_escalate_files;
          ignore reason;
          true)

let schedule_nav_miss_reconcile (ws : t) ~(doc : Document.t)
    ~(symbol_key : string) : unit =
  let key = normalize_name symbol_key in
  if key = "" then ()
  else (
    Perf_stats.tick "nav.miss_trigger_reconcile";
    let imports = best_effort_doc_imports_for_scheduling doc in
    let profile = workspace_profile_for_budget ws in
    let nav_miss_high_cap =
      match profile with
      | ProfileLarge -> max nav_miss_high_enqueue_cap 64
      | ProfileMedium -> max nav_miss_high_enqueue_cap 36
      | ProfileSmall -> nav_miss_high_enqueue_cap
    in
    let quick_scan_files_budget =
      match profile with
      | ProfileLarge -> max nav_quick_scan_files 128
      | ProfileMedium -> max nav_quick_scan_files 72
      | ProfileSmall -> nav_quick_scan_files
    in
    let quick_scan_total_budget =
      match profile with
      | ProfileLarge -> max nav_quick_scan_total_bytes 4_194_304
      | ProfileMedium -> max nav_quick_scan_total_bytes 2_359_296
      | ProfileSmall -> nav_quick_scan_total_bytes
    in
    let high_budget =
      if ws.startup_diag_hover_ready_ms <> None then max_int
      else max 0 nav_miss_high_cap
    in
    let high_used = ref 0 in
    let scheduled_paths : (string, bool) Hashtbl.t = Hashtbl.create 16 in
    let enqueue_path_once (path : string) : unit =
      let path_key = normalize_path_key path in
      if path_key <> "" && not (Hashtbl.mem scheduled_paths path_key) then (
        Hashtbl.replace scheduled_paths path_key true;
        let use_high =
          if !high_used < high_budget then (
            incr high_used;
            true)
          else false
        in
        let lane = if use_high then LaneOpen else LaneRoot in
        enqueue_bg_path ws ~lane ~reason_group:"nav_miss" ~high:use_high path)
    in
    let enqueue_compool_path (name : string) : unit =
      match ws.index with
      | None -> ()
      | Some idx -> (
          match Workspace_index.find_compool idx ~name with
          | None -> ()
          | Some p -> enqueue_path_once p)
    in
    (match Hashtbl.find_opt ws.quick_nav_index key with
    | None -> ()
    | Some entries ->
        List.iter
          (fun (e : quick_nav_entry) ->
            match Uri_path.file_path_of_uri e.qn_uri with
            | Some p -> enqueue_path_once p
            | None -> ())
          entries);
    imports
    |> List.iter (fun (imp : Preprocess.import) ->
        enqueue_compool_path imp.name);
    (match ws.symbol_hints with
    | None -> ()
    | Some (values, types) ->
        let add_candidates (tbl : (string, string list) Hashtbl.t) =
          match Hashtbl.find_opt tbl key with
          | None -> ()
          | Some compools -> List.iter enqueue_compool_path compools
        in
        add_candidates values;
        add_candidates types);
    (if Hashtbl.length scheduled_paths = 0 then
       let pat = "PROC " ^ key in
       let contains_pat (s : string) : bool =
         let n = String.length s in
         let m = String.length pat in
         let rec loop i =
           if i + m > n then false
           else if String.sub s i m = pat then true
           else loop (i + 1)
         in
         m > 0 && loop 0
       in
       let read_prefix (path : string) ~(max_bytes : int) : string option =
         try
           let ic = open_in_bin path in
           Fun.protect
             ~finally:(fun () -> close_in_noerr ic)
             (fun () ->
               let size = try in_channel_length ic with _ -> max_bytes in
               let n = max 0 (min max_bytes size) in
               Some (really_input_string ic n))
         with _ -> None
       in
       match ws.index with
       | None -> ()
       | Some idx ->
           Workspace_index.source_paths_for_proc_hint idx ~name:key
           |> List.iter enqueue_path_once;
           if Hashtbl.length scheduled_paths = 0 then (
             let rec scan scanned scanned_bytes = function
               | [] -> ()
               | _ when scanned >= quick_scan_files_budget -> ()
               | _ when scanned_bytes >= quick_scan_total_budget -> ()
               | path :: tl -> (
                   match
                     read_prefix path ~max_bytes:nav_quick_scan_per_file_bytes
                   with
                   | None -> scan (scanned + 1) scanned_bytes tl
                   | Some text ->
                       let bytes = String.length text in
                       let next_bytes = scanned_bytes + bytes in
                       if next_bytes > quick_scan_total_budget then ()
                       else
                         let upper = String.uppercase_ascii text in
                         if contains_pat upper then enqueue_path_once path;
                         scan (scanned + 1) next_bytes tl)
             in
             scan 0 0 (Workspace_index.all_source_paths idx);
             if Hashtbl.length scheduled_paths = 0 then (
               ensure_graph_fresh ws;
               let rec enqueue_closure i remaining =
                 if
                   remaining <= 0
                   || i >= Array.length ws.graph_root_closure_paths
                 then ()
                 else (
                   enqueue_path_once ws.graph_root_closure_paths.(i);
                   enqueue_closure (i + 1) (remaining - 1))
               in
               enqueue_closure 0
                 (min 48 (Array.length ws.graph_root_closure_paths)))));
    ignore
      (maybe_escalate_index_reconcile ws ~doc:(Some doc) ~reason:"nav_miss"
         ~has_imports_override:(imports <> [])))

let invalidate_symbol_hints (ws : t) : unit = ws.symbol_hints <- None

let rescan (ws : t) : unit =
  startup_mark_started ws;
  mark_graph_dirty ws;
  invalidate_lsif_snapshot ws;
  ws.parse_epoch <- ws.parse_epoch + 1;
  ws.lsif_snapshot_revision <- 0;
  Hashtbl.clear ws.files;
  Hashtbl.clear ws.nav_response_cache;
  Hashtbl.clear ws.bg_enqueued;
  Hashtbl.clear ws.bg_parsed;
  Hashtbl.clear ws.closed_doc_last_touch;
  Hashtbl.clear ws.bg_closed_diags;
  Hashtbl.clear ws.bg_pending_diag_payloads;
  Hashtbl.clear ws.bg_pending_diag_set;
  Hashtbl.clear ws.open_parse_generation;
  Hashtbl.clear ws.open_provisional_since_ms;
  Hashtbl.clear ws.quick_nav_index;
  Hashtbl.clear ws.quick_nav_pending_set;
  Hashtbl.clear ws.quick_nav_done_set;
  Hashtbl.clear ws.nav_quick_scan_offset_by_path;
  ws.quick_nav_index_done <- 0;
  ws.quick_nav_index_total <- 0;
  Hashtbl.clear ws.parse_worker_inflight;
  while not (Queue.is_empty ws.bg_high_small_queue) do
    ignore (Queue.pop ws.bg_high_small_queue)
  done;
  while not (Queue.is_empty ws.bg_norm_small_queue) do
    ignore (Queue.pop ws.bg_norm_small_queue)
  done;
  while not (Queue.is_empty ws.bg_high_large_queue) do
    ignore (Queue.pop ws.bg_high_large_queue)
  done;
  while not (Queue.is_empty ws.bg_norm_large_queue) do
    ignore (Queue.pop ws.bg_norm_large_queue)
  done;
  while not (Queue.is_empty ws.quick_nav_pending_paths) do
    ignore (Queue.pop ws.quick_nav_pending_paths)
  done;
  while not (Queue.is_empty ws.parse_worker_jobs) do
    ignore (Queue.pop ws.parse_worker_jobs)
  done;
  while not (Queue.is_empty ws.parse_worker_results) do
    ignore (Queue.pop ws.parse_worker_results)
  done;
  while not (Queue.is_empty ws.bg_pending_diag_updates) do
    ignore (Queue.pop ws.bg_pending_diag_updates)
  done;
  ws.bg_seed_paths <- [||];
  ws.bg_seed_cursor <- 0;
  ws.bg_seed_needs_refresh <- true;
  invalidate_symbol_hints ws;
  if ws.sem_store_enabled then Semantic_store.reset ws.semantic_store;
  if ws.lsif_delta_enabled then Lsif_delta.reset ws.lsif_delta_state;
  match ws.root_path with
  | None -> ws.index <- None
  | Some root -> (
      let idx = Workspace_index.start ~root in
      ws.index <- Some idx;
      let max_dirs, max_files =
        startup_scan_budget_for_root ~network:(is_probably_network_path root)
      in
      try ignore (Workspace_index.scan_step idx ~max_dirs ~max_files)
      with _ -> ())

let compool_count (ws : t) : int =
  pump_index_background ws;
  match ws.index with
  | None -> 0
  | Some idx -> Workspace_index.compool_count idx
