module T = Lsp.Types
module Lib = Jovial_lsp_lib

type profile = Small | Medium | Large | Huge

type profile_config = {
  profile : profile;
  name : string;
  compool_count : int;
  main_count : int;
  local_count : int;
  statement_count : int;
}

type bench_case = {
  config : profile_config;
  root : string;
  source_paths : string list;
  main_path : string;
  main_uri : T.DocumentUri.t;
  main_text : string;
  query_pos : T.Position.t;
  semantic_range : T.Range.t;
  source_bytes : int;
}

type latency_stats = {
  name : string;
  samples : float list;
  failures : int;
  first_error : string option;
}

type startup_result = {
  ws : Lib.Workspace.t;
  kind : string;
  skeleton_ready_ms : float option;
  local_ast_ready_ms : float option;
  full_nav_ready_ms : float option;
  ticks : int;
  timed_out : bool;
  readiness : Yojson.Safe.t;
  scheduler : Yojson.Safe.t;
  memory : Yojson.Safe.t;
  perf_stats : Yojson.Safe.t;
}

type profile_result = {
  case : bench_case;
  cold : startup_result;
  warm : startup_result;
  hover : latency_stats;
  definition : latency_stats;
  references : latency_stats;
  semantic_range : latency_stats;
  open_file_diagnostics : latency_stats;
  memory_after_requests : Yojson.Safe.t;
  perf_stats_after_requests : Yojson.Safe.t;
}

let now_ms () = Unix.gettimeofday () *. 1000.0

let utc_timestamp () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let file_timestamp () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let rec ensure_dir path =
  if path = "" || Sys.file_exists path then ()
  else (
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    try Unix.mkdir path 0o755 with _ -> ())

let write_text path text =
  ensure_dir (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
      output_string oc text)

let read_text path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_json path json =
  ensure_dir (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
      Yojson.Safe.pretty_to_channel oc json;
      output_char oc '\n')

let is_dir path = try Sys.is_directory path with _ -> false

let rec remove_tree path =
  if Sys.file_exists path then
    if is_dir path then (
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      try Unix.rmdir path with _ -> ())
    else try Sys.remove path with _ -> ()

let mk_temp_dir prefix =
  let base = Filename.get_temp_dir_name () in
  let rec pick attempts =
    if attempts <= 0 then failwith "failed to create temporary benchmark root";
    let path =
      Filename.concat base
        (Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ())
           (Random.bits () land 0xFFFFFF))
    in
    try
      Unix.mkdir path 0o755;
      path
    with _ -> pick (attempts - 1)
  in
  pick 64

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let default_report_dir () =
  let cwd = Sys.getcwd () in
  let base = Filename.basename cwd in
  let parent = Filename.dirname cwd in
  let parent_base = Filename.basename parent in
  if base = "lsp-server" && parent_base = "apps" then
    Filename.concat (Filename.dirname parent) (Filename.concat "reports" "perf")
  else Filename.concat cwd (Filename.concat "reports" "perf")

let profile_config = function
  | Small ->
      {
        profile = Small;
        name = "small";
        compool_count = 4;
        main_count = 16;
        local_count = 8;
        statement_count = 20;
      }
  | Medium ->
      {
        profile = Medium;
        name = "medium";
        compool_count = 12;
        main_count = 96;
        local_count = 12;
        statement_count = 32;
      }
  | Large ->
      {
        profile = Large;
        name = "large";
        compool_count = 32;
        main_count = 320;
        local_count = 16;
        statement_count = 48;
      }
  | Huge ->
      {
        profile = Huge;
        name = "huge";
        compool_count = 64;
        main_count = 1_000;
        local_count = 20;
        statement_count = 80;
      }

let profile_kind = function
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"
  | Huge -> "huge"

let split_commas s =
  s |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun x -> x <> "")

let profiles_of_arg s =
  let add_one acc raw =
    match String.lowercase_ascii raw with
    | "small" -> Small :: acc
    | "medium" -> Medium :: acc
    | "large" -> Large :: acc
    | "huge" -> Huge :: acc
    | "all" -> Large :: Medium :: Small :: acc
    | "everything" -> Huge :: Large :: Medium :: Small :: acc
    | other -> invalid_arg ("unknown benchmark profile: " ^ other)
  in
  let names = split_commas s in
  let selected = if names = [] then [ "small" ] else names in
  let rank = function Small -> 0 | Medium -> 1 | Large -> 2 | Huge -> 3 in
  selected |> List.fold_left add_one []
  |> List.sort_uniq (fun a b -> compare (rank a) (rank b))

let compool_name i = Printf.sprintf "CP%03d" i
let main_name i = Printf.sprintf "MAIN%03d" i
let local_name main_index local_index = Printf.sprintf "LOCAL%03d_%02d" main_index local_index

let compool_text (cfg : profile_config) i =
  let name = compool_name i in
  let b = Buffer.create 512 in
  Buffer.add_string b "START\n";
  Buffer.add_string b ("COMPOOL " ^ name ^ ";\n");
  Buffer.add_string b "DEF BEGIN\n";
  for item = 0 to 5 do
    Buffer.add_string b
      (Printf.sprintf "  ITEM %s'VALUE%02d U 6;\n" name item)
  done;
  Buffer.add_string b "END\nTERM\n";
  ignore cfg;
  Buffer.contents b

let add_imports b (cfg : profile_config) main_index =
  let import_count = min cfg.compool_count 3 in
  for offset = 0 to import_count - 1 do
    let compool = compool_name ((main_index + offset) mod cfg.compool_count) in
    Buffer.add_string b (Printf.sprintf "!COMPOOL ('%s');\n" compool)
  done

let main_text (cfg : profile_config) i =
  let b = Buffer.create 4096 in
  let proc = main_name i in
  Buffer.add_string b "START\n";
  add_imports b cfg i;
  Buffer.add_string b "DEFINE TEN \"10\";\n";
  Buffer.add_string b (Printf.sprintf "DEF PROC %s RENT;\n" proc);
  Buffer.add_string b "BEGIN\n";
  for local = 0 to cfg.local_count - 1 do
    Buffer.add_string b
      (Printf.sprintf "  ITEM %s U 6;\n" (local_name i local))
  done;
  for stmt = 0 to cfg.statement_count - 1 do
    let lhs = local_name i (stmt mod cfg.local_count) in
    let rhs = local_name i ((stmt + 1) mod cfg.local_count) in
    let compool = compool_name ((i + stmt) mod cfg.compool_count) in
    Buffer.add_string b
      (Printf.sprintf "  %s = %s + %s'VALUE%02d + TEN + %d;\n" lhs rhs
         compool (stmt mod 6) stmt)
  done;
  Buffer.add_string b "END\nTERM\n";
  Buffer.contents b

let manifest_json (cfg : profile_config) =
  `Assoc
    [
      ("schemaVersion", `Int 1);
      ("kind", `String "jovial-lsp-synthetic-benchmark-workspace");
      ("profile", `String cfg.name);
      ("compoolCount", `Int cfg.compool_count);
      ("mainCount", `Int cfg.main_count);
      ("localCount", `Int cfg.local_count);
      ("statementCount", `Int cfg.statement_count);
    ]

let generate_workspace root (cfg : profile_config) =
  ensure_dir root;
  for i = 0 to cfg.compool_count - 1 do
    write_text
      (Filename.concat root (Printf.sprintf "%s.j73" (compool_name i)))
      (compool_text cfg i)
  done;
  for i = 0 to cfg.main_count - 1 do
    write_text
      (Filename.concat root (Printf.sprintf "%s.j73" (main_name i)))
      (main_text cfg i)
  done;
  write_json (Filename.concat root "BENCHMARK-MANIFEST.json")
    (manifest_json cfg)

let source_extension path =
  String.lowercase_ascii (Filename.extension path) = ".j73"

let rec collect_sources root =
  if not (Sys.file_exists root) then []
  else if not (is_dir root) then if source_extension root then [ root ] else []
  else
    Sys.readdir root |> Array.to_list
    |> List.concat_map (fun name -> collect_sources (Filename.concat root name))
    |> List.sort String.compare

let clear_persistent_caches root =
  remove_tree (Filename.concat root ".jovial-lsp-cache");
  remove_tree (Filename.concat root ".jovial-lsp")

let ensure_bench_workspace base_root (cfg : profile_config) =
  let root = Filename.concat base_root cfg.name in
  let manifest = Filename.concat root "BENCHMARK-MANIFEST.json" in
  if not (Sys.file_exists manifest) then generate_workspace root cfg;
  root

let docuri_of_path path =
  match Lib.Uri_path.docuri_of_path path with
  | Some uri -> uri
  | None -> failwith ("failed to build file URI for " ^ path)

let position_of_offset text off =
  let rec loop i line character =
    if i >= off then ({ line; character } : T.Position.t)
    else if text.[i] = '\n' then loop (i + 1) (line + 1) 0
    else loop (i + 1) line (character + 1)
  in
  loop 0 0 0

let find_nth text ~needle ~nth =
  let n = String.length text in
  let m = String.length needle in
  let rec at i j =
    j = m || (i + j < n && text.[i + j] = needle.[j] && at i (j + 1))
  in
  let rec loop i seen =
    if i + m > n then
      failwith
        (Printf.sprintf "missing occurrence %d of %S in generated fixture" nth
           needle)
    else if at i 0 then if seen = nth then i else loop (i + 1) (seen + 1)
    else loop (i + 1) seen
  in
  loop 0 0

let count_bytes paths =
  List.fold_left
    (fun acc path ->
      try acc + Unix.(stat path).st_size with _ -> acc)
    0 paths

let prepare_case base_root profile =
  let config = profile_config profile in
  let root = ensure_bench_workspace base_root config in
  let source_paths = collect_sources root in
  let main_path = Filename.concat root "MAIN000.j73" in
  let main_text = read_text main_path in
  let needle = local_name 0 0 in
  let off = find_nth main_text ~needle ~nth:2 in
  let query_pos = position_of_offset main_text off in
  let range_end_line = min 120 (config.statement_count + config.local_count + 12) in
  let semantic_range =
    {
      T.Range.start = { line = 0; character = 0 };
      end_ = { line = range_end_line; character = 0 };
    }
  in
  {
    config;
    root;
    source_paths;
    main_path;
    main_uri = docuri_of_path main_path;
    main_text;
    query_pos;
    semantic_range;
    source_bytes = count_bytes source_paths;
  }

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let rec json_at json path =
  match path with
  | [] -> Some json
  | key :: rest -> Option.bind (assoc_field key json) (fun j -> json_at j rest)

let bool_at json path =
  match json_at json path with Some (`Bool b) -> b | _ -> false

let option_json_float = function None -> `Null | Some f -> `Float f

let drain_publish_side_effects ws =
  ignore (Lib.Workspace.drain_open_diag_revalidate_uris ws ~max_items:max_int);
  ignore (Lib.Workspace.drain_pending_diag_updates ws ~max_items:max_int)

let note_stage t0 ws uri skeleton_ms local_ast_ms full_nav_ms =
  let readiness = Lib.Workspace.startup_readiness_json_for_report ws in
  let elapsed = max 0.0 (now_ms () -. t0) in
  if !skeleton_ms = None && bool_at readiness [ "components"; "quickNavIndexReady" ]
  then skeleton_ms := Some elapsed;
  if !local_ast_ms = None && Lib.Workspace.open_doc_converged ws ~uri then
    local_ast_ms := Some elapsed;
  if !full_nav_ms = None && Lib.Workspace.startup_is_ready_now ws then
    full_nav_ms := Some elapsed

let run_startup case ~kind ~clear_cache ~timeout_ms ~tick_budget_ms =
  if clear_cache then clear_persistent_caches case.root;
  Gc.compact ();
  Lib.Workspace_foundation.Perf_stats.reset ();
  let t0 = now_ms () in
  let ws = Lib.Workspace.create () in
  Lib.Workspace.set_root_path ws (Some case.root);
  ignore (Lib.Workspace.set_source_files ws case.source_paths);
  ignore (Lib.Workspace.ensure_index_health ws);
  Lib.Workspace.open_doc ~inline_catch_up:false ws ~uri:case.main_uri
    ~file:(Some case.main_path) ~text:case.main_text;
  let skeleton_ms = ref None in
  let local_ast_ms = ref None in
  let full_nav_ms = ref None in
  let ticks = ref 0 in
  let timed_out = ref false in
  let observe () =
    drain_publish_side_effects ws;
    note_stage t0 ws case.main_uri skeleton_ms local_ast_ms full_nav_ms
  in
  observe ();
  while !full_nav_ms = None && now_ms () -. t0 < float_of_int timeout_ms do
    let budget =
      Lib.Workspace.startup_background_budget_ms ws
        ~base_budget_ms:tick_budget_ms
    in
    Lib.Workspace.background_tick ws ~budget_ms:budget ~mode:Lib.Workspace.BgTickIdle
      ~idle_quiet_ms:0 ~last_message_ms:t0;
    incr ticks;
    observe ();
    if !full_nav_ms = None then Thread.delay 0.001
  done;
  if !full_nav_ms = None then timed_out := true;
  {
    ws;
    kind;
    skeleton_ready_ms = !skeleton_ms;
    local_ast_ready_ms = !local_ast_ms;
    full_nav_ready_ms = !full_nav_ms;
    ticks = !ticks;
    timed_out = !timed_out;
    readiness = Lib.Workspace.startup_readiness_json_for_report ws;
    scheduler = Lib.Workspace.debug_scheduler_json ws;
    memory = Lib.Workspace.debug_memory_json ws;
    perf_stats = Lib.Workspace.perf_stats_json ws;
  }

let percentile sorted pct =
  match sorted with
  | [] -> 0.0
  | xs ->
      let n = List.length xs in
      let idx = int_of_float (Float.ceil (pct *. float_of_int n)) - 1 in
      List.nth xs (max 0 (min (n - 1) idx))

let latency_stats name samples failures first_error =
  { name; samples = List.rev samples; failures; first_error }

let measure_latency name samples f =
  for _ = 1 to min 3 samples do
    try f () with _ -> ()
  done;
  let rec loop i acc failures first_error =
    if i >= samples then latency_stats name acc failures first_error
    else
      let t0 = now_ms () in
      let failures, first_error =
        try
          f ();
          (failures, first_error)
        with exn ->
          let msg = Printexc.to_string exn in
          (failures + 1, match first_error with Some _ -> first_error | None -> Some msg)
      in
      let elapsed = max 0.0 (now_ms () -. t0) in
      loop (i + 1) (elapsed :: acc) failures first_error
  in
  loop 0 [] 0 None

let latency_json stats =
  let sorted = List.sort Float.compare stats.samples in
  let count = List.length stats.samples in
  let total = List.fold_left ( +. ) 0.0 stats.samples in
  let mean = if count = 0 then 0.0 else total /. float_of_int count in
  `Assoc
    [
      ("name", `String stats.name);
      ("count", `Int count);
      ("failures", `Int stats.failures);
      ( "firstError",
        match stats.first_error with None -> `Null | Some msg -> `String msg );
      ("minMs", `Float (match sorted with [] -> 0.0 | x :: _ -> x));
      ("meanMs", `Float mean);
      ( "maxMs",
        `Float
          (match List.rev sorted with [] -> 0.0 | x :: _ -> x) );
      ("p50Ms", `Float (percentile sorted 0.50));
      ("p95Ms", `Float (percentile sorted 0.95));
      ("p99Ms", `Float (percentile sorted 0.99));
      ("samplesMs", `List (List.map (fun x -> `Float x) stats.samples));
    ]

let run_request_latencies case ws samples =
  Lib.Workspace_foundation.Perf_stats.reset ();
  let hover =
    measure_latency "hover" samples (fun () ->
        ignore
          (Lib.Workspace.hover_for ws ~uri:case.main_uri ~pos:case.query_pos))
  in
  let definition =
    measure_latency "definition" samples (fun () ->
        ignore
          (Lib.Workspace.definition_locations_for ws ~uri:case.main_uri
             ~pos:case.query_pos))
  in
  let references =
    measure_latency "references" samples (fun () ->
        ignore
          (Lib.Workspace.references_locations_for ws ~uri:case.main_uri
             ~pos:case.query_pos ~include_decl:true))
  in
  let semantic_range =
    measure_latency "semanticTokenRange" samples (fun () ->
        ignore
          (Lib.Workspace.semantic_tokens_range_for ws ~uri:case.main_uri
             ~range:case.semantic_range))
  in
  let open_file_diagnostics =
    measure_latency "openFileDiagnostics" samples (fun () ->
        Lib.Workspace.close_doc ws ~uri:case.main_uri;
        Lib.Workspace.open_doc ~inline_catch_up:true ws ~uri:case.main_uri
          ~file:(Some case.main_path) ~text:case.main_text;
        ignore (Lib.Workspace.finish_open_doc_now_if_needed ws ~uri:case.main_uri);
        ignore (Lib.Workspace.diagnostics_snapshot_for ws ~uri:case.main_uri))
  in
  (hover, definition, references, semantic_range, open_file_diagnostics)

let startup_json result =
  `Assoc
    [
      ("kind", `String result.kind);
      ("skeletonReadyMs", option_json_float result.skeleton_ready_ms);
      ("localAstReadyMs", option_json_float result.local_ast_ready_ms);
      ("fullNavReadyMs", option_json_float result.full_nav_ready_ms);
      ("ticks", `Int result.ticks);
      ("timedOut", `Bool result.timed_out);
      ("readiness", result.readiness);
      ("scheduler", result.scheduler);
      ("memory", result.memory);
      ("perfStats", result.perf_stats);
    ]

let profile_config_json (cfg : profile_config) =
  `Assoc
    [
      ("name", `String cfg.name);
      ("kind", `String (profile_kind cfg.profile));
      ("compoolCount", `Int cfg.compool_count);
      ("mainCount", `Int cfg.main_count);
      ("localCount", `Int cfg.local_count);
      ("statementCount", `Int cfg.statement_count);
    ]

let case_json case =
  `Assoc
    [
      ("profile", profile_config_json case.config);
      ("root", `String case.root);
      ("sourceFiles", `Int (List.length case.source_paths));
      ("sourceBytes", `Int case.source_bytes);
      ("mainPath", `String case.main_path);
      ("mainUri", `String (Lib.Uri_path.docuri_to_string case.main_uri));
      ( "queryPosition",
        `Assoc
          [
            ("line", `Int case.query_pos.line);
            ("character", `Int case.query_pos.character);
          ] );
    ]

let profile_result_json result =
  `Assoc
    [
      ("profile", `String result.case.config.name);
      ("workspace", case_json result.case);
      ("startup", `Assoc [ ("cold", startup_json result.cold); ("warm", startup_json result.warm) ]);
      ( "latencies",
        `Assoc
          [
            ("hover", latency_json result.hover);
            ("definition", latency_json result.definition);
            ("references", latency_json result.references);
            ("semanticTokenRange", latency_json result.semantic_range);
            ( "openFileDiagnostics",
              latency_json result.open_file_diagnostics );
          ] );
      ("memoryAfterRequests", result.memory_after_requests);
      ("perfStatsAfterRequests", result.perf_stats_after_requests);
    ]

let run_profile base_root samples startup_timeout_ms tick_budget_ms profile =
  let case = prepare_case base_root profile in
  Printf.eprintf "bench %s: %d files, %.2f MB\n%!" case.config.name
    (List.length case.source_paths)
    (float_of_int case.source_bytes /. (1024.0 *. 1024.0));
  let cold =
    run_startup case ~kind:"cold" ~clear_cache:true ~timeout_ms:startup_timeout_ms
      ~tick_budget_ms
  in
  let warm =
    run_startup case ~kind:"warm" ~clear_cache:false
      ~timeout_ms:startup_timeout_ms ~tick_budget_ms
  in
  let hover, definition, references, semantic_range, open_file_diagnostics =
    run_request_latencies case warm.ws samples
  in
  {
    case;
    cold;
    warm;
    hover;
    definition;
    references;
    semantic_range;
    open_file_diagnostics;
    memory_after_requests = Lib.Workspace.debug_memory_json warm.ws;
    perf_stats_after_requests = Lib.Workspace.perf_stats_json warm.ws;
  }

let summary_entry result =
  let stat_p95 stats =
    let sorted = List.sort Float.compare stats.samples in
    percentile sorted 0.95
  in
  `Assoc
    [
      ("profile", `String result.case.config.name);
      ("sourceFiles", `Int (List.length result.case.source_paths));
      ("sourceBytes", `Int result.case.source_bytes);
      ("coldSkeletonReadyMs", option_json_float result.cold.skeleton_ready_ms);
      ("coldLocalAstReadyMs", option_json_float result.cold.local_ast_ready_ms);
      ("coldFullNavReadyMs", option_json_float result.cold.full_nav_ready_ms);
      ("warmFullNavReadyMs", option_json_float result.warm.full_nav_ready_ms);
      ("hoverP95Ms", `Float (stat_p95 result.hover));
      ("definitionP95Ms", `Float (stat_p95 result.definition));
      ("referencesP95Ms", `Float (stat_p95 result.references));
      ("semanticRangeP95Ms", `Float (stat_p95 result.semantic_range));
      ("diagnosticsP95Ms", `Float (stat_p95 result.open_file_diagnostics));
    ]

let report_json ~generated_at ~argv ~samples ~startup_timeout_ms ~tick_budget_ms
    ~workspace_base results =
  let runs = List.map profile_result_json results in
  `Assoc
    [
      ("schemaVersion", `Int 1);
      ("tool", `String "jovial-lsp-workspace-bench");
      ("generatedAt", `String generated_at);
      ("ocamlVersion", `String Sys.ocaml_version);
      ("osType", `String Sys.os_type);
      ("wordSize", `Int Sys.word_size);
      ("argv", `List (List.map (fun arg -> `String arg) argv));
      ( "config",
        `Assoc
          [
            ("samples", `Int samples);
            ("startupTimeoutMs", `Int startup_timeout_ms);
            ("tickBudgetMs", `Int tick_budget_ms);
            ("workspaceBase", `String workspace_base);
          ] );
      ("summary", `List (List.map summary_entry results));
      ("runs", `List runs);
    ]

let ms_cell = function None -> "timeout" | Some ms -> Printf.sprintf "%.2f" ms

let p95_cell stats =
  let sorted = List.sort Float.compare stats.samples in
  Printf.sprintf "%.2f" (percentile sorted 0.95)

let markdown_summary ~generated_at ~json_path results =
  let b = Buffer.create 4096 in
  Buffer.add_string b "# JOVIAL LSP Benchmark Summary\n\n";
  Buffer.add_string b (Printf.sprintf "- Generated: `%s`\n" generated_at);
  Buffer.add_string b (Printf.sprintf "- JSON: `%s`\n\n" json_path);
  Buffer.add_string b
    "| Profile | Files | MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Definition p95 ms | References p95 ms | Semantic range p95 ms | Diagnostics p95 ms |\n";
  Buffer.add_string b
    "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |\n";
  List.iter
    (fun result ->
      Buffer.add_string b
        (Printf.sprintf
           "| %s | %d | %.2f | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
           result.case.config.name
           (List.length result.case.source_paths)
           (float_of_int result.case.source_bytes /. (1024.0 *. 1024.0))
           (ms_cell result.cold.skeleton_ready_ms)
           (ms_cell result.cold.local_ast_ready_ms)
           (ms_cell result.cold.full_nav_ready_ms)
           (ms_cell result.warm.full_nav_ready_ms)
           (p95_cell result.hover) (p95_cell result.definition)
           (p95_cell result.references) (p95_cell result.semantic_range)
           (p95_cell result.open_file_diagnostics)))
    results;
  Buffer.contents b

let replace_extension path ext =
  let base =
    try Filename.chop_extension path with Invalid_argument _ -> path
  in
  base ^ ext

let () =
  Random.self_init ();
  let profiles_arg = ref "small" in
  let samples = ref 40 in
  let startup_timeout_ms = ref 30_000 in
  let tick_budget_ms = ref 50 in
  let workspace_root = ref None in
  let out_dir = ref (default_report_dir ()) in
  let output = ref None in
  let set_optional r value = r := Some (absolute_path value) in
  let speclist =
    [
      ( "--profiles",
        Arg.Set_string profiles_arg,
        "Comma-separated profiles: small,medium,large,huge,all,everything" );
      ( "--samples",
        Arg.Set_int samples,
        "Request latency samples per operation (default: 40)" );
      ( "--startup-timeout-ms",
        Arg.Set_int startup_timeout_ms,
        "Startup drain timeout per cold/warm run (default: 30000)" );
      ( "--tick-budget-ms",
        Arg.Set_int tick_budget_ms,
        "Background tick budget while draining startup (default: 50)" );
      ( "--workspace-root",
        Arg.String (set_optional workspace_root),
        "Parent directory for generated/reused synthetic workspaces" );
      ( "--out-dir",
        Arg.String (fun value -> out_dir := absolute_path value),
        "Directory for JSON and markdown reports" );
      ( "--output",
        Arg.String (set_optional output),
        "Exact JSON report path; markdown summary uses the same basename" );
    ]
  in
  let usage =
    "workspace_bench.exe [--profiles small,medium] [--samples 40]"
  in
  Arg.parse speclist
    (fun arg -> invalid_arg ("unexpected positional argument: " ^ arg))
    usage;
  let samples = max 1 !samples in
  let startup_timeout_ms = max 1 !startup_timeout_ms in
  let tick_budget_ms = max 1 !tick_budget_ms in
  let selected_profiles = profiles_of_arg !profiles_arg in
  let workspace_base =
    match !workspace_root with
    | Some path ->
        ensure_dir path;
        path
    | None -> mk_temp_dir "jovial-lsp-bench"
  in
  let generated_at = utc_timestamp () in
  let results =
    List.map
      (run_profile workspace_base samples startup_timeout_ms tick_budget_ms)
      selected_profiles
  in
  let json_path =
    match !output with
    | Some path -> path
    | None ->
        ensure_dir !out_dir;
        Filename.concat !out_dir
          (Printf.sprintf "jovial-lsp-benchmark-%s.json" (file_timestamp ()))
  in
  let json =
    report_json ~generated_at ~argv:(Array.to_list Sys.argv) ~samples
      ~startup_timeout_ms ~tick_budget_ms ~workspace_base results
  in
  write_json json_path json;
  let md_path = replace_extension json_path ".md" in
  write_text md_path (markdown_summary ~generated_at ~json_path results);
  Printf.printf "wrote %s\nwrote %s\n%!" json_path md_path
