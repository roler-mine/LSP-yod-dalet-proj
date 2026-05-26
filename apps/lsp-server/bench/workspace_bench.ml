(* Module overview: Benchmark harness for measuring workspace indexing and request performance. *)

module T = Lsp.Types
module Lib = Jovial_lsp_lib

(* This harness measures the workspace engine directly. It intentionally skips
   VS Code, stdio framing, JSON serialization, and languageclient scheduling so
   reports isolate server-side indexing/query behavior. *)

type profile = Small | Medium | Large | Huge | TargetHuge | MixedStress

type byte_shape = {
  target_files : int;
  average_bytes : int;
  outlier_count : int;
  outlier_bytes : int;
}

type mixed_stress_shape = {
  stress_source_shape : byte_shape;
  stress_noise_files : int;
  stress_noise_bytes : int;
  stress_workspace_bytes : int;
  stress_startup_file_bytes : int option;
  stress_realistic_source : bool;
  stress_active_source_bytes : int option;
}

type source_style = SimplePadded | RealisticDense

type profile_config = {
  profile : profile;
  name : string;
  compool_count : int;
  include_count : int;
  main_count : int;
  local_count : int;
  statement_count : int;
  byte_shape : byte_shape option;
  noise_count : int;
  noise_bytes : int;
  workspace_target_bytes : int option;
  noise_sparse : bool;
  startup_source_bytes : int option;
  source_style : source_style;
  realistic_active_bytes : int option;
  missing_icopy_stride : int;
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
  workspace_bytes : int;
}

type latency_stats = {
  name : string;
  samples : float list;
  failures : int;
  first_error : string option;
}

type startup_load_stats = {
  load_hover : latency_stats;
  load_definition : latency_stats;
  load_references : latency_stats;
  load_semantic_range : latency_stats;
  load_diagnostics : latency_stats;
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
  startup_load : startup_load_stats option;
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

let filler_line =
  "                                                                        \n"

let write_text_padded path text target_bytes =
  ensure_dir (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
      output_string oc text;
      let written = ref (String.length text) in
      let filler_len = String.length filler_line in
      while !written < target_bytes do
        output_string oc filler_line;
        written := !written + filler_len
      done)

let write_text_sparse path text target_bytes =
  ensure_dir (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
      output_string oc text;
      let written = String.length text in
      if target_bytes > written then (
        seek_out oc (target_bytes - 1);
        output_char oc '\n'))

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

let default_target_shape =
  {
    target_files = 520;
    average_bytes = 2 * 1024 * 1024;
    outlier_count = 5;
    outlier_bytes = 50 * 1024 * 1024;
  }

let default_mixed_stress_shape =
  {
    stress_source_shape =
      {
        target_files = 640;
        average_bytes = (3 * 1024 * 1024 * 1024) / 640;
        outlier_count = 8;
        outlier_bytes = 64 * 1024 * 1024;
      };
    stress_noise_files = 80;
    stress_noise_bytes = 2 * 1024 * 1024;
    stress_workspace_bytes = 20 * 1024 * 1024 * 1024;
    stress_startup_file_bytes = Some (768 * 1024);
    stress_realistic_source = false;
    stress_active_source_bytes = Some (32 * 1024);
  }

let profile_config ~(target_shape : byte_shape)
    ~(mixed_stress_shape : mixed_stress_shape) =
  function
  | Small ->
      {
        profile = Small;
        name = "small";
        compool_count = 4;
        include_count = 0;
        main_count = 16;
        local_count = 8;
        statement_count = 20;
        byte_shape = None;
        noise_count = 0;
        noise_bytes = 0;
        workspace_target_bytes = None;
        noise_sparse = false;
        startup_source_bytes = None;
        source_style = SimplePadded;
        realistic_active_bytes = None;
        missing_icopy_stride = 0;
      }
  | Medium ->
      {
        profile = Medium;
        name = "medium";
        compool_count = 12;
        include_count = 0;
        main_count = 96;
        local_count = 12;
        statement_count = 32;
        byte_shape = None;
        noise_count = 0;
        noise_bytes = 0;
        workspace_target_bytes = None;
        noise_sparse = false;
        startup_source_bytes = None;
        source_style = SimplePadded;
        realistic_active_bytes = None;
        missing_icopy_stride = 0;
      }
  | Large ->
      {
        profile = Large;
        name = "large";
        compool_count = 32;
        include_count = 0;
        main_count = 320;
        local_count = 16;
        statement_count = 48;
        byte_shape = None;
        noise_count = 0;
        noise_bytes = 0;
        workspace_target_bytes = None;
        noise_sparse = false;
        startup_source_bytes = None;
        source_style = SimplePadded;
        realistic_active_bytes = None;
        missing_icopy_stride = 0;
      }
  | Huge ->
      {
        profile = Huge;
        name = "huge";
        compool_count = 64;
        include_count = 0;
        main_count = 1_000;
        local_count = 20;
        statement_count = 80;
        byte_shape = None;
        noise_count = 0;
        noise_bytes = 0;
        workspace_target_bytes = None;
        noise_sparse = false;
        startup_source_bytes = None;
        source_style = SimplePadded;
        realistic_active_bytes = None;
        missing_icopy_stride = 0;
      }
  | TargetHuge ->
      let target_files = max 2 target_shape.target_files in
      let compool_count = max 1 (min 64 (target_files / 8)) in
      {
        profile = TargetHuge;
        name = "target-huge";
        compool_count;
        include_count = 0;
        main_count = target_files - compool_count;
        local_count = 20;
        statement_count = 80;
        byte_shape = Some { target_shape with target_files };
        noise_count = 0;
        noise_bytes = 0;
        workspace_target_bytes = None;
        noise_sparse = false;
        startup_source_bytes = None;
        source_style = SimplePadded;
        realistic_active_bytes = None;
        missing_icopy_stride = 0;
      }
  | MixedStress ->
      let shape = mixed_stress_shape.stress_source_shape in
      let target_files = max 3 shape.target_files in
      let compool_count =
        max 1 (min (max 1 (target_files / 7)) (target_files - 2))
      in
      let include_count =
        max 1
          (min (max 1 (target_files / 4))
             (target_files - compool_count - 1))
      in
      {
        profile = MixedStress;
        name =
          (if mixed_stress_shape.stress_realistic_source then
             "mixed-realistic-stress"
           else "mixed-stress");
        compool_count;
        include_count;
        main_count = target_files - compool_count - include_count;
        local_count = 28;
        statement_count = 140;
        byte_shape = Some { shape with target_files };
        noise_count = max 0 mixed_stress_shape.stress_noise_files;
        noise_bytes = max 0 mixed_stress_shape.stress_noise_bytes;
        workspace_target_bytes =
          Some (max 0 mixed_stress_shape.stress_workspace_bytes);
        noise_sparse = true;
        startup_source_bytes = mixed_stress_shape.stress_startup_file_bytes;
        source_style =
          (if mixed_stress_shape.stress_realistic_source then RealisticDense
           else SimplePadded);
        realistic_active_bytes =
          (if mixed_stress_shape.stress_realistic_source then
             mixed_stress_shape.stress_active_source_bytes
           else None);
        missing_icopy_stride = 19;
      }

let profile_kind = function
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"
  | Huge -> "huge"
  | TargetHuge -> "target-huge"
  | MixedStress -> "mixed-stress"

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
    | "target-huge" | "target_huge" | "prod-huge" | "realistic-huge" ->
        TargetHuge :: acc
    | "mixed-stress" | "mixed_stress" | "stress-mixed" | "stress_mixed" ->
        MixedStress :: acc
    | "all" -> Large :: Medium :: Small :: acc
    | "everything" -> Huge :: Large :: Medium :: Small :: acc
    | other -> invalid_arg ("unknown benchmark profile: " ^ other)
  in
  let names = split_commas s in
  let selected = if names = [] then [ "small" ] else names in
  let rank = function
    | Small -> 0
    | Medium -> 1
    | Large -> 2
    | Huge -> 3
    | TargetHuge -> 4
    | MixedStress -> 5
  in
  selected |> List.fold_left add_one []
  |> List.sort_uniq (fun a b -> compare (rank a) (rank b))

let compool_name i = Printf.sprintf "CP%03d" i
let include_name i = Printf.sprintf "INC%03d" i
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

let include_text (cfg : profile_config) i =
  let name = include_name i in
  let b = Buffer.create 512 in
  Buffer.add_string b "START\n";
  Buffer.add_string b
    (Printf.sprintf "ITEM %s'VALUE U 6;\nITEM %s'FLAG U 1;\n" name name);
  Buffer.add_string b "TERM\n";
  ignore cfg;
  Buffer.contents b

let add_imports b (cfg : profile_config) main_index =
  let import_count = min cfg.compool_count 3 in
  for offset = 0 to import_count - 1 do
    let compool = compool_name ((main_index + offset) mod cfg.compool_count) in
    Buffer.add_string b (Printf.sprintf "!COMPOOL ('%s');\n" compool)
  done

let add_icopies b (cfg : profile_config) main_index =
  if cfg.include_count > 0 then (
    let include_count = min cfg.include_count 4 in
    for offset = 0 to include_count - 1 do
      let name = include_name ((main_index + offset) mod cfg.include_count) in
      Buffer.add_string b (Printf.sprintf "ICOPY ('%s.j73');\n" name)
    done;
    if
      cfg.missing_icopy_stride > 0 && main_index > 0
      && main_index mod cfg.missing_icopy_stride = 0
    then
      Buffer.add_string b
        (Printf.sprintf "ICOPY ('MISSING%03d.j73');\n" main_index))

let realistic_ballast_chunk =
  "%" ^ String.make 4090 'R' ^ "%\n"

let add_ballast_until_close b ~target_bytes ~closing =
  let closing_len = String.length closing in
  let chunk_len = String.length realistic_ballast_chunk in
  while Buffer.length b + closing_len + chunk_len <= target_bytes do
    Buffer.add_string b realistic_ballast_chunk
  done;
  let remaining = target_bytes - Buffer.length b - closing_len in
  if remaining >= 3 then (
    Buffer.add_char b '%';
    Buffer.add_string b (String.make (remaining - 3) 'R');
    Buffer.add_string b "%\n");
  Buffer.add_string b closing

let active_source_target cfg ~target_bytes =
  match cfg.realistic_active_bytes with
  | Some n when n > 0 -> min target_bytes (max 4096 n)
  | _ -> target_bytes

let add_realistic_until_close b cfg ~target_bytes ~closing line_for =
  let active_target = active_source_target cfg ~target_bytes in
  let i = ref 0 in
  while Buffer.length b + String.length closing < active_target do
    Buffer.add_string b (line_for !i);
    incr i
  done;
  add_ballast_until_close b ~target_bytes ~closing

let realistic_compool_text (cfg : profile_config) i ~target_bytes =
  let name = compool_name i in
  let b = Buffer.create (min target_bytes (1024 * 1024)) in
  Buffer.add_string b "START\n";
  Buffer.add_string b ("COMPOOL " ^ name ^ ";\n");
  Buffer.add_string b (Printf.sprintf "DEFINE %s_LIMIT \"1024\";\n" name);
  Buffer.add_string b (Printf.sprintf "TYPE %s_COUNT U 6;\n" name);
  Buffer.add_string b "DEF BEGIN\n";
  add_realistic_until_close b cfg ~target_bytes ~closing:"END\nTERM\n" (fun n ->
      match n mod 6 with
      | 0 -> Printf.sprintf "  ITEM %s'VALUE%04d U 6;\n" name n
      | 1 -> Printf.sprintf "  ITEM %s'FLAG%04d U 1;\n" name n
      | 2 -> Printf.sprintf "  TABLE %s'TAB%04d (4) U 6;\n" name n
      | 3 ->
          Printf.sprintf "  ITEM %s'ACCUM%04d %s_COUNT;\n" name n name
      | 4 -> Printf.sprintf "  REF PROC %s'HELPER%04d RENT;\n" name n
      | _ -> Printf.sprintf "  TYPE %s'TYPE%04d U 10;\n" name n);
  ignore cfg;
  Buffer.contents b

let realistic_include_text (cfg : profile_config) i ~target_bytes =
  let name = include_name i in
  let b = Buffer.create (min target_bytes (512 * 1024)) in
  Buffer.add_string b "START\n";
  Buffer.add_string b (Printf.sprintf "DEFINE %s_BASE \"1\";\n" name);
  add_realistic_until_close b cfg ~target_bytes ~closing:"TERM\n" (fun n ->
      match n mod 5 with
      | 0 -> Printf.sprintf "ITEM %s'VALUE%04d U 6;\n" name n
      | 1 -> Printf.sprintf "ITEM %s'FLAG%04d U 1;\n" name n
      | 2 -> Printf.sprintf "TABLE %s'BUFFER%04d (8) U 6;\n" name n
      | 3 -> Printf.sprintf "TYPE %s'TYPE%04d U 10;\n" name n
      | _ -> Printf.sprintf "REF ITEM %s'EXTERNAL%04d U 6;\n" name n);
  ignore cfg;
  Buffer.contents b

let main_text (cfg : profile_config) i =
  let b = Buffer.create 4096 in
  let proc = main_name i in
  Buffer.add_string b "START\n";
  add_icopies b cfg i;
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

let realistic_main_text (cfg : profile_config) i ~target_bytes =
  let b = Buffer.create (min target_bytes (1024 * 1024)) in
  let proc = main_name i in
  Buffer.add_string b "START\n";
  add_icopies b cfg i;
  add_imports b cfg i;
  Buffer.add_string b "DEFINE TEN \"10\";\n";
  Buffer.add_string b "DEFINE SCALE(A,B) \"$A+$B\";\n";
  Buffer.add_string b (Printf.sprintf "DEF PROC %s RENT;\n" proc);
  Buffer.add_string b "BEGIN\n";
  for local = 0 to cfg.local_count - 1 do
    Buffer.add_string b
      (Printf.sprintf "  ITEM %s U 6;\n" (local_name i local))
  done;
  for table = 0 to 5 do
    Buffer.add_string b
      (Printf.sprintf "  TABLE %s_TAB%02d (8) U 6;\n" proc table);
    Buffer.add_string b (Printf.sprintf "  BLOCK %s_BLK%02d;\n" proc table);
    Buffer.add_string b "  BEGIN\n";
    Buffer.add_string b (Printf.sprintf "    ITEM %s_FIELD%02d U 6;\n" proc table);
    Buffer.add_string b (Printf.sprintf "    ITEM %s_FLAG%02d U 1;\n" proc table);
    Buffer.add_string b "  END\n"
  done;
  add_realistic_until_close b cfg ~target_bytes ~closing:"END\nTERM\n" (fun stmt ->
      let lhs = local_name i (stmt mod cfg.local_count) in
      let rhs = local_name i ((stmt + 1) mod cfg.local_count) in
      let compool = compool_name ((i + stmt) mod cfg.compool_count) in
      match stmt mod 24 with
      | 0 ->
          Printf.sprintf "  %s = %s + %s'VALUE%02d + TEN + %d;\n" lhs rhs
            compool (stmt mod 6) stmt
      | 1 ->
          Printf.sprintf "  %s = SCALE(%s,%s'VALUE%02d) + %d;\n" lhs rhs
            compool (stmt mod 6) stmt
      | 2 | 10 | 18 ->
          Printf.sprintf "  %s_TAB%02d(%d) = %s + %d;\n" proc (stmt mod 6)
            ((stmt mod 8) + 1) rhs stmt
      | 3 | 11 | 19 ->
          Printf.sprintf "  %s = %s + %s'FLAG%02d;\n" lhs rhs compool
            (stmt mod 6)
      | 4 | 12 | 20 -> Printf.sprintf "  %s = %s + TEN;\n" lhs rhs
      | 5 | 13 | 21 ->
          Printf.sprintf "  %s = %s + %s'VALUE%02d;\n" lhs lhs compool
            (stmt mod 6)
      | 6 | 14 | 22 -> Printf.sprintf "  %s = %d + TEN;\n" lhs stmt
      | _ -> Printf.sprintf "  %s = %s + %d;\n" lhs rhs stmt);
  Buffer.contents b

let source_count cfg = cfg.compool_count + cfg.include_count + cfg.main_count

let startup_ordinal cfg = cfg.compool_count + cfg.include_count

let target_bytes_for_source (cfg : profile_config) ~ordinal ~base_len =
  match cfg.byte_shape with
  | None -> None
  | Some shape ->
      let total_files = max 1 (source_count cfg) in
      let outliers = max 0 (min shape.outlier_count total_files) in
      let startup_bytes =
        match cfg.startup_source_bytes with
        | Some n when ordinal = startup_ordinal cfg -> Some (max base_len n)
        | _ -> None
      in
      let startup_override_count =
        match cfg.startup_source_bytes with
        | Some _ when startup_ordinal cfg < total_files - outliers -> 1
        | _ -> 0
      in
      let regular_count = max 1 (total_files - outliers - startup_override_count) in
      let desired_total = total_files * max 1 shape.average_bytes in
      let outlier_total = outliers * max 1 shape.outlier_bytes in
      let startup_total =
        match cfg.startup_source_bytes with
        | Some n when startup_override_count = 1 -> max base_len n
        | _ -> 0
      in
      let regular_target =
        max base_len ((desired_total - outlier_total - startup_total) / regular_count)
      in
      let target =
        match startup_bytes with
        | Some n -> n
        | None ->
            if outliers > 0 && ordinal >= total_files - outliers then
              max base_len shape.outlier_bytes
            else regular_target
      in
      Some (max base_len target)

let write_source_text cfg ~ordinal path text =
  match target_bytes_for_source cfg ~ordinal ~base_len:(String.length text) with
  | None -> write_text path text
  | Some target_bytes -> write_text_padded path text target_bytes

let write_generated_source cfg ~ordinal path simple_text realistic_text =
  match cfg.source_style with
  | SimplePadded -> write_source_text cfg ~ordinal path (simple_text ())
  | RealisticDense ->
      let base = simple_text () in
      let target_bytes =
        match target_bytes_for_source cfg ~ordinal ~base_len:(String.length base) with
        | None -> String.length base
        | Some n -> n
      in
      write_text path (realistic_text ~target_bytes)

let byte_shape_json = function
  | None -> `Null
  | Some shape ->
      `Assoc
        [
          ("targetFiles", `Int shape.target_files);
          ("averageBytes", `Int shape.average_bytes);
          ("outlierCount", `Int shape.outlier_count);
          ("outlierBytes", `Int shape.outlier_bytes);
        ]

let manifest_json (cfg : profile_config) =
  `Assoc
    [
      ("schemaVersion", `Int 1);
      ("kind", `String "jovial-lsp-synthetic-benchmark-workspace");
      ("profile", `String cfg.name);
      ("compoolCount", `Int cfg.compool_count);
      ("includeCount", `Int cfg.include_count);
      ("mainCount", `Int cfg.main_count);
      ("localCount", `Int cfg.local_count);
      ("statementCount", `Int cfg.statement_count);
      ("byteShape", byte_shape_json cfg.byte_shape);
      ("noiseCount", `Int cfg.noise_count);
      ("noiseBytes", `Int cfg.noise_bytes);
      ( "workspaceTargetBytes",
        match cfg.workspace_target_bytes with None -> `Null | Some n -> `Int n
      );
      ("noiseSparse", `Bool cfg.noise_sparse);
      ( "startupSourceBytes",
        match cfg.startup_source_bytes with None -> `Null | Some n -> `Int n
      );
      ( "sourceStyle",
        `String
          (match cfg.source_style with
          | SimplePadded -> "simple-padded"
          | RealisticDense -> "realistic-dense") );
      ( "realisticActiveBytes",
        match cfg.realistic_active_bytes with
        | None -> `Null
        | Some n -> `Int n );
      ("missingIcopyStride", `Int cfg.missing_icopy_stride);
    ]

let noise_extension i =
  match i mod 5 with
  | 0 -> ".md"
  | 1 -> ".txt"
  | 2 -> ".json"
  | 3 -> ".c"
  | _ -> ".log"

let noise_text i =
  Printf.sprintf
    "# Mixed workspace ballast %03d\n\
     This file is intentionally not a JOVIAL source file. It exercises \
     mixed-repository filesystem shape without being passed to the LSP as a \
     source unit.\n"
    i

let desired_source_bytes (cfg : profile_config) : int =
  match cfg.byte_shape with
  | None -> 0
  | Some shape -> source_count cfg * max 1 shape.average_bytes

let desired_noise_bytes (cfg : profile_config) : int =
  match cfg.workspace_target_bytes with
  | Some target -> max 0 (target - desired_source_bytes cfg)
  | None -> cfg.noise_count * max 0 cfg.noise_bytes

let generate_noise root (cfg : profile_config) =
  let total_noise = desired_noise_bytes cfg in
  if cfg.noise_count > 0 && total_noise > 0 then
    for i = 0 to cfg.noise_count - 1 do
      let dir =
        Filename.concat
          (Filename.concat root "mixed-noise")
          (Printf.sprintf "group-%02d" (i mod 8))
      in
      let path =
        Filename.concat dir
          (Printf.sprintf "ballast-%03d%s" i (noise_extension i))
      in
      let base = total_noise / cfg.noise_count in
      let extra = if i < total_noise mod cfg.noise_count then 1 else 0 in
      let target_bytes = max 1 (base + extra) in
      if cfg.noise_sparse then write_text_sparse path (noise_text i) target_bytes
      else write_text_padded path (noise_text i) target_bytes
    done

let generate_workspace root (cfg : profile_config) =
  ensure_dir root;
  for i = 0 to cfg.compool_count - 1 do
    let path = Filename.concat root (Printf.sprintf "%s.j73" (compool_name i)) in
    write_generated_source cfg ~ordinal:i path
      (fun () -> compool_text cfg i)
      (fun ~target_bytes -> realistic_compool_text cfg i ~target_bytes)
  done;
  for i = 0 to cfg.include_count - 1 do
    let ordinal = cfg.compool_count + i in
    let path = Filename.concat root (Printf.sprintf "%s.j73" (include_name i)) in
    write_generated_source cfg ~ordinal path
      (fun () -> include_text cfg i)
      (fun ~target_bytes -> realistic_include_text cfg i ~target_bytes)
  done;
  for i = 0 to cfg.main_count - 1 do
    let ordinal = cfg.compool_count + cfg.include_count + i in
    let path = Filename.concat root (Printf.sprintf "%s.j73" (main_name i)) in
    write_generated_source cfg ~ordinal path
      (fun () -> main_text cfg i)
      (fun ~target_bytes -> realistic_main_text cfg i ~target_bytes)
  done;
  generate_noise root cfg;
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
  remove_tree (Lib.Workspace_storage_layout.workspace_dir ~root);
  remove_tree (Filename.concat root ".jovial-lsp-cache");
  remove_tree (Filename.concat root ".jovial-lsp")

let manifest_matches manifest expected =
  try Yojson.Safe.from_file manifest = expected with _ -> false

let ensure_bench_workspace base_root (cfg : profile_config) =
  let root = Filename.concat base_root cfg.name in
  let manifest = Filename.concat root "BENCHMARK-MANIFEST.json" in
  let expected = manifest_json cfg in
  if not (manifest_matches manifest expected) then (
    remove_tree root;
    generate_workspace root cfg);
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

let rec count_tree_bytes path =
  if not (Sys.file_exists path) then 0
  else if is_dir path then
    Sys.readdir path |> Array.to_list
    |> List.fold_left
         (fun acc name -> acc + count_tree_bytes (Filename.concat path name))
         0
  else try Unix.(stat path).st_size with _ -> 0

let prepare_case base_root target_shape mixed_stress_shape profile =
  let config = profile_config ~target_shape ~mixed_stress_shape profile in
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
    workspace_bytes = count_tree_bytes root;
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

let make_latency_stats name samples failures first_error =
  { name; samples = List.rev samples; failures; first_error }

type startup_load_bucket = {
  bucket_name : string;
  mutable bucket_samples : float list;
  mutable bucket_failures : int;
  mutable bucket_first_error : string option;
}

type startup_load_acc = {
  mutable load_next : int;
  hover_bucket : startup_load_bucket;
  definition_bucket : startup_load_bucket;
  references_bucket : startup_load_bucket;
  semantic_bucket : startup_load_bucket;
  diagnostics_bucket : startup_load_bucket;
}

let make_bucket name =
  {
    bucket_name = name;
    bucket_samples = [];
    bucket_failures = 0;
    bucket_first_error = None;
  }

let make_startup_load_acc () =
  {
    load_next = 0;
    hover_bucket = make_bucket "startupHover";
    definition_bucket = make_bucket "startupDefinition";
    references_bucket = make_bucket "startupReferences";
    semantic_bucket = make_bucket "startupSemanticTokenRange";
    diagnostics_bucket = make_bucket "startupOpenFileDiagnostics";
  }

let record_bucket bucket f =
  let t0 = now_ms () in
  let failed, first_error =
    try
      f ();
      (false, bucket.bucket_first_error)
    with exn ->
      (true,
       match bucket.bucket_first_error with
       | Some _ as existing -> existing
       | None -> Some (Printexc.to_string exn))
  in
  let elapsed = max 0.0 (now_ms () -. t0) in
  bucket.bucket_samples <- elapsed :: bucket.bucket_samples;
  if failed then bucket.bucket_failures <- bucket.bucket_failures + 1;
  bucket.bucket_first_error <- first_error

let bucket_latency bucket =
  make_latency_stats bucket.bucket_name bucket.bucket_samples
    bucket.bucket_failures bucket.bucket_first_error

let startup_load_stats_of_acc acc =
  {
    load_hover = bucket_latency acc.hover_bucket;
    load_definition = bucket_latency acc.definition_bucket;
    load_references = bucket_latency acc.references_bucket;
    load_semantic_range = bucket_latency acc.semantic_bucket;
    load_diagnostics = bucket_latency acc.diagnostics_bucket;
  }

let run_startup_request_probe case ws acc =
  let slot = acc.load_next mod 5 in
  acc.load_next <- acc.load_next + 1;
  match slot with
  | 0 ->
      record_bucket acc.diagnostics_bucket (fun () ->
          ignore
            (Lib.Workspace.finish_open_doc_now_if_needed ws
               ~uri:case.main_uri);
          ignore (Lib.Workspace.diagnostics_snapshot_for ws ~uri:case.main_uri))
  | 1 ->
      record_bucket acc.hover_bucket (fun () ->
          ignore
            (Lib.Workspace.hover_for ws ~uri:case.main_uri ~pos:case.query_pos))
  | 2 ->
      record_bucket acc.semantic_bucket (fun () ->
          ignore
            (Lib.Workspace.semantic_tokens_range_for ws ~uri:case.main_uri
               ~range:case.semantic_range))
  | 3 ->
      record_bucket acc.definition_bucket (fun () ->
          ignore
            (Lib.Workspace.definition_locations_for ws ~uri:case.main_uri
               ~pos:case.query_pos))
  | _ ->
      record_bucket acc.references_bucket (fun () ->
          ignore
            (Lib.Workspace.references_locations_for ws ~uri:case.main_uri
               ~pos:case.query_pos ~include_decl:true))

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

let run_startup case ~kind ~clear_cache ~timeout_ms ~tick_budget_ms
    ~request_load =
  if clear_cache then clear_persistent_caches case.root;
  Gc.compact ();
  Lib.Workspace_foundation.Perf_stats.reset ();
  let t0 = now_ms () in
  let ws = Lib.Workspace.create () in
  Lib.Workspace.set_root_path ws (Some case.root);
  ignore (Lib.Workspace.set_source_files ws case.source_paths);
  Lib.Workspace.open_doc ~inline_catch_up:false ws ~uri:case.main_uri
    ~file:(Some case.main_path) ~text:case.main_text;
  let skeleton_ms = ref None in
  let local_ast_ms = ref None in
  let full_nav_ms = ref None in
  let ticks = ref 0 in
  let timed_out = ref false in
  let startup_load_acc =
    if request_load then Some (make_startup_load_acc ()) else None
  in
  let observe () =
    drain_publish_side_effects ws;
    note_stage t0 ws case.main_uri skeleton_ms local_ast_ms full_nav_ms
  in
  observe ();
  (match startup_load_acc with
  | None -> ()
  | Some acc ->
      run_startup_request_probe case ws acc;
      observe ());
  while !full_nav_ms = None && now_ms () -. t0 < float_of_int timeout_ms do
    let budget =
      Lib.Workspace.startup_background_budget_ms ws
        ~base_budget_ms:tick_budget_ms
    in
    Lib.Workspace.background_tick ws ~budget_ms:budget ~mode:Lib.Workspace.BgTickIdle
      ~idle_quiet_ms:0 ~last_message_ms:t0;
    incr ticks;
    observe ();
    (match startup_load_acc with
    | None -> ()
    | Some acc ->
        if !full_nav_ms = None then (
          run_startup_request_probe case ws acc;
          observe ()));
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
    startup_load = Option.map startup_load_stats_of_acc startup_load_acc;
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

let empty_latency_stats name = { name; samples = []; failures = 0; first_error = None }

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

let startup_load_json = function
  | None -> `Null
  | Some stats ->
      `Assoc
        [
          ("hover", latency_json stats.load_hover);
          ("definition", latency_json stats.load_definition);
          ("references", latency_json stats.load_references);
          ("semanticTokenRange", latency_json stats.load_semantic_range);
          ("openFileDiagnostics", latency_json stats.load_diagnostics);
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
      ("startupLoad", startup_load_json result.startup_load);
    ]

let profile_config_json (cfg : profile_config) =
  `Assoc
    [
      ("name", `String cfg.name);
      ("kind", `String (profile_kind cfg.profile));
      ("compoolCount", `Int cfg.compool_count);
      ("includeCount", `Int cfg.include_count);
      ("mainCount", `Int cfg.main_count);
      ("localCount", `Int cfg.local_count);
      ("statementCount", `Int cfg.statement_count);
      ("byteShape", byte_shape_json cfg.byte_shape);
      ("noiseCount", `Int cfg.noise_count);
      ("noiseBytes", `Int cfg.noise_bytes);
      ( "workspaceTargetBytes",
        match cfg.workspace_target_bytes with None -> `Null | Some n -> `Int n
      );
      ("noiseSparse", `Bool cfg.noise_sparse);
      ( "startupSourceBytes",
        match cfg.startup_source_bytes with None -> `Null | Some n -> `Int n
      );
      ( "sourceStyle",
        `String
          (match cfg.source_style with
          | SimplePadded -> "simple-padded"
          | RealisticDense -> "realistic-dense") );
      ("missingIcopyStride", `Int cfg.missing_icopy_stride);
    ]

let case_json case =
  `Assoc
    [
      ("profile", profile_config_json case.config);
      ("root", `String case.root);
      ("sourceFiles", `Int (List.length case.source_paths));
      ("sourceBytes", `Int case.source_bytes);
      ("workspaceBytes", `Int case.workspace_bytes);
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

let run_profile base_root target_shape mixed_stress_shape samples
    startup_timeout_ms tick_budget_ms startup_request_load startup_only profile =
  let case = prepare_case base_root target_shape mixed_stress_shape profile in
  Printf.eprintf "bench %s: %d source files, %.2f source MB, %.2f workspace MB\n%!"
    case.config.name
    (List.length case.source_paths)
    (float_of_int case.source_bytes /. (1024.0 *. 1024.0))
    (float_of_int case.workspace_bytes /. (1024.0 *. 1024.0));
  let cold =
    run_startup case ~kind:"cold" ~clear_cache:true ~timeout_ms:startup_timeout_ms
      ~tick_budget_ms ~request_load:startup_request_load
  in
  let warm =
    run_startup case ~kind:"warm" ~clear_cache:false
      ~timeout_ms:startup_timeout_ms ~tick_budget_ms
      ~request_load:startup_request_load
  in
  let hover, definition, references, semantic_range, open_file_diagnostics =
    if startup_only then
      ( empty_latency_stats "hover",
        empty_latency_stats "definition",
        empty_latency_stats "references",
        empty_latency_stats "semanticTokenRange",
        empty_latency_stats "openFileDiagnostics" )
    else run_request_latencies case warm.ws samples
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
      ("workspaceBytes", `Int result.case.workspace_bytes);
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

let mixed_stress_shape_json shape =
  `Assoc
    [
      ("sourceShape", byte_shape_json (Some shape.stress_source_shape));
      ("noiseFiles", `Int shape.stress_noise_files);
      ("noiseBytes", `Int shape.stress_noise_bytes);
      ("workspaceBytes", `Int shape.stress_workspace_bytes);
      ( "startupFileBytes",
        match shape.stress_startup_file_bytes with
        | None -> `Null
        | Some n -> `Int n );
      ("realisticSource", `Bool shape.stress_realistic_source);
      ( "activeSourceBytes",
        match shape.stress_active_source_bytes with
        | None -> `Null
        | Some n -> `Int n );
    ]

let report_json ~generated_at ~argv ~samples ~startup_timeout_ms ~tick_budget_ms
    ~startup_request_load ~startup_only ~workspace_base ~target_shape
    ~mixed_stress_shape results =
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
            ("startupRequestLoad", `Bool startup_request_load);
            ("startupOnly", `Bool startup_only);
            ("workspaceBase", `String workspace_base);
            ("targetHuge", byte_shape_json (Some target_shape));
            ("mixedStress", mixed_stress_shape_json mixed_stress_shape);
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
    "| Profile | Files | Source MB | Workspace MB | Cold skeleton ms | Cold local AST ms | Cold full nav ms | Warm full nav ms | Hover p95 ms | Definition p95 ms | References p95 ms | Semantic range p95 ms | Diagnostics p95 ms |\n";
  Buffer.add_string b
    "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |\n";
  List.iter
    (fun result ->
      Buffer.add_string b
        (Printf.sprintf
           "| %s | %d | %.2f | %.2f | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
           result.case.config.name
           (List.length result.case.source_paths)
           (float_of_int result.case.source_bytes /. (1024.0 *. 1024.0))
           (float_of_int result.case.workspace_bytes /. (1024.0 *. 1024.0))
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
  let startup_request_load = ref false in
  let startup_only = ref false in
  let target_files = ref default_target_shape.target_files in
  let target_average_mb =
    ref (float_of_int default_target_shape.average_bytes /. (1024.0 *. 1024.0))
  in
  let target_outliers = ref default_target_shape.outlier_count in
  let target_outlier_mb =
    ref (float_of_int default_target_shape.outlier_bytes /. (1024.0 *. 1024.0))
  in
  let stress_files =
    ref default_mixed_stress_shape.stress_source_shape.target_files
  in
  let stress_average_mb =
    ref
      (float_of_int
         default_mixed_stress_shape.stress_source_shape.average_bytes
      /. (1024.0 *. 1024.0))
  in
  let stress_outliers =
    ref default_mixed_stress_shape.stress_source_shape.outlier_count
  in
  let stress_outlier_mb =
    ref
      (float_of_int default_mixed_stress_shape.stress_source_shape.outlier_bytes
      /. (1024.0 *. 1024.0))
  in
  let stress_noise_files = ref default_mixed_stress_shape.stress_noise_files in
  let stress_noise_mb =
    ref
      (float_of_int default_mixed_stress_shape.stress_noise_bytes
      /. (1024.0 *. 1024.0))
  in
  let stress_workspace_gb =
    ref
      (float_of_int default_mixed_stress_shape.stress_workspace_bytes
      /. (1024.0 *. 1024.0 *. 1024.0))
  in
  let stress_startup_file_mb =
    ref
      (match default_mixed_stress_shape.stress_startup_file_bytes with
      | None -> 0.0
      | Some n -> float_of_int n /. (1024.0 *. 1024.0))
  in
  let stress_realistic_source = ref false in
  let stress_active_source_kb =
    ref
      (match default_mixed_stress_shape.stress_active_source_bytes with
      | None -> 0.0
      | Some n -> float_of_int n /. 1024.0)
  in
  let workspace_root = ref None in
  let out_dir = ref (default_report_dir ()) in
  let output = ref None in
  let set_optional r value = r := Some (absolute_path value) in
  let speclist =
    [
      ( "--profiles",
        Arg.Set_string profiles_arg,
        "Comma-separated profiles: small,medium,large,huge,target-huge,mixed-stress,all,everything" );
      ( "--samples",
        Arg.Set_int samples,
        "Request latency samples per operation (default: 40)" );
      ( "--startup-timeout-ms",
        Arg.Set_int startup_timeout_ms,
        "Startup drain timeout per cold/warm run (default: 30000)" );
      ( "--tick-budget-ms",
        Arg.Set_int tick_budget_ms,
        "Background tick budget while draining startup (default: 50)" );
      ( "--startup-request-load",
        Arg.Set startup_request_load,
        "Continuously issue rotating hover/definition/references/semantic/diagnostics requests during startup" );
      ( "--startup-only",
        Arg.Set startup_only,
        "Run cold/warm startup and startup-load probes only; skip post-startup latency sweeps" );
      ( "--target-files",
        Arg.Set_int target_files,
        "Source file count for target-huge profile (default: 520)" );
      ( "--target-average-mb",
        Arg.Set_float target_average_mb,
        "Average source file size for target-huge profile in MiB (default: 2.0)" );
      ( "--target-outliers",
        Arg.Set_int target_outliers,
        "Number of target-huge outlier files (default: 5)" );
      ( "--target-outlier-mb",
        Arg.Set_float target_outlier_mb,
        "Outlier source file size for target-huge profile in MiB (default: 50.0)" );
      ( "--stress-files",
        Arg.Set_int stress_files,
        "Source file count for mixed-stress profile (default: 640)" );
      ( "--stress-average-mb",
        Arg.Set_float stress_average_mb,
        "Average source file size for mixed-stress profile in MiB (default: 2.5)" );
      ( "--stress-outliers",
        Arg.Set_int stress_outliers,
        "Number of mixed-stress outlier source files (default: 8)" );
      ( "--stress-outlier-mb",
        Arg.Set_float stress_outlier_mb,
        "Outlier source file size for mixed-stress profile in MiB (default: 64.0)" );
      ( "--stress-noise-files",
        Arg.Set_int stress_noise_files,
        "Non-source ballast file count for mixed-stress profile (default: 80)" );
      ( "--stress-noise-mb",
        Arg.Set_float stress_noise_mb,
        "Size of each non-source ballast file for mixed-stress profile in MiB (default: 2.0)" );
      ( "--stress-workspace-gb",
        Arg.Set_float stress_workspace_gb,
        "Logical total workspace size for mixed-stress profile in GiB; non-source ballast is sparse (default: 20.0)" );
      ( "--stress-startup-file-mb",
        Arg.Set_float stress_startup_file_mb,
        "Target size for MAIN000 in mixed-stress profile in MiB; 0 disables override (default: 0.75)" );
      ( "--stress-realistic-source",
        Arg.Set stress_realistic_source,
        "Generate realistic JOVIAL-like source bodies with comment ballast instead of whitespace-padded files" );
      ( "--stress-active-source-kb",
        Arg.Set_float stress_active_source_kb,
        "Active non-comment source per realistic mixed-stress file in KiB; 0 disables density cap (default: 32)" );
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
  let samples = if !startup_only then 0 else max 1 !samples in
  let startup_timeout_ms = max 1 !startup_timeout_ms in
  let tick_budget_ms = max 1 !tick_budget_ms in
  let mb_to_bytes mb =
    max 1 (int_of_float (Float.ceil (max 0.0 mb *. 1024.0 *. 1024.0)))
  in
  let gb_to_bytes gb =
    max 1
      (int_of_float
         (Float.ceil (max 0.0 gb *. 1024.0 *. 1024.0 *. 1024.0)))
  in
  let target_shape =
    {
      target_files = max 2 !target_files;
      average_bytes = mb_to_bytes !target_average_mb;
      outlier_count = max 0 !target_outliers;
      outlier_bytes = mb_to_bytes !target_outlier_mb;
    }
  in
  let mixed_stress_shape =
    {
      stress_source_shape =
        {
          target_files = max 3 !stress_files;
          average_bytes = mb_to_bytes !stress_average_mb;
          outlier_count = max 0 !stress_outliers;
          outlier_bytes = mb_to_bytes !stress_outlier_mb;
        };
      stress_noise_files = max 0 !stress_noise_files;
      stress_noise_bytes = mb_to_bytes !stress_noise_mb;
      stress_workspace_bytes = gb_to_bytes !stress_workspace_gb;
      stress_startup_file_bytes =
        if !stress_startup_file_mb <= 0.0 then None
        else Some (mb_to_bytes !stress_startup_file_mb);
      stress_realistic_source = !stress_realistic_source;
      stress_active_source_bytes =
        if !stress_active_source_kb <= 0.0 then None
        else
          Some
            (max 1
               (int_of_float
                  (Float.ceil (max 0.0 !stress_active_source_kb *. 1024.0))));
    }
  in
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
      (run_profile workspace_base target_shape mixed_stress_shape samples
         startup_timeout_ms tick_budget_ms !startup_request_load !startup_only)
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
      ~startup_timeout_ms ~tick_budget_ms
      ~startup_request_load:!startup_request_load ~startup_only:!startup_only
      ~workspace_base ~target_shape ~mixed_stress_shape results
  in
  write_json json_path json;
  let md_path = replace_extension json_path ".md" in
  write_text md_path (markdown_summary ~generated_at ~json_path results);
  Printf.printf "wrote %s\nwrote %s\n%!" json_path md_path
