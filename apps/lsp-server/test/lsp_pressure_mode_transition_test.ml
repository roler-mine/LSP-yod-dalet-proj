module WB = Jovial_lsp_lib.Workspace_base

let failf fmt = Printf.ksprintf failwith fmt

let getenv_int (name:string) ~(default:int) : int =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw ->
      (try int_of_string (String.trim raw) with _ -> default)

let sleep_seconds (secs:float) : unit =
  ignore (Unix.select [] [] [] secs)

let gc_snapshot () : string =
  let s = Gc.quick_stat () in
  Printf.sprintf "live_words=%d heap_words=%d top_heap_words=%d"
    s.Gc.live_words
    s.Gc.heap_words
    s.Gc.top_heap_words

let () =
  Random.self_init ();
  let hard_timeout_s =
    float_of_int (max 1 (getenv_int "JOVIAL_TEST_INDEXING_HARD_TIMEOUT_S" ~default:300))
  in
  let started = Unix.gettimeofday () in
  let ensure_budget (phase:string) : unit =
    if hard_timeout_s -. (Unix.gettimeofday () -. started) <= 0.0 then
      failf "pressure-mode transition test exceeded hard timeout (%.1fs) at %s" hard_timeout_s phase
  in

  let ws = WB.create () in
  ignore (WB.workspace_pressure_mode ws);
  ensure_budget "before pressure allocation";

  let chunk_ints = 500_000 in
  let blob_count = 40 in
  let blobs = ref [] in
  for _ = 1 to blob_count do
    blobs := Array.make chunk_ints 42 :: !blobs
  done;
  Gc.full_major ();
  Gc.compact ();
  sleep_seconds 0.3;

  let pressured = WB.workspace_pressure_mode ws in
  (match pressured with
   | WB.PressureNormal ->
       failf
         "expected pressure mode to move away from normal after allocation; liveMB=%d (%s)"
         (WB.workspace_pressure_live_mb ws)
         (gc_snapshot ())
   | WB.PressureSoft | WB.PressureCritical -> ());

  blobs := [];
  Gc.full_major ();
  Gc.compact ();
  sleep_seconds 0.3;
  ensure_budget "after release";

  let rec wait_for_normal attempts =
    if attempts <= 0 then false
    else
      match WB.workspace_pressure_mode ws with
      | WB.PressureNormal -> true
      | WB.PressureSoft | WB.PressureCritical ->
          sleep_seconds 0.05;
          Gc.full_major ();
          wait_for_normal (attempts - 1)
  in
  if not (wait_for_normal 120) then
    failf
      "pressure mode did not return to normal after release (mode=%s, liveMB=%d)"
      (WB.pressure_mode_to_string (WB.workspace_pressure_mode ws))
      (WB.workspace_pressure_live_mb ws);

  print_endline "lsp_pressure_mode_transition_test: ok"
