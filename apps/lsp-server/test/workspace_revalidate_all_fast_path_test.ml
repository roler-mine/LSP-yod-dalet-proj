module T = Lsp.Types
module Lib = Jovial_lsp_lib
module WB = Jovial_lsp_lib.Workspace_base

let failf fmt = Printf.ksprintf failwith fmt

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

let uri_of_path_exn (path : string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_path path with
  | Some uri -> uri
  | None -> failf "invalid path for uri conversion: %s" path

let find_nth_substring (s : string) ~(needle : string) ~(nth : int) : int =
  let n = String.length s in
  let m = String.length needle in
  if m = 0 then failf "needle must be non-empty";
  let rec seek_from i found =
    if i + m > n then failf "substring %S occurrence #%d not found" needle nth
    else if String.sub s i m = needle then
      if found = nth then i else seek_from (i + 1) (found + 1)
    else seek_from (i + 1) found
  in
  seek_from 0 0

let position_of_offset (s : string) (off : int) : T.Position.t =
  if off < 0 || off > String.length s then failf "offset out of bounds: %d" off;
  let rec loop i line col =
    if i >= off then ({ line; character = col } : T.Position.t)
    else if s.[i] = '\n' then loop (i + 1) (line + 1) 0
    else loop (i + 1) line (col + 1)
  in
  loop 0 0 0

let metric_calls (stats : Yojson.Safe.t) ~(name : string) : int =
  match stats with
  | `Assoc fields -> (
      match List.assoc_opt "metrics" fields with
      | Some (`List metrics) ->
          let rec find = function
            | [] -> 0
            | `Assoc mfields :: tl -> (
                match
                  (List.assoc_opt "name" mfields, List.assoc_opt "calls" mfields)
                with
                | Some (`String metric_name), Some (`Int calls)
                  when metric_name = name ->
                    calls
                | Some (`String metric_name), Some (`Intlit s)
                  when metric_name = name -> (
                    try int_of_string s with _ -> 0)
                | _ -> find tl)
            | _ :: tl -> find tl
          in
          find metrics
      | _ -> 0)
  | _ -> 0

let clear_open_revalidate_queue (ws : WB.t) : unit =
  Hashtbl.clear ws.open_diag_revalidate_payloads;
  Hashtbl.clear ws.open_diag_revalidate_set;
  while not (Queue.is_empty ws.open_diag_revalidate_updates) do
    ignore (Queue.pop ws.open_diag_revalidate_updates)
  done

let expect_queue_empty (ws : WB.t) : unit =
  if not (Queue.is_empty ws.open_diag_revalidate_updates) then
    failf "expected open revalidate queue to be empty";
  if Hashtbl.length ws.open_diag_revalidate_payloads <> 0 then
    failf "expected open revalidate payloads to be empty";
  if Hashtbl.length ws.open_diag_revalidate_set <> 0 then
    failf "expected open revalidate set to be empty"

let pool_text =
  String.concat "\n"
    [
      "START";
      "COMPOOL POOLA;";
      "DEF BEGIN";
      "  ITEM EXISTS U 1;";
      "END";
      "TERM";
      "";
    ]

let compa_text =
  String.concat "\n"
    [
      "START";
      "!COMPOOL ('POOLA');";
      "COMPOOL COMPA;";
      "DEF BEGIN";
      "  ITEM AVAL U 1;";
      "END";
      "TERM";
      "";
    ]

let main_text =
  String.concat "\n"
    [
      "START";
      "!COMPOOL ('COMPA');";
      "DEF PROC MAIN RENT;";
      "BEGIN";
      "  ITEM LOCAL U 1;";
      "  LOCAL = AVAL'COMPA;";
      "END";
      "TERM";
      "";
    ]

let () =
  Random.self_init ();
  Lib.Perf_stats.reset ();

  let root = mk_temp_dir "jovial-ws-revalidate-fast-path" in
  let pool_path = Filename.concat root "POOLA.j73" in
  let compa_path = Filename.concat root "COMPA.j73" in
  let main_path = Filename.concat root "MAIN.j73" in
  write_text pool_path pool_text;
  write_text compa_path compa_text;
  write_text main_path main_text;

  let pool_uri = uri_of_path_exn pool_path in
  let compa_uri = uri_of_path_exn compa_path in
  let main_uri = uri_of_path_exn main_path in
  let ws = WB.create () in
  WB.set_root_path ws (Some root);
  WB.rescan ws;
  WB.open_doc ws ~uri:pool_uri ~file:(Some pool_path) ~text:pool_text;
  WB.open_doc ws ~uri:compa_uri ~file:(Some compa_path) ~text:compa_text;
  WB.open_doc ws ~uri:main_uri ~file:(Some main_path) ~text:main_text;
  clear_open_revalidate_queue ws;

  let local_usage_pos =
    find_nth_substring main_text ~needle:"LOCAL" ~nth:1 |> fun off ->
    position_of_offset main_text (off + 1)
  in
  ignore (WB.definition_locations_for ws ~uri:main_uri ~pos:local_usage_pos);
  let sem_rev_before = Lib.Semantic_store.global_rev ws.semantic_store in
  if sem_rev_before <= 0 then
    failf "expected semantic store to be populated before revalidate_all";

  let hint_values = (Hashtbl.create 1, Hashtbl.create 1) in
  ws.symbol_hints <- Some hint_values;
  ws.graph_needs_refresh <- false;

  let revalidated = WB.revalidate_all ws in
  if List.length revalidated <> 3 then
    failf "expected 3 revalidated open docs, got %d" (List.length revalidated);
  if ws.graph_needs_refresh then
    failf "revalidate_all should not mark the graph dirty for unchanged docs";
  (match ws.symbol_hints with
  | Some (defs, refs) when defs == fst hint_values && refs == snd hint_values ->
      ()
  | _ -> failf "revalidate_all should not invalidate symbol hints");
  expect_queue_empty ws;

  let sem_rev_after = Lib.Semantic_store.global_rev ws.semantic_store in
  if sem_rev_after <> sem_rev_before then
    failf
      "revalidate_all should not invalidate semantic-store snapshots for \
       unchanged docs (before=%d after=%d)"
      sem_rev_before sem_rev_after;

  let stats = WB.perf_stats_json ws in
  if metric_calls stats ~name:"diag.open.revalidate_fast_path" < 3 then
    failf "expected diag.open.revalidate_fast_path to be recorded";
  if metric_calls stats ~name:"store_doc.side_effects_skipped" < 3 then
    failf "expected store_doc.side_effects_skipped to be recorded";

  print_endline "workspace_revalidate_all_fast_path_test: ok"
