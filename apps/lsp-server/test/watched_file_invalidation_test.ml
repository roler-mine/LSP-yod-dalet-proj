module T = Lsp.Types
module Lib = Jovial_lsp_lib

let failf fmt = Printf.ksprintf failwith fmt

let uri_of_path_exn (p:string) : T.DocumentUri.t =
  match Lib.Uri_path.docuri_of_path p with
  | Some u -> u
  | None -> failf "invalid path URI: %s" p

let write_text (path:string) (text:string) : unit =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let () =
  let tmp_root = Filename.concat (Filename.get_temp_dir_name ()) "jovial-watch-invalidation-test" in
  (try if Sys.file_exists tmp_root then Sys.remove tmp_root with _ -> ());
  (try Unix.mkdir tmp_root 0o755 with _ -> ());

  let src_path = Filename.concat tmp_root "MAIN.j73" in
  let src_text =
    String.concat "\n"
      [
        "START";
        "DEF PROC MAIN RENT;";
        "BEGIN";
        "  ITEM X U 1;";
        "  X = X + 1;";
        "END";
        "TERM";
        "";
      ]
  in
  write_text src_path src_text;

  let ws = Lib.Workspace.create () in
  Lib.Workspace.set_root_path ws (Some tmp_root);
  Lib.Workspace.rescan ws;
  let uri = uri_of_path_exn src_path in
  Lib.Workspace.open_doc ws ~uri ~file:(Some src_path) ~text:src_text;

  ignore (Lib.Workspace.lsif_index_json ws);

  Lib.Workspace.apply_watched_file_changes ws
    ~changes:[ (src_path, `Changed) ];

  let delta = Lib.Workspace.lsif_delta_json ws ~base_revision:0 in
  (match delta with
   | `Assoc _ -> ()
   | _ -> failf "lsif delta response should be object after watched-file invalidation");

  print_endline "watched_file_invalidation_test: ok"
