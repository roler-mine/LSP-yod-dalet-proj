let initialize_stdio () =
  if Sys.win32 then (
    set_binary_mode_in stdin true;
    set_binary_mode_out stdout true)

let run_server () = Jovial_lsp_lib.Lsp_server.run stdin stdout

let () =
  initialize_stdio ();
  run_server ()
