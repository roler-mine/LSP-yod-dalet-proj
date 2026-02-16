let () =
  (* Critical on Windows for LSP Content-Length framing *)
  if Sys.win32 then (
    set_binary_mode_in stdin true;
    set_binary_mode_out stdout true;
  );

  Jovial_lsp_lib.Lsp_server.run stdin stdout
