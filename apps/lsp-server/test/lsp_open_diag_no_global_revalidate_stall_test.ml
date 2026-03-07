let failf = Lsp_test_helpers.failf

let contains_substring ~(haystack : string) ~(needle : string) : bool =
  let n = String.length haystack in
  let m = String.length needle in
  if m = 0 then true
  else
    let rec loop i =
      if i + m > n then false
      else if String.sub haystack i m = needle then true
      else loop (i + 1)
    in
    loop 0

let read_text (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let first_existing_path (candidates : string list) : string option =
  let rec loop = function
    | [] -> None
    | p :: tl -> if Sys.file_exists p then Some p else loop tl
  in
  loop candidates

let () =
  let cwd = Sys.getcwd () in
  let server_path =
    match
      first_existing_path
        [
          Filename.concat cwd "../lib/lsp/lsp_server.ml";
          Filename.concat cwd "../../lib/lsp/lsp_server.ml";
          Filename.concat cwd "lib/lsp/lsp_server.ml";
          Filename.concat cwd "../../../apps/lsp-server/lib/lsp/lsp_server.ml";
        ]
    with
    | Some p -> p
    | None -> failf "could not locate lib/lsp/lsp_server.ml from cwd=%s" cwd
  in
  let text = read_text server_path in
  if
    contains_substring ~haystack:text
      ~needle:"consume_warmup_revalidate_pending"
  then
    failf
      "steady-state loop still references consume_warmup_revalidate_pending \
       (expected URI-scoped open revalidate queue)";
  if
    not
      (contains_substring ~haystack:text
         ~needle:"publish_open_diag_revalidate_updates")
  then
    failf "steady-state loop missing publish_open_diag_revalidate_updates call";
  print_endline "lsp_open_diag_no_global_revalidate_stall_test: ok"
