(* Module overview: Build-time utility used by repository maintenance scripts. *)

let remove_if_exists path =
  try if Sys.file_exists path then Sys.remove path with _ -> ()

let copy_file src dst =
  let ic = open_in_bin src in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let oc = open_out_bin dst in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          let buf = Bytes.create 65536 in
          let rec loop () =
            match input ic buf 0 (Bytes.length buf) with
            | 0 -> ()
            | n ->
                output oc buf 0 n;
                loop ()
          in
          loop ()))

let () =
  if Array.length Sys.argv >= 3 then (
    copy_file Sys.argv.(1) Sys.argv.(2);
    Array.iteri
      (fun i path -> if i > 0 && i <> 2 then remove_if_exists path)
      Sys.argv)
  else
    Array.iteri
      (fun i path -> if i > 0 then remove_if_exists path)
      Sys.argv
