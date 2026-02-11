type t = {
  text : string;
  (* line_starts.(i) = absolute offset of first char in line i (0-based) *)
  line_starts : int array;
}

let of_string (text : string) : t =
  let n = String.length text in
  let lines =
    let c = ref 1 in
    for i = 0 to n - 1 do
      if text.[i] = '\n' then incr c
    done;
    !c
  in
  let starts = Array.make lines 0 in
  let line = ref 0 in
  starts.(0) <- 0;
  for i = 0 to n - 1 do
    if text.[i] = '\n' then (
      incr line;
      if !line < lines then starts.(!line) <- i + 1
    )
  done;
  { text; line_starts = starts }

let line_count (t : t) = Array.length t.line_starts

let line_start_offset (t : t) ~(line:int) =
  if line < 0 || line >= Array.length t.line_starts then None
  else Some t.line_starts.(line)

let offset_of_line_col (t : t) ~(line:int) ~(col:int) =
  if col < 0 then None
  else
    match line_start_offset t ~line with
    | None -> None
    | Some start ->
        let next_start =
          if line + 1 < Array.length t.line_starts
          then t.line_starts.(line + 1)
          else String.length t.text + 1
        in
        let raw = next_start - start in
        let line_len =
          (* exclude trailing '\n' if present *)
          if raw > 0
             && start + raw - 1 < String.length t.text
             && t.text.[start + raw - 1] = '\n'
          then raw - 1
          else raw
        in
        if col > line_len then None else Some (start + col)

let line_col_of_offset (t : t) (off : int) : int * int =
  let off =
    if off < 0 then 0
    else
      let n = String.length t.text in
      if off > n then n else off
  in
  let starts = t.line_starts in
  (* rightmost start <= off *)
  let lo = ref 0 in
  let hi = ref (Array.length starts - 1) in
  while !lo <= !hi do
    let mid = (!lo + !hi) / 2 in
    if starts.(mid) <= off then lo := mid + 1 else hi := mid - 1
  done;
  let line = max 0 !hi in
  let col = off - starts.(line) in
  (line, col)
