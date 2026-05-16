(* Module overview: Line and offset index for converting between byte offsets and LSP positions. *)

type t = {
  text : string;
  (* line_starts.(i) = absolute offset of first char in line i (0-based) *)
  mutable line_starts : int array option;
}

let line_starts_of_string (text : string) : int array =
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
      if !line < lines then starts.(!line) <- i + 1)
  done;
  starts

let ensure_line_starts (t : t) : int array =
  match t.line_starts with
  | Some starts -> starts
  | None ->
      let starts = line_starts_of_string t.text in
      t.line_starts <- Some starts;
      starts

let of_string (text : string) : t = { text; line_starts = None }

let clamp_offset (t : t) (off : int) : int =
  let n = String.length t.text in
  if off < 0 then 0 else if off > n then n else off

let line_index_of_offset (t : t) (off : int) : int =
  let off = clamp_offset t off in
  let starts = ensure_line_starts t in
  let lo = ref 0 in
  let hi = ref (Array.length starts - 1) in
  while !lo <= !hi do
    let mid = (!lo + !hi) / 2 in
    if starts.(mid) <= off then lo := mid + 1 else hi := mid - 1
  done;
  max 0 !hi

let first_line_index_after_offset (t : t) (off : int) : int option =
  let off = clamp_offset t off in
  let starts = ensure_line_starts t in
  let lo = ref 0 in
  let hi = ref (Array.length starts) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if starts.(mid) > off then hi := mid else lo := mid + 1
  done;
  if !lo >= Array.length starts then None else Some !lo

let replace_range (t : t) ~(start_off : int) ~(end_off : int)
    ~(replacement : string) ~(text : string) : t =
  if text = t.text then t
  else
    let a = clamp_offset t start_off in
    let b = clamp_offset t end_off in
    let a, b = if a <= b then (a, b) else (b, a) in
    let old_len = String.length t.text in
    if a = 0 && b = old_len then of_string text
    else
      let seg_start_idx = line_index_of_offset t a in
      let starts_prev = ensure_line_starts t in
      let seg_start = starts_prev.(seg_start_idx) in
      let suffix_idx =
        match first_line_index_after_offset t b with
        | Some idx -> idx
        | None -> Array.length starts_prev
      in
      let suffix_exists = suffix_idx < Array.length starts_prev in
      let seg_end =
        if suffix_exists then starts_prev.(suffix_idx) else old_len
      in
      let before_len = max 0 (a - seg_start) in
      let after_len = max 0 (seg_end - b) in
      let segment_len = before_len + String.length replacement + after_len in
      let buf = Buffer.create segment_len in
      if before_len > 0 then
        Buffer.add_substring buf t.text seg_start before_len;
      Buffer.add_string buf replacement;
      if after_len > 0 then Buffer.add_substring buf t.text b after_len;
      let segment_text = Buffer.contents buf in
      let segment_starts_full = line_starts_of_string segment_text in
      let segment_count =
        let count = Array.length segment_starts_full in
        if
          suffix_exists && count > 0
          && segment_starts_full.(count - 1) = String.length segment_text
        then count - 1
        else count
      in
      let prefix_count = seg_start_idx in
      let suffix_count = Array.length starts_prev - suffix_idx in
      let starts = Array.make (prefix_count + segment_count + suffix_count) 0 in
      if prefix_count > 0 then Array.blit starts_prev 0 starts 0 prefix_count;
      for i = 0 to segment_count - 1 do
        starts.(prefix_count + i) <- seg_start + segment_starts_full.(i)
      done;
      (if suffix_count > 0 then
         let delta = String.length text - old_len in
         for i = 0 to suffix_count - 1 do
           starts.(prefix_count + segment_count + i) <-
             starts_prev.(suffix_idx + i) + delta
         done);
      { text; line_starts = Some starts }

let line_count (t : t) = Array.length (ensure_line_starts t)

let line_start_offset (t : t) ~(line : int) =
  let starts = ensure_line_starts t in
  if line < 0 || line >= Array.length starts then None else Some starts.(line)

let line_length (t : t) ~(line : int) =
  match line_start_offset t ~line with
  | None -> None
  | Some start ->
      let starts = ensure_line_starts t in
      let next_start =
        if line + 1 < Array.length starts then starts.(line + 1)
        else String.length t.text + 1
      in
      let raw = next_start - start in
      let len =
        if
          raw > 0
          && start + raw - 1 < String.length t.text
          && t.text.[start + raw - 1] = '\n'
        then raw - 1
        else raw
      in
      Some (max 0 len)

let offset_of_line_col (t : t) ~(line : int) ~(col : int) =
  if col < 0 then None
  else
    match line_start_offset t ~line with
    | None -> None
    | Some start ->
        let starts = ensure_line_starts t in
        let next_start =
          if line + 1 < Array.length starts then starts.(line + 1)
          else String.length t.text + 1
        in
        let raw = next_start - start in
        let line_len =
          (* exclude trailing '\n' if present *)
          if
            raw > 0
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
  let starts = ensure_line_starts t in
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
