type metric_mut = {
  mutable calls : int;
  mutable total_ms : float;
  mutable max_ms : float;
  mutable last_ms : float;
  samples : float array;
  mutable sample_count : int;
  mutable sample_pos : int;
}

type metric = { calls : int; total_ms : float; max_ms : float; last_ms : float }

let metrics : (string, metric_mut) Hashtbl.t = Hashtbl.create 64
let sample_window = 128

let metric_for (name : string) : metric_mut =
  match Hashtbl.find_opt metrics name with
  | Some m -> m
  | None ->
      let m : metric_mut =
        {
          calls = 0;
          total_ms = 0.0;
          max_ms = 0.0;
          last_ms = 0.0;
          samples = Array.make sample_window 0.0;
          sample_count = 0;
          sample_pos = 0;
        }
      in
      Hashtbl.add metrics name m;
      m

let observe_ms (name : string) (elapsed_ms : float) : unit =
  let m = metric_for name in
  m.calls <- m.calls + 1;
  m.total_ms <- m.total_ms +. elapsed_ms;
  m.last_ms <- elapsed_ms;
  if elapsed_ms > m.max_ms then m.max_ms <- elapsed_ms;
  if sample_window > 0 then (
    m.samples.(m.sample_pos) <- elapsed_ms;
    m.sample_pos <- (m.sample_pos + 1) mod sample_window;
    if m.sample_count < sample_window then m.sample_count <- m.sample_count + 1)

let now_ms () : float = Unix.gettimeofday () *. 1000.0

let time (name : string) (f : unit -> 'a) : 'a =
  let t0 = now_ms () in
  try
    let out = f () in
    let t1 = now_ms () in
    observe_ms name (max 0.0 (t1 -. t0));
    out
  with exn ->
    let t1 = now_ms () in
    observe_ms name (max 0.0 (t1 -. t0));
    raise exn

let tick (name : string) : unit = observe_ms name 0.0

let metric_snapshot (m : metric_mut) : metric =
  {
    calls = m.calls;
    total_ms = m.total_ms;
    max_ms = m.max_ms;
    last_ms = m.last_ms;
  }

let percentile_from_samples (m : metric_mut) (pct : float) : float =
  if m.sample_count <= 0 then 0.0
  else
    let samples = Array.sub m.samples 0 m.sample_count in
    Array.sort Float.compare samples;
    let pct = if pct < 0.0 then 0.0 else if pct > 1.0 then 1.0 else pct in
    let idx =
      int_of_float (Float.ceil (pct *. float_of_int m.sample_count)) - 1
    in
    let idx = if idx < 0 then 0 else min idx (m.sample_count - 1) in
    samples.(idx)

let snapshot_json () : Yojson.Safe.t =
  let entries =
    Hashtbl.fold
      (fun name m acc ->
        let snap = metric_snapshot m in
        let avg_ms =
          if snap.calls <= 0 then 0.0
          else snap.total_ms /. float_of_int snap.calls
        in
        let p50_ms = percentile_from_samples m 0.50 in
        let p95_ms = percentile_from_samples m 0.95 in
        let p99_ms = percentile_from_samples m 0.99 in
        `Assoc
          [
            ("name", `String name);
            ("calls", `Int snap.calls);
            ("totalMs", `Float snap.total_ms);
            ("avgMs", `Float avg_ms);
            ("maxMs", `Float snap.max_ms);
            ("lastMs", `Float snap.last_ms);
            ("sampleCount", `Int m.sample_count);
            ("p50Ms", `Float p50_ms);
            ("p95Ms", `Float p95_ms);
            ("p99Ms", `Float p99_ms);
          ]
        :: acc)
      metrics []
    |> List.sort (fun a b ->
        match (a, b) with
        | `Assoc fa, `Assoc fb ->
            let name fields =
              match List.assoc_opt "name" fields with
              | Some (`String s) -> s
              | _ -> ""
            in
            String.compare (name fa) (name fb)
        | _ -> 0)
  in
  `Assoc [ ("metrics", `List entries) ]

let reset () : unit = Hashtbl.clear metrics
