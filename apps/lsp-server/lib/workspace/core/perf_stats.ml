type metric_mut = {
  mutable calls : int;
  mutable total_ms : float;
  mutable max_ms : float;
  mutable last_ms : float;
}

type metric = { calls : int; total_ms : float; max_ms : float; last_ms : float }

let metrics : (string, metric_mut) Hashtbl.t = Hashtbl.create 64

let metric_for (name : string) : metric_mut =
  match Hashtbl.find_opt metrics name with
  | Some m -> m
  | None ->
      let m : metric_mut =
        { calls = 0; total_ms = 0.0; max_ms = 0.0; last_ms = 0.0 }
      in
      Hashtbl.add metrics name m;
      m

let observe_ms (name : string) (elapsed_ms : float) : unit =
  let m = metric_for name in
  m.calls <- m.calls + 1;
  m.total_ms <- m.total_ms +. elapsed_ms;
  m.last_ms <- elapsed_ms;
  if elapsed_ms > m.max_ms then m.max_ms <- elapsed_ms

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

let snapshot_json () : Yojson.Safe.t =
  let entries =
    Hashtbl.fold
      (fun name m acc ->
        let snap = metric_snapshot m in
        let avg_ms =
          if snap.calls <= 0 then 0.0
          else snap.total_ms /. float_of_int snap.calls
        in
        `Assoc
          [
            ("name", `String name);
            ("calls", `Int snap.calls);
            ("totalMs", `Float snap.total_ms);
            ("avgMs", `Float avg_ms);
            ("maxMs", `Float snap.max_ms);
            ("lastMs", `Float snap.last_ms);
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
