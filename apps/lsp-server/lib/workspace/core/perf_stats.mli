type metric = {
  calls : int;
  total_ms : float;
  max_ms : float;
  last_ms : float;
}

val now_ms : unit -> float
val observe_ms : string -> float -> unit
val time : string -> (unit -> 'a) -> 'a
val tick : string -> unit

val snapshot_json : unit -> Yojson.Safe.t
val reset : unit -> unit
