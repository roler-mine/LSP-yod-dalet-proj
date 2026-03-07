type t

type delta = {
  base_revision : int;
  revision : int;
  reset : bool;
  deletes : string list;
  upserts : Yojson.Safe.t list;
}

val create : unit -> t
val revision : t -> int
val reset : t -> unit
val symbols_of_index_json : Yojson.Safe.t -> (string, Yojson.Safe.t) Hashtbl.t

val update_full :
  t -> revision:int -> symbols:(string, Yojson.Safe.t) Hashtbl.t -> unit

val diff :
  t ->
  base_revision:int ->
  current_revision:int ->
  current_symbols:(string, Yojson.Safe.t) Hashtbl.t ->
  delta

val delta_json : delta -> Yojson.Safe.t
