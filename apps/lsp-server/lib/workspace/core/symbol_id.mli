(** Module overview: Stable symbol identifier helpers used by semantic and LSIF indexes. *)

type t = private int

val of_int : int -> t
val of_stable_string : string -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val to_int : t -> int
val to_string : t -> string
