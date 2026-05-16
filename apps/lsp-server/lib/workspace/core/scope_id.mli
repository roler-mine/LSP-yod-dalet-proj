(** Module overview: Stable scope identifier helpers for semantic graph entries. *)

type t = private int

val of_int : int -> t
val of_stable_string : string -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val to_int : t -> int
val to_string : t -> string
