(** Module overview: Budget helpers that keep background work within interactive latency targets. *)

type check_result =
  | Continue
  | Stop of Workspace_readiness.reason

type t

val start : ws:Workspace_state.t -> soft_budget_ms:int -> t
val check : ?phase:string -> t -> check_result
val should_stop : ?phase:string -> t -> bool
val reason_if_stopped : t -> Workspace_readiness.reason option
val checkpoints : t -> int
val exceeded : t -> bool
val cancelled : t -> bool
