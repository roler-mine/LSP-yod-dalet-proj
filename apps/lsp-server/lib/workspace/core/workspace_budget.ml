(* Module overview: Budget helpers that keep background work within interactive latency targets. *)

type check_result =
  | Continue
  | Stop of Workspace_readiness.reason

type t = {
  ws : Workspace_state.t;
  deadline_ms : float;
  mutable exceeded : bool;
  mutable cancelled : bool;
  mutable checkpoints : int;
  mutable stop_reason : Workspace_readiness.reason option;
}

module Perf_stats = Workspace_foundation.Perf_stats
module R = Workspace_readiness

let metric_fragment s =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'A' .. 'Z' as c -> Bytes.set b i (Char.lowercase_ascii c)
    | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> ()
    | _ -> Bytes.set b i '_'
  done;
  Bytes.unsafe_to_string b

let tick_stop ?phase reason =
  let reason_label = metric_fragment (R.reason_label reason) in
  Perf_stats.tick ("budget.stop." ^ reason_label);
  (match reason with
  | R.Cancelled ->
      Perf_stats.tick "cancel.applied";
      Perf_stats.tick "budget.cancelled"
  | R.SoftBudgetExceeded ->
      Perf_stats.tick "nav.soft_budget_exceeded";
      Perf_stats.tick "budget.soft_budget_exceeded"
  | _ -> ());
  match phase with
  | None -> ()
  | Some phase when String.trim phase = "" -> ()
  | Some phase ->
      Perf_stats.tick
        ("budget.stop." ^ metric_fragment phase ^ "." ^ reason_label)

let start ~(ws : Workspace_state.t) ~(soft_budget_ms : int) : t =
  let soft_budget_ms = max 0 soft_budget_ms in
  {
    ws;
    deadline_ms = Perf_stats.now_ms () +. float_of_int soft_budget_ms;
    exceeded = false;
    cancelled = false;
    checkpoints = 0;
    stop_reason = None;
  }

let set_stopped ?phase budget reason =
  (match reason with
  | R.Cancelled -> budget.cancelled <- true
  | R.SoftBudgetExceeded -> budget.exceeded <- true
  | _ -> ());
  match budget.stop_reason with
  | Some reason -> Stop reason
  | None ->
      budget.stop_reason <- Some reason;
      tick_stop ?phase reason;
      Stop reason

let check ?phase budget =
  budget.checkpoints <- budget.checkpoints + 1;
  match budget.stop_reason with
  | Some reason -> Stop reason
  | None ->
      if Workspace_runtime.request_cancelled budget.ws then
        set_stopped ?phase budget R.Cancelled
      else if Perf_stats.now_ms () >= budget.deadline_ms then
        set_stopped ?phase budget R.SoftBudgetExceeded
      else Continue

let should_stop ?phase budget =
  match check ?phase budget with Continue -> false | Stop _ -> true

let reason_if_stopped budget = budget.stop_reason
let checkpoints budget = budget.checkpoints
let exceeded budget = budget.exceeded
let cancelled budget = budget.cancelled
