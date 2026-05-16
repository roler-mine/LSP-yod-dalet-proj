(** Module overview: Startup readiness state machine for navigation and diagnostics targets. *)

type t =
  | LexicalOnly
  | SkeletonReady
  | LocalAstReady
  | WorkspaceSemanticReady
  | CrossModuleSemanticReady

type authority = Provisional | Authoritative

type reason =
  | ParseStale
  | ParseSkippedLargeFile
  | WorkspaceIndexWarming
  | CrossModuleIndexWarming
  | SemanticStoreMiss
  | MacroExpansionUnavailable
  | Cancelled
  | SoftBudgetExceeded
  | MemoryPressure
  | UnknownReason of string

type 'a result = {
  value : 'a;
  readiness : t;
  authority : authority;
  reasons : reason list;
}

val compare : t -> t -> int
val min : t -> t -> t
val max : t -> t -> t
val is_authoritative : 'a result -> bool
val map : ('a -> 'b) -> 'a result -> 'b result
val provisional : ?reasons:reason list -> readiness:t -> 'a -> 'a result
val authoritative : readiness:t -> 'a -> 'a result
val label : t -> string
val authority_label : authority -> string
val reason_label : reason -> string
val result_json : value:('a -> Yojson.Safe.t) -> 'a result -> Yojson.Safe.t
