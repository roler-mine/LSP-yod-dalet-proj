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

let rank = function
  | LexicalOnly -> 0
  | SkeletonReady -> 1
  | LocalAstReady -> 2
  | WorkspaceSemanticReady -> 3
  | CrossModuleSemanticReady -> 4

let compare a b = Int.compare (rank a) (rank b)
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let is_authoritative r =
  match r.authority with Authoritative -> true | Provisional -> false

let map f r = { r with value = f r.value }

let provisional ?(reasons = []) ~readiness value =
  { value; readiness; authority = Provisional; reasons }

let authoritative ~readiness value =
  { value; readiness; authority = Authoritative; reasons = [] }

let label = function
  | LexicalOnly -> "lexicalOnly"
  | SkeletonReady -> "skeletonReady"
  | LocalAstReady -> "localAstReady"
  | WorkspaceSemanticReady -> "workspaceSemanticReady"
  | CrossModuleSemanticReady -> "crossModuleSemanticReady"

let authority_label = function
  | Provisional -> "provisional"
  | Authoritative -> "authoritative"

let reason_label = function
  | ParseStale -> "parseStale"
  | ParseSkippedLargeFile -> "parseSkippedLargeFile"
  | WorkspaceIndexWarming -> "workspaceIndexWarming"
  | CrossModuleIndexWarming -> "crossModuleIndexWarming"
  | SemanticStoreMiss -> "semanticStoreMiss"
  | MacroExpansionUnavailable -> "macroExpansionUnavailable"
  | Cancelled -> "cancelled"
  | SoftBudgetExceeded -> "softBudgetExceeded"
  | MemoryPressure -> "memoryPressure"
  | UnknownReason s -> "unknown:" ^ s

let result_json ~(value : 'a -> Yojson.Safe.t) (r : 'a result) :
    Yojson.Safe.t =
  `Assoc
    [
      ("value", value r.value);
      ("readiness", `String (label r.readiness));
      ("authority", `String (authority_label r.authority));
      ("authoritative", `Bool (is_authoritative r));
      ("reasons", `List (List.map (fun reason -> `String (reason_label reason)) r.reasons));
    ]
