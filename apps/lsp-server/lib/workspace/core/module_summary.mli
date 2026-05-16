(** Module overview: Summarizes parsed modules and compools for change detection and indexing. *)

(* Dependency-facing public surface of one source file.

   The hash is intentionally narrower than the full document digest for parsed
   files, so private procedure-body edits do not fan out to importers. When the
   summary cannot be computed from current syntax/tokens, it is marked
   conservative and the content hash participates in the public hash. *)

type public_symbol = {
  name : string;
  key : string;
  kind : string;
  loc : Ast.Loc.t;
  signature : string;
  exported : bool;
  imported : bool;
}

type public_define = {
  name : string;
  key : string;
  formals : string list;
  requires_call : bool;
  body_signature : string;
}

type t = {
  source_uri : string;
  source_file : string option;
  content_hash : string;
  compool_name : string option;
  exported_symbols : public_symbol list;
  exported_types : public_symbol list;
  imported_compools : string list;
  icopy_targets : string list;
  define_public_macros : public_define list;
  public_signature_hash : string;
  conservative : bool;
  reasons : string list;
}

val of_document : Document.t -> t
val public_signature_unchanged : t -> t -> bool
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> t option
