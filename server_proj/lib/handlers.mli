module T = Lsp.Types

type position_encoding = [ `UTF8 | `UTF16 ]
type t

val create : ?position_encoding:position_encoding -> unit -> t

(* Workspace indexing (optional; safe if never called) *)
val set_workspace_root  : t -> string -> unit
val set_workspace_roots : t -> string list -> unit
val reindex_workspace   : t -> unit

val did_open  : t -> T.DidOpenTextDocumentParams.t  -> T.PublishDiagnosticsParams.t
val did_change: t -> T.DidChangeTextDocumentParams.t -> T.PublishDiagnosticsParams.t
val did_close : t -> T.DidCloseTextDocumentParams.t -> T.PublishDiagnosticsParams.t

val hover      : t -> T.HoverParams.t      -> T.Hover.t option
val definition : t -> T.DefinitionParams.t -> T.Locations.t option

val references_at : t -> uri:T.DocumentUri.t -> pos:T.Position.t -> T.Location.t list
val references : t -> T.ReferenceParams.t -> T.Location.t list option

val document_symbols :
  t -> T.DocumentSymbolParams.t ->
  [ `DocumentSymbol of T.DocumentSymbol.t list
  | `SymbolInformation of T.SymbolInformation.t list
  ] option

val completion :
  t -> T.CompletionParams.t ->
  [ `CompletionList of T.CompletionList.t
  | `List of T.CompletionItem.t list
  ] option

(* Keep Main compiling later even if you haven’t wired it yet. *)
val code_actions_json : t -> uri:T.DocumentUri.t -> Yojson.Safe.t