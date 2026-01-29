module T = Lsp.Types

type position_encoding = [ `UTF8 | `UTF16 ]
type t

val create : ?position_encoding:position_encoding -> unit -> t

val did_open  : t -> T.DidOpenTextDocumentParams.t  -> T.PublishDiagnosticsParams.t
val did_change: t -> T.DidChangeTextDocumentParams.t -> T.PublishDiagnosticsParams.t
val did_close : t -> T.DidCloseTextDocumentParams.t -> T.PublishDiagnosticsParams.t

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

val hover      : t -> T.HoverParams.t      -> T.Hover.t option
val references : t -> T.ReferenceParams.t -> T.Location.t list option
val definition : t -> T.DefinitionParams.t -> T.Locations.t option
