(* Module overview: Maps internal Jovial symbol categories to LSP symbol-kind values. *)

module T = Lsp.Types

open Workspace_nav_model
module Metadata = Workspace_symbol_metadata

let label_of_def_kind (kind : int) : string =
  if kind = sym_kind_module then "module"
  else if kind = sym_kind_type then "type"
  else if kind = sym_kind_field then "field"
  else if kind = sym_kind_func then "procedure"
  else if kind = sym_kind_var then "item"
  else if kind = sym_kind_const then "status value / status constant"
  else "symbol"

let role_of_def_kind (kind : int) : string =
  if kind = sym_kind_module then "separately compiled program unit"
  else if kind = sym_kind_type then "user-defined type description"
  else if kind = sym_kind_field then "table, block, or record field"
  else if kind = sym_kind_func then "executable subroutine"
  else if kind = sym_kind_var then "scalar data object / variable or constant"
  else if kind = sym_kind_const then "status value / status constant or DEFINE"
  else "JOVIAL declaration or symbol"

let lsp_symbol_kind_of_def_kind (kind : int) : T.SymbolKind.t =
  if kind = sym_kind_module then T.SymbolKind.Module
  else if kind = sym_kind_type then T.SymbolKind.Class
  else if kind = sym_kind_field then T.SymbolKind.Field
  else if kind = sym_kind_func then T.SymbolKind.Function
  else if kind = sym_kind_const then T.SymbolKind.Constant
  else T.SymbolKind.Variable

let lsp_symbol_kind_of_metadata
    (metadata : Metadata.jovial_symbol_metadata)
    ~(fallback : T.SymbolKind.t) : T.SymbolKind.t =
  match metadata.Metadata.jovial_kind with
  | Metadata.JovialProgram | Metadata.JovialModule | Metadata.JovialCompool
  | Metadata.JovialCompoolImport | Metadata.JovialBlock ->
      T.SymbolKind.Module
  | Metadata.JovialType | Metadata.JovialBuiltinType -> T.SymbolKind.Class
  | Metadata.JovialField | Metadata.JovialLabel -> T.SymbolKind.Field
  | Metadata.JovialProcedure | Metadata.JovialFunction -> T.SymbolKind.Function
  | Metadata.JovialDefine | Metadata.JovialConstantItem
  | Metadata.JovialConstantTable | Metadata.JovialStatusConstant ->
      T.SymbolKind.Constant
  | Metadata.JovialItem | Metadata.JovialTable | Metadata.JovialOverlay
  | Metadata.JovialParameter ->
      T.SymbolKind.Variable
  | Metadata.JovialUnknownSymbol -> fallback

let lsp_symbol_kind_of_skeleton_kind = function
  | Skeleton_index.Program | Skeleton_index.Module | Skeleton_index.Compool ->
      T.SymbolKind.Module
  | Skeleton_index.Procedure | Skeleton_index.Function -> T.SymbolKind.Function
  | Skeleton_index.Item | Skeleton_index.Table -> T.SymbolKind.Variable
  | Skeleton_index.Block -> T.SymbolKind.Module
  | Skeleton_index.Type -> T.SymbolKind.Class
  | Skeleton_index.Label -> T.SymbolKind.Field
  | Skeleton_index.Define -> T.SymbolKind.Constant
  | Skeleton_index.ExternalDef | Skeleton_index.ExternalRef ->
      T.SymbolKind.Variable

let completion_kind_of_skeleton_kind = function
  | Skeleton_index.Program | Skeleton_index.Module | Skeleton_index.Compool
  | Skeleton_index.Block ->
      T.CompletionItemKind.Module
  | Skeleton_index.Procedure | Skeleton_index.Function -> T.CompletionItemKind.Function
  | Skeleton_index.Type -> T.CompletionItemKind.Class
  | Skeleton_index.Define -> T.CompletionItemKind.Constant
  | _ -> T.CompletionItemKind.Variable

let completion_kind_of_metadata
    (metadata : Metadata.jovial_symbol_metadata)
    ~(fallback : T.CompletionItemKind.t) : T.CompletionItemKind.t =
  match metadata.Metadata.jovial_kind with
  | Metadata.JovialProgram | Metadata.JovialModule | Metadata.JovialCompool
  | Metadata.JovialCompoolImport | Metadata.JovialBlock ->
      T.CompletionItemKind.Module
  | Metadata.JovialType | Metadata.JovialBuiltinType -> T.CompletionItemKind.Class
  | Metadata.JovialProcedure | Metadata.JovialFunction ->
      T.CompletionItemKind.Function
  | Metadata.JovialField -> T.CompletionItemKind.Property
  | Metadata.JovialDefine | Metadata.JovialConstantItem
  | Metadata.JovialConstantTable | Metadata.JovialStatusConstant ->
      T.CompletionItemKind.Constant
  | Metadata.JovialItem | Metadata.JovialTable | Metadata.JovialOverlay
  | Metadata.JovialParameter | Metadata.JovialLabel ->
      T.CompletionItemKind.Variable
  | Metadata.JovialUnknownSymbol -> fallback

let label_of_skeleton_kind = function
  | Skeleton_index.Program | Skeleton_index.Module -> "module"
  | Skeleton_index.Compool -> "compool"
  | Skeleton_index.Procedure -> "procedure"
  | Skeleton_index.Function -> "function"
  | Skeleton_index.Item -> "item"
  | Skeleton_index.Table -> "table"
  | Skeleton_index.Block -> "block"
  | Skeleton_index.Type -> "type"
  | Skeleton_index.Label -> "label"
  | Skeleton_index.Define -> "define"
  | Skeleton_index.ExternalDef -> "external DEF"
  | Skeleton_index.ExternalRef -> "external REF"
