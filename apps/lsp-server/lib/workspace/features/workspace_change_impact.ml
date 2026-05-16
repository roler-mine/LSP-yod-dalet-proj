(* Module overview: Computes likely downstream effects of edits, renames, and type changes. *)

module Metadata = Workspace_symbol_metadata
open Workspace_nav_model
open Workspace_nav_lookup

let change_impact_for_def ws (d : def) =
  match d.metadata.Metadata.external_kind with
  | Metadata.ExternalRef ->
      "Changing this REF can break module linkage or calls in this file, but \
       the actual implementation is controlled by the matching DEF."
  | Metadata.ExternalDef ->
      "Callers, parameter bindings, return type, external DEF/REF declarations, \
       and COMPOOL or module users may be affected. Run Find References before \
       changing this declaration."
  | _ -> (
      match d.metadata.Metadata.jovial_kind with
      | Metadata.JovialProcedure | Metadata.JovialFunction ->
          "Callers, parameter bindings, return type, external DEF/REF \
           declarations, and COMPOOL or module users may be affected. Run Find \
           References before changing this declaration."
      | Metadata.JovialDefine ->
          "Macro expansion changes can affect every use site, including code \
           that only sees the DEFINE through imported or included declarations. \
           Run Find References before changing this declaration."
      | Metadata.JovialTable | Metadata.JovialBlock | Metadata.JovialOverlay
      | Metadata.JovialConstantTable ->
          "Type, size, layout, or name changes can affect assignments, formulas, \
           table/block layout, COMPOOL users, and external DEF/REF users. Run \
           Find References before changing this declaration."
      | _ -> (
          match jovial_kind_for_def ws d with
          | "define" ->
              "Macro expansion changes can affect every use site, including \
               code that only sees the DEFINE through imported or included \
               declarations. Run Find References before changing this \
               declaration."
          | "table" | "block" ->
              "Type, size, layout, or name changes can affect assignments, \
               formulas, table/block layout, COMPOOL users, and external \
               DEF/REF users. Run Find References before changing this \
               declaration."
          | _ ->
              "Type, size, layout, or name changes can affect assignments, \
               formulas, table entries, COMPOOL users, and external DEF/REF \
               users. Run Find References before changing this declaration."))
