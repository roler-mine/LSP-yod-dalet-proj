(* Module overview: Extracts symbol classifications, declaration roles, and type display metadata. *)

module T = Lsp.Types

type jovial_external_kind =
  | ExternalLocal
  | ExternalDef
  | ExternalRef
  | ExternalSystem

type jovial_decl_role =
  | RealDeclaration
  | ExternalDefinition
  | ExternalReferenceImport
  | CompoolImport
  | UsageReference
  | TypeUse
  | MacroDefinition
  | MacroUse

type jovial_symbol_kind =
  | JovialProgram
  | JovialModule
  | JovialCompool
  | JovialCompoolImport
  | JovialItem
  | JovialTable
  | JovialBlock
  | JovialOverlay
  | JovialType
  | JovialProcedure
  | JovialFunction
  | JovialParameter
  | JovialField
  | JovialLabel
  | JovialDefine
  | JovialConstantItem
  | JovialConstantTable
  | JovialStatusConstant
  | JovialBuiltinType
  | JovialUnknownSymbol

type jovial_type_origin =
  | BuiltinType
  | UserDefinedType of string
  | InferredType
  | UnknownType

type jovial_type_info = {
  display : string;
  origin : jovial_type_origin;
  resolved_display : string option;
  type_decl_uri : T.DocumentUri.t option;
  type_decl_loc : Ast.Loc.t option;
  explanation : string option;
}

type jovial_symbol_metadata = {
  jovial_kind : jovial_symbol_kind;
  external_kind : jovial_external_kind;
  decl_role : jovial_decl_role;
  type_info : jovial_type_info option;
  storage : Ast.storage option;
  is_constant : bool;
  is_readonly : bool;
  is_inline : bool;
  is_imported : bool;
  is_exported : bool;
  source_keyword : string option;
  has_body : bool option;
}

let unknown_type_info =
  {
    display = "unknown";
    origin = UnknownType;
    resolved_display = None;
    type_decl_uri = None;
    type_decl_loc = None;
    explanation = None;
  }

let default_metadata =
  {
    jovial_kind = JovialUnknownSymbol;
    external_kind = ExternalLocal;
    decl_role = RealDeclaration;
    type_info = None;
    storage = None;
    is_constant = false;
    is_readonly = false;
    is_inline = false;
    is_imported = false;
    is_exported = false;
    source_keyword = None;
    has_body = None;
  }

let system_subroutine_metadata =
  {
    default_metadata with
    jovial_kind = JovialProcedure;
    external_kind = ExternalSystem;
    decl_role = RealDeclaration;
    source_keyword = Some "SYSTEM";
    has_body = Some false;
  }

let external_kind_of_ast = function
  | Ast.LocalDecl -> ExternalLocal
  | Ast.DefDecl -> ExternalDef
  | Ast.RefDecl -> ExternalRef

let decl_role_of_external_kind = function
  | ExternalLocal | ExternalSystem -> RealDeclaration
  | ExternalDef -> ExternalDefinition
  | ExternalRef -> ExternalReferenceImport

let storage_label = function
  | Ast.Automatic -> "automatic"
  | Ast.Static -> "static"
  | Ast.External -> "external"

let external_label = function
  | ExternalLocal -> "local"
  | ExternalDef -> "external DEF"
  | ExternalRef -> "external REF import"
  | ExternalSystem -> "system/built-in"

let decl_role_label = function
  | RealDeclaration -> "local"
  | ExternalDefinition -> "external DEF"
  | ExternalReferenceImport -> "external REF import"
  | CompoolImport -> "COMPOOL import"
  | UsageReference -> "usage reference"
  | TypeUse -> "type use"
  | MacroDefinition -> "macro definition"
  | MacroUse -> "macro use"

let symbol_kind_label = function
  | JovialProgram -> "main program module"
  | JovialModule -> "module"
  | JovialCompool -> "COMPOOL module"
  | JovialCompoolImport -> "COMPOOL import"
  | JovialItem -> "item"
  | JovialTable -> "table"
  | JovialBlock -> "block"
  | JovialOverlay -> "overlay declaration"
  | JovialType -> "type"
  | JovialProcedure -> "procedure"
  | JovialFunction -> "function"
  | JovialParameter -> "parameter"
  | JovialField -> "field"
  | JovialLabel -> "label"
  | JovialDefine -> "DEFINE macro"
  | JovialConstantItem -> "constant item"
  | JovialConstantTable -> "constant table"
  | JovialStatusConstant -> "status constant"
  | JovialBuiltinType -> "built-in type"
  | JovialUnknownSymbol -> "symbol"

let symbol_kind_summary = function
  | JovialProgram -> "JOVIAL main program module"
  | JovialModule -> "JOVIAL module"
  | JovialCompool -> "JOVIAL COMPOOL module"
  | JovialCompoolImport -> "JOVIAL COMPOOL import"
  | JovialItem -> "JOVIAL item"
  | JovialTable -> "JOVIAL table"
  | JovialBlock -> "JOVIAL block"
  | JovialOverlay -> "JOVIAL overlay declaration"
  | JovialType -> "JOVIAL type"
  | JovialProcedure -> "JOVIAL procedure"
  | JovialFunction -> "JOVIAL function"
  | JovialParameter -> "JOVIAL parameter"
  | JovialField -> "JOVIAL table/block field"
  | JovialLabel -> "JOVIAL label"
  | JovialDefine -> "JOVIAL DEFINE macro"
  | JovialConstantItem -> "JOVIAL constant item"
  | JovialConstantTable -> "JOVIAL constant table"
  | JovialStatusConstant -> "JOVIAL status constant"
  | JovialBuiltinType -> "JOVIAL built-in type"
  | JovialUnknownSymbol -> "JOVIAL symbol"

let metadata_summary (m : jovial_symbol_metadata) : string =
  match (m.external_kind, m.jovial_kind) with
  | ExternalDef, JovialProcedure -> "JOVIAL external DEF procedure"
  | ExternalDef, JovialFunction -> "JOVIAL external DEF function"
  | ExternalRef, JovialProcedure -> "JOVIAL external REF procedure import"
  | ExternalRef, JovialFunction -> "JOVIAL external REF function import"
  | ExternalDef, _ ->
      Printf.sprintf "JOVIAL external DEF %s"
        (symbol_kind_label m.jovial_kind)
  | ExternalRef, _ ->
      Printf.sprintf "JOVIAL external REF %s import"
        (symbol_kind_label m.jovial_kind)
  | ExternalSystem, _ ->
      Printf.sprintf "JOVIAL system %s" (symbol_kind_label m.jovial_kind)
  | ExternalLocal, _ -> symbol_kind_summary m.jovial_kind

let is_external_ref (m : jovial_symbol_metadata) =
  match m.external_kind with ExternalRef -> true | _ -> false

let is_external_def (m : jovial_symbol_metadata) =
  match m.external_kind with ExternalDef -> true | _ -> false

let is_proc_like = function
  | JovialProcedure | JovialFunction -> true
  | _ -> false

let is_real_declaration (m : jovial_symbol_metadata) =
  not (is_external_ref m)

let has_real_implementation (m : jovial_symbol_metadata) =
  is_proc_like m.jovial_kind
  && (not (is_external_ref m))
  && match m.has_body with Some true -> true | _ -> false

let keyword_of_data_kind = function
  | Ast.DataItem -> "ITEM"
  | Ast.DataTable -> "TABLE"
  | Ast.DataBlock -> "BLOCK"
  | Ast.DataUnknown -> "ITEM"

let jovial_kind_of_data_kind ~is_constant = function
  | Ast.DataTable ->
      if is_constant then JovialConstantTable else JovialTable
  | Ast.DataBlock -> JovialBlock
  | Ast.DataItem | Ast.DataUnknown ->
      if is_constant then JovialConstantItem else JovialItem

let literal_to_display = function
  | Ast.LInt s -> s
  | Ast.LFloat s -> s
  | Ast.LBit { raw; _ } -> raw
  | Ast.LString s -> "'" ^ s ^ "'"
  | Ast.LChar c -> Printf.sprintf "'%c'" c
  | Ast.LBool b -> if b then "TRUE" else "FALSE"
  | Ast.LNull -> "NULL"

let binop_display = function
  | Ast.BAdd -> "+"
  | Ast.BSub -> "-"
  | Ast.BMul -> "*"
  | Ast.BDiv -> "/"
  | Ast.BMod -> "MOD"
  | Ast.BPow -> "**"
  | Ast.BAnd -> "AND"
  | Ast.BOr -> "OR"
  | Ast.BBitAnd -> "&"
  | Ast.BBitOr -> "|"
  | Ast.BBitXor -> "XOR"
  | Ast.BEqv -> "EQV"
  | Ast.BShl -> "<<"
  | Ast.BShr -> ">>"
  | Ast.BEq -> "="
  | Ast.BNe -> "<>"
  | Ast.BLt -> "<"
  | Ast.BLe -> "<="
  | Ast.BGt -> ">"
  | Ast.BGe -> ">="

let unop_display = function
  | Ast.UPlus -> "+"
  | Ast.UMinus -> "-"
  | Ast.UNot -> "NOT "
  | Ast.UBitNot -> "~"

let rec expr_display (e : Ast.expr Ast.node) =
  match e.v with
  | Ast.EName id -> id.v
  | Ast.ELit lit -> literal_to_display lit
  | Ast.EParen x -> "(" ^ expr_display x ^ ")"
  | Ast.EUnop { op; rhs } -> unop_display op ^ expr_display rhs
  | Ast.EBinop { op; lhs; rhs } ->
      expr_display lhs ^ " " ^ binop_display op ^ " " ^ expr_display rhs
  | Ast.ECall { callee; args } ->
      callee.v ^ "("
      ^ (args |> List.map expr_display |> String.concat ", ")
      ^ ")"
  | Ast.EIndex { base; index } ->
      expr_display base ^ "("
      ^ (index |> List.map expr_display |> String.concat ", ")
      ^ ")"
  | Ast.EField { base; field } -> expr_display base ^ "." ^ field.v
  | Ast.EConvert { ty; expr } -> type_display ty ^ " " ^ expr_display expr
  | Ast.EPreset { base; items } ->
      expr_display base ^ "("
      ^ (items |> List.map expr_display |> String.concat ", ")
      ^ ")"
  | Ast.EOmitted -> ""
  | Ast.ERepeat { count; items } ->
      expr_display count ^ "*("
      ^ (items |> List.map expr_display |> String.concat ", ")
      ^ ")"
  | Ast.EPositioned { indexes; values } ->
      "POS("
      ^ (indexes |> List.map expr_display |> String.concat ", ")
      ^ "):"
      ^ (values |> List.map expr_display |> String.concat ", ")
  | Ast.ERange { lo; hi } -> expr_display lo ^ ":" ^ expr_display hi
  | Ast.EAt { field; ptr } -> expr_display field ^ " @ " ^ expr_display ptr
  | Ast.EDeref { ptr } -> "@ " ^ expr_display ptr
  | Ast.EError _ | Ast.EMissing _ -> "?"

and scalar_base_name = function
  | Ast.ScalarUnsigned -> "U"
  | Ast.ScalarSigned -> "S"
  | Ast.ScalarFloat -> "F"
  | Ast.ScalarFixed -> "A"
  | Ast.ScalarBit -> "B"
  | Ast.ScalarChar -> "C"

and round_mode_name = function Ast.Round -> "R" | Ast.Truncate -> "T"

and type_display (t : Ast.type_expr Ast.node) =
  match t.v with
  | Ast.TName id -> id.v
  | Ast.TScalar { base; round; sizes } ->
      let base_text = scalar_base_name base in
      let head =
        match round with
        | None -> base_text
        | Some mode -> base_text ^ "," ^ round_mode_name mode
      in
      let sep = match base with Ast.ScalarFixed -> "," | _ -> " " in
      let size_text = sizes |> List.map expr_display |> String.concat sep in
      if size_text = "" then head else head ^ " " ^ size_text
  | Ast.TPointer inner -> "P " ^ type_display inner
  | Ast.TSpecifiedTable { elem; dims; kind } ->
      let dim_text =
        match dims with
        | [] -> ""
        | _ ->
            "(" ^ (dims |> List.map expr_display |> String.concat ",") ^ ")"
      in
      let kind_text =
        match kind with
        | Ast.SpecTableW entry_size -> "W " ^ expr_display entry_size
        | Ast.SpecTableV None -> "V"
        | Ast.SpecTableV (Some entry_size) -> "V " ^ expr_display entry_size
      in
      "SPECIFIED TABLE" ^ dim_text ^ " " ^ kind_text ^ " "
      ^ type_display elem
  | Ast.TArray { elem; dims } -> (
      match elem.v with
      | Ast.TName id
        when
          let k = String.uppercase_ascii id.v in
          k = "U" || k = "S" || k = "F" || k = "A" || k = "B" || k = "C" ->
          let sep =
            match String.uppercase_ascii id.v with "A" -> "," | _ -> " "
          in
          id.v ^ " " ^ (dims |> List.map expr_display |> String.concat sep)
      | _ ->
          type_display elem ^ "("
          ^ (dims |> List.map expr_display |> String.concat ",")
          ^ ")")
  | Ast.TStatus values ->
      "STATUS ("
      ^ (values
        |> List.map (fun (sv : Ast.status_value Ast.node) ->
               "V(" ^ sv.v.sv_name.v ^ ")")
        |> String.concat ", ")
      ^ ")"
  | Ast.TRecord _ -> "BLOCK"
  | Ast.TFunc _ -> "PROC"

let int_literal_expr (e : Ast.expr Ast.node) =
  match e.v with
  | Ast.ELit (Ast.LInt s) -> (
      try
        ignore (int_of_string s);
        true
      with _ -> false)
  | _ -> false

let builtin_size_name name =
  let key = String.uppercase_ascii (String.trim name) in
  match key with "U" | "S" | "W" | "F" | "A" | "B" | "C" -> true | _ -> false

let rec has_non_integer_builtin_size (t : Ast.type_expr Ast.node) =
  match t.v with
  | Ast.TScalar { sizes; _ } ->
      List.exists (fun dim -> not (int_literal_expr dim)) sizes
  | Ast.TArray { elem = { v = Ast.TName id; _ }; dims }
    when builtin_size_name id.v ->
      List.exists (fun dim -> not (int_literal_expr dim)) dims
  | Ast.TArray { elem; _ } | Ast.TPointer elem ->
      has_non_integer_builtin_size elem
  | Ast.TSpecifiedTable { elem; dims; kind } ->
      List.exists (fun dim -> not (int_literal_expr dim)) dims
      || (match kind with
         | Ast.SpecTableW entry_size
         | Ast.SpecTableV (Some entry_size) ->
             not (int_literal_expr entry_size)
         | Ast.SpecTableV None -> false)
      || has_non_integer_builtin_size elem
  | Ast.TStatus _ -> false
  | Ast.TRecord fields ->
      List.exists
        (fun (field : Ast.field_decl Ast.node) ->
          has_non_integer_builtin_size field.v.ftype)
        fields
  | Ast.TFunc { params; returns } ->
      List.exists
        (fun (param : Ast.param Ast.node) ->
          has_non_integer_builtin_size param.v.ptype)
        params
      || Option.fold ~none:false ~some:has_non_integer_builtin_size returns
  | Ast.TName _ -> false

let rich_type_display ?implementation_config (t : Ast.type_expr Ast.node) =
  let implementation_config : Implementation_config.t option =
    implementation_config
  in
  match t.v with
  | Ast.TSpecifiedTable _ -> type_display t
  | _ when has_non_integer_builtin_size t -> type_display t
  | _ ->
      let ty = Jovial_type.of_ast_type_expr (Jovial_type.empty_type_env ()) t in
      let display =
        match implementation_config with
        | None -> Jovial_type.display ty
        | Some config -> Jovial_type.display_with_config config ty
      in
      match display with
      | "unknown" -> type_display t
      | display -> display

let builtin_type_details ?implementation_config name dims =
  let implementation_config : Implementation_config.t option =
    implementation_config
  in
  let key = String.uppercase_ascii (String.trim name) in
  let dim () =
    match dims with
    | d :: _ -> Some (expr_display d)
    | [] -> None
  in
  match key with
  | "U" ->
      let size = dim () in
      ( "unsigned integer",
        Option.map (fun n -> "unsigned integer, " ^ n ^ " bits") size )
  | "S" ->
      let size = dim () in
      ( "signed integer",
        Option.map
          (fun n -> "signed integer, " ^ n ^ " magnitude bits plus sign")
          size )
  | "F" ->
      let precision =
        match dim () with
        | Some _ as hit -> hit
        | None ->
            Option.bind implementation_config (fun c ->
                Option.map string_of_int c.Implementation_config.float_precision)
      in
      ( "floating",
        Option.map
          (fun n -> "floating type, mantissa precision " ^ n)
          precision )
  | "A" ->
      let parts =
        match dims with
        | [] -> (
            match implementation_config with
            | Some { Implementation_config.fixed_precision = Some n; _ } ->
                [ "?"; string_of_int n ]
            | _ -> [])
        | _ -> dims |> List.map expr_display
      in
      ( "fixed",
        match parts with
        | scale :: fraction :: _ ->
            Some ("fixed type, scale " ^ scale ^ ", fraction " ^ fraction)
        | _ -> Some "fixed type" )
  | "B" ->
      let size = dim () in
      ("bit string", Option.map (fun n -> "bit string, " ^ n ^ " bits") size)
  | "C" ->
      let size = dim () in
      ( "character string",
        Option.map (fun n -> "character string, " ^ n ^ " characters") size )
  | "STATUS" -> ("status", Some "status enumeration/list")
  | "P" -> ("pointer", Some "pointer type")
  | _ -> ("built-in", None)

let is_builtin_type_name name =
  let key = String.uppercase_ascii (String.trim name) in
  match key with
  | "U" | "S" | "F" | "A" | "B" | "C" | "STATUS" | "P" -> true
  | _ -> false

let type_info_of_type_expr ?implementation_config
    (t : Ast.type_expr Ast.node) : jovial_type_info =
  let implementation_config : Implementation_config.t option =
    implementation_config
  in
  let display = rich_type_display ?implementation_config t in
  match t.v with
  | Ast.TScalar { base; sizes; _ } ->
      let _, explanation =
        builtin_type_details ?implementation_config (scalar_base_name base) sizes
      in
      {
        display;
        origin = BuiltinType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation;
      }
  | Ast.TName id when is_builtin_type_name id.v ->
      let cls, explanation =
        builtin_type_details ?implementation_config id.v []
      in
      {
        display;
        origin = BuiltinType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation = Some (match explanation with Some e -> e | None -> cls);
      }
  | Ast.TName id ->
      {
        display;
        origin = UserDefinedType id.v;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation = None;
      }
  | Ast.TPointer inner -> (
      match inner.v with
      | Ast.TName id ->
          {
            display;
            origin = BuiltinType;
            resolved_display = None;
            type_decl_uri = None;
            type_decl_loc = None;
            explanation = Some ("typed pointer to " ^ id.v);
          }
      | _ ->
          {
            display;
            origin = BuiltinType;
            resolved_display = None;
            type_decl_uri = None;
            type_decl_loc = None;
            explanation = Some "pointer type";
          })
  | Ast.TArray { elem = { v = Ast.TName id; _ }; dims }
    when is_builtin_type_name id.v ->
      let _, explanation =
        builtin_type_details ?implementation_config id.v dims
      in
      {
        display;
        origin = BuiltinType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation;
      }
  | Ast.TArray _ ->
      {
        display;
        origin = InferredType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation = Some "table type";
      }
  | Ast.TSpecifiedTable { kind; _ } ->
      let entry_size =
        match kind with
        | Ast.SpecTableW entry_size
        | Ast.SpecTableV (Some entry_size) ->
            Some (expr_display entry_size)
        | Ast.SpecTableV None -> None
      in
      {
        display;
        origin = InferredType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation =
          Some
            (match entry_size with
            | Some size -> "specified table, entry size " ^ size
            | None -> "specified table");
      }
  | Ast.TStatus values ->
      {
        display;
        origin = BuiltinType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation =
          Some
            (Printf.sprintf "status enumeration/list with %d %s"
               (List.length values)
               (if List.length values = 1 then "value" else "values"));
      }
  | Ast.TRecord _ ->
      {
        display;
        origin = InferredType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation = Some "block or record type";
      }
  | Ast.TFunc _ ->
      {
        display;
        origin = InferredType;
        resolved_display = None;
        type_decl_uri = None;
        type_decl_loc = None;
        explanation = Some "procedure signature";
      }
