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
    is_imported = false;
    is_exported = false;
    source_keyword = None;
    has_body = None;
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
  | Ast.LString s -> "'" ^ s ^ "'"
  | Ast.LChar c -> Printf.sprintf "'%c'" c
  | Ast.LBool b -> if b then "TRUE" else "FALSE"

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
  | Ast.ERange { lo; hi } -> expr_display lo ^ ":" ^ expr_display hi
  | Ast.EAt { field; ptr } -> expr_display field ^ " @ " ^ expr_display ptr
  | Ast.EDeref { ptr } -> "@ " ^ expr_display ptr

and type_display (t : Ast.type_expr Ast.node) =
  match t.v with
  | Ast.TName id -> id.v
  | Ast.TPointer inner -> "P " ^ type_display inner
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
  | Ast.TRecord _ -> "BLOCK"
  | Ast.TFunc _ -> "PROC"

let builtin_type_details name dims =
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
      let precision = dim () in
      ( "floating",
        Option.map
          (fun n -> "floating type, mantissa precision " ^ n)
          precision )
  | "A" ->
      let parts = dims |> List.map expr_display in
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

let type_info_of_type_expr (t : Ast.type_expr Ast.node) : jovial_type_info =
  let display = type_display t in
  match t.v with
  | Ast.TName id when is_builtin_type_name id.v ->
      let cls, explanation = builtin_type_details id.v [] in
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
      let _, explanation = builtin_type_details id.v dims in
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
