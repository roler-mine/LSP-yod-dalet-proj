type int_kind = Signed | Unsigned

type status_value = {
  name : string;
  loc : Ast.Loc.t option;
  representation : int option;
}

type dim_bound =
  | BoundInt of int
  | BoundStatus of string
  | BoundExpr of Ast.expr Ast.node
  | BoundUnknown

type dim = {
  lower : dim_bound option;
  upper : dim_bound option;
}

type t =
  | Unknown
  | Named of string
  | Integer of { kind : int_kind; bits : int option }
  | Float of { precision : int option }
  | Fixed of {
      scale : int option;
      fraction : int option;
    }
  | BitString of { bits : int option }
  | CharString of { chars : int option }
  | Status of { values : status_value list }
  | Pointer of {
      target : t option;
      typed : bool;
    }
  | Table of {
      dims : dim list;
      entry : t;
    }
  | Block of field list
  | Procedure of proc_signature

and field = {
  name : string;
  key : string;
  ty : t;
  loc : Ast.Loc.t;
}

and proc_signature = {
  params : param list;
  returns : t option;
  use_attr : Ast.proc_use;
}

and param = {
  param_name : string;
  param_key : string;
  mode : Ast.param_mode;
  param_ty : t;
  param_loc : Ast.Loc.t;
}

type type_env = (string, Ast.type_expr Ast.node) Hashtbl.t

let normalize_name name = String.uppercase_ascii (String.trim name)

let empty_type_env () = Hashtbl.create 16

let add_type env name ty =
  let key = normalize_name name in
  if key <> "" then Hashtbl.replace env key ty

let type_env_of_list entries =
  let env = empty_type_env () in
  List.iter (fun (name, ty) -> add_type env name ty) entries;
  env

let int_of_string_opt s = try Some (int_of_string s) with _ -> None

let literal_display = function
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
  | Ast.ELit lit -> literal_display lit
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
  | Ast.EConvert { ty; expr } ->
      display (of_ast_type_expr (empty_type_env ()) ty) ^ " " ^ expr_display expr
  | Ast.EPreset { base; items } ->
      expr_display base ^ "("
      ^ (items |> List.map expr_display |> String.concat ", ")
      ^ ")"
  | Ast.ERange { lo; hi } -> expr_display lo ^ ":" ^ expr_display hi
  | Ast.EAt { field; ptr } -> expr_display field ^ " @ " ^ expr_display ptr
  | Ast.EDeref { ptr } -> "@ " ^ expr_display ptr

and dim_bound_of_expr (e : Ast.expr Ast.node) =
  match e.v with
  | Ast.ELit (Ast.LInt s) -> (
      match int_of_string_opt s with
      | Some n -> BoundInt n
      | None -> BoundExpr e)
  | Ast.EName id -> BoundStatus id.v
  | _ -> BoundExpr e

and dim_of_expr (e : Ast.expr Ast.node) =
  match e.v with
  | Ast.ERange { lo; hi } ->
      { lower = Some (dim_bound_of_expr lo); upper = Some (dim_bound_of_expr hi) }
  | _ -> { lower = None; upper = Some (dim_bound_of_expr e) }

and int_dim_value (e : Ast.expr Ast.node) =
  match e.v with
  | Ast.ELit (Ast.LInt s) -> int_of_string_opt s
  | _ -> None

and int_dim_at dims index =
  match List.nth_opt dims index with Some d -> int_dim_value d | None -> None

and builtin_array_type key dims =
  match key with
  | "U" -> Some (Integer { kind = Unsigned; bits = int_dim_at dims 0 })
  | "S" | "W" -> Some (Integer { kind = Signed; bits = int_dim_at dims 0 })
  | "F" -> Some (Float { precision = int_dim_at dims 0 })
  | "A" ->
      Some
        (Fixed
           { scale = int_dim_at dims 0; fraction = int_dim_at dims 1 })
  | "B" -> Some (BitString { bits = int_dim_at dims 0 })
  | "C" -> Some (CharString { chars = int_dim_at dims 0 })
  | "STATUS" | "V" -> Some (Status { values = [] })
  | _ -> None

and builtin_name_type key =
  match key with
  | "U" -> Some (Integer { kind = Unsigned; bits = None })
  | "S" | "W" -> Some (Integer { kind = Signed; bits = None })
  | "F" -> Some (Float { precision = None })
  | "A" -> Some (Fixed { scale = None; fraction = None })
  | "B" -> Some (BitString { bits = None })
  | "C" -> Some (CharString { chars = None })
  | "STATUS" | "V" -> Some (Status { values = [] })
  | "P" -> Some (Pointer { target = None; typed = false })
  | _ -> None

and of_ast_type_expr env t = of_ast_type_expr_seen env [] t

and of_ast_type_expr_seen env seen (t : Ast.type_expr Ast.node) =
  match t.v with
  | Ast.TName id ->
      let key = normalize_name id.v in
      if key = "" then Unknown
      else (
        match builtin_name_type key with
        | Some ty -> ty
        | None -> (
            match Hashtbl.find_opt env key with
            | Some defn when not (List.mem key seen) ->
                of_ast_type_expr_seen env (key :: seen) defn
            | _ -> Named id.v))
  | Ast.TPointer inner ->
      Pointer
        { target = Some (of_ast_type_expr_seen env seen inner); typed = true }
  | Ast.TStatus values ->
      let int_representation = function
        | Some { Ast.v = Ast.ELit (Ast.LInt raw); _ } -> (
            try Some (int_of_string raw) with _ -> None)
        | _ -> None
      in
      Status
        {
          values =
            List.map
              (fun (sv : Ast.status_value Ast.node) ->
                {
                  name = sv.v.sv_name.v;
                  loc = Some sv.v.sv_name.loc;
                  representation = int_representation sv.v.sv_representation;
                })
              values;
        }
  | Ast.TSpecifiedTable { elem; dims; _ } ->
      Table
        {
          dims = List.map dim_of_expr dims;
          entry = of_ast_type_expr_seen env seen elem;
        }
  | Ast.TArray { elem = ({ v = Ast.TName id; _ } as elem); dims } -> (
      let key = normalize_name id.v in
      match builtin_array_type key dims with
      | Some ty -> ty
      | None ->
          Table
            {
              dims = List.map dim_of_expr dims;
              entry = of_ast_type_expr_seen env seen elem;
            })
  | Ast.TArray { elem; dims } ->
      Table
        {
          dims = List.map dim_of_expr dims;
          entry = of_ast_type_expr_seen env seen elem;
        }
  | Ast.TRecord fields ->
      Block
        (List.map
           (fun (field : Ast.field_decl Ast.node) ->
             {
               name = field.v.fname.v;
               key = normalize_name field.v.fname.v;
               ty = of_ast_type_expr_seen env seen field.v.ftype;
               loc = field.loc;
             })
           fields)
  | Ast.TFunc { params; returns } ->
      Procedure
        {
          params =
            List.map
              (fun (param : Ast.param Ast.node) ->
                {
                  param_name = param.v.pname.v;
                  param_key = normalize_name param.v.pname.v;
                  mode = param.v.pmode;
                  param_ty = of_ast_type_expr_seen env seen param.v.ptype;
                  param_loc = param.loc;
                })
              params;
          returns = Option.map (of_ast_type_expr_seen env seen) returns;
          use_attr = Ast.UseNormal;
        }

and dim_bound_display = function
  | BoundInt n -> string_of_int n
  | BoundStatus name -> name
  | BoundExpr expr -> expr_display expr
  | BoundUnknown -> "?"

and dim_display { lower; upper } =
  match (lower, upper) with
  | None, Some upper -> dim_bound_display upper
  | Some lower, Some upper -> dim_bound_display lower ^ ":" ^ dim_bound_display upper
  | Some lower, None -> dim_bound_display lower ^ ":?"
  | None, None -> "?"

and mode_display = function
  | Ast.In -> "IN"
  | Ast.Out -> "OUT"
  | Ast.InOut -> "INOUT"

and display_opt_suffix prefix = function
  | Some n -> prefix ^ string_of_int n
  | None -> String.trim prefix

and display (ty : t) =
  match ty with
  | Unknown -> "unknown"
  | Named name -> name
  | Integer { kind = Unsigned; bits } -> display_opt_suffix "U " bits
  | Integer { kind = Signed; bits } -> display_opt_suffix "S " bits
  | Float { precision } -> display_opt_suffix "F " precision
  | Fixed { scale; fraction } -> (
      match (scale, fraction) with
      | Some scale, Some fraction ->
          Printf.sprintf "A %d,%d" scale fraction
      | Some scale, None -> Printf.sprintf "A %d" scale
      | None, Some fraction -> Printf.sprintf "A ?,%d" fraction
      | None, None -> "A")
  | BitString { bits } -> display_opt_suffix "B " bits
  | CharString { chars } -> display_opt_suffix "C " chars
  | Status { values = [] } -> "STATUS"
  | Status { values } ->
      "STATUS ("
      ^ (values
        |> List.map (fun (value : status_value) -> value.name)
        |> String.concat ", ")
      ^ ")"
  | Pointer { target = None; _ } -> "P"
  | Pointer { target = Some target; _ } -> "P " ^ display target
  | Table { dims; entry } ->
      let dim_text =
        match dims with
        | [] -> ""
        | _ -> "(" ^ (dims |> List.map dim_display |> String.concat ",") ^ ")"
      in
      "TABLE" ^ dim_text ^ " " ^ display entry
  | Block [] -> "BLOCK"
  | Block fields ->
      let field_text =
        fields
        |> List.map (fun (field : field) -> field.name ^ " " ^ display field.ty)
        |> String.concat "; "
      in
      "BLOCK { " ^ field_text ^ " }"
  | Procedure { params; returns; _ } ->
      let param_text =
        params
        |> List.map (fun param ->
               let mode =
                 match param.mode with Ast.In -> "" | _ -> mode_display param.mode ^ " "
               in
               mode ^ param.param_name ^ ": " ^ display param.param_ty)
        |> String.concat ", "
      in
      let ret_text =
        match returns with Some ret -> " RETURNS " ^ display ret | None -> ""
      in
      "PROC(" ^ param_text ^ ")" ^ ret_text

let rec numeric_like = function
  | Integer _ | Float _ | Fixed _ -> true
  | Table { entry; _ } -> numeric_like entry
  | _ -> false

let compatible_option cmp lhs rhs =
  match (lhs, rhs) with
  | None, _ | _, None -> true
  | Some lhs, Some rhs -> cmp lhs rhs

let rec compatible ~(lhs : t) ~(rhs : t) =
  match (lhs, rhs) with
  | Unknown, _ | _, Unknown -> true
  | Named a, Named b -> normalize_name a = normalize_name b
  | Named _, _ | _, Named _ -> true
  | Integer _, Integer _
  | Float _, Float _
  | Fixed _, Fixed _
  | Status _, Status _ ->
      true
  | lhs, rhs when numeric_like lhs && numeric_like rhs -> true
  | BitString { bits = lhs }, BitString { bits = rhs }
  | CharString { chars = lhs }, CharString { chars = rhs } ->
      compatible_option ( = ) lhs rhs
  | Pointer { target = lhs; _ }, Pointer { target = rhs; _ } ->
      compatible_option (fun lhs rhs -> compatible ~lhs ~rhs) lhs rhs
  | Table { entry = lhs; _ }, Table { entry = rhs; _ } -> compatible ~lhs ~rhs
  | Table { entry; _ }, other | other, Table { entry; _ } ->
      compatible ~lhs:entry ~rhs:other
  | Block _, Block _ -> true
  | Procedure lhs, Procedure rhs ->
      let params_ok =
        match (lhs.params, rhs.params) with
        | [], _ | _, [] -> true
        | lhs_params, rhs_params
          when List.length lhs_params = List.length rhs_params ->
            List.for_all2
              (fun lhs rhs -> compatible ~lhs:lhs.param_ty ~rhs:rhs.param_ty)
              lhs_params rhs_params
        | _ -> false
      in
      params_ok
      && compatible_option
           (fun lhs rhs -> compatible ~lhs ~rhs)
           lhs.returns rhs.returns
  | _ -> false

let conversion_required ~lhs ~rhs =
  compatible ~lhs ~rhs && display lhs <> display rhs

let rec field_type ty name =
  let key = normalize_name name in
  match ty with
  | Block fields -> (
      match List.find_opt (fun field -> field.key = key) fields with
      | Some field -> Some field.ty
      | None -> None)
  | Table { entry; _ } -> field_type entry name
  | _ -> None

let table_entry_type = function Table { entry; _ } -> Some entry | _ -> None

let pointer_target = function
  | Pointer { target; _ } -> target
  | _ -> None
