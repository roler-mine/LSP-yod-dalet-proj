(* Module overview: Jovial compile-time expression evaluation helpers used by semantic checks. *)

module T = Lsp.Types

type ctf_value =
  | CtfInt of int64
  | CtfFloat of string
  | CtfString of string
  | CtfChar of char
  | CtfBool of bool

type ctf_error_kind =
  | UnknownIdentifier of string
  | NonConstantReference of string
  | ParseDamage of Ast.parse_error
  | UnsupportedConstruct of string
  | UnsupportedBuiltin of string
  | TypeMismatch of string
  | DivideByZero
  | Overflow
  | UnsafeFloatComparison

type ctf_error = {
  loc : Ast.Loc.t;
  kind : ctf_error_kind;
  message : string;
}

type ctf_result = Known of ctf_value | Unknown of ctf_error list

type binding =
  | Constant of Ast.expr Ast.node
  | Non_constant
  | Impl_param of ctf_value

type env = { bindings : (string, binding) Hashtbl.t }

let normalize_name (name : string) : string =
  String.uppercase_ascii (String.trim name)

let empty_env () = { bindings = Hashtbl.create 64 }

let copy_env (env : env) : env =
  let bindings = Hashtbl.create (max 16 (Hashtbl.length env.bindings * 2)) in
  Hashtbl.iter (fun k v -> Hashtbl.replace bindings k v) env.bindings;
  { bindings }

let add_constant (env : env) (name : string) (expr : Ast.expr Ast.node) : unit =
  let key = normalize_name name in
  if key <> "" then Hashtbl.replace env.bindings key (Constant expr)

let add_non_constant (env : env) (name : string) : unit =
  let key = normalize_name name in
  if key <> "" then Hashtbl.replace env.bindings key Non_constant

let add_impl_param (env : env) (name : string) (value : ctf_value) : unit =
  let key = normalize_name name in
  if key <> "" then Hashtbl.replace env.bindings key (Impl_param value)

let add_impl_int (env : env) (name : string) (value : int option) : unit =
  match value with
  | Some n when n > 0 -> add_impl_param env name (CtfInt (Int64.of_int n))
  | _ -> ()

let add_implementation_config (env : env)
    (config : Implementation_config.t) : unit =
  add_impl_int env "BITSINWORD" config.bits_in_word;
  add_impl_int env "WORDSIZE" config.bits_in_word;
  add_impl_int env "BYTESINWORD" config.bytes_in_word;
  add_impl_int env "BITSINBYTE" (Implementation_config.byte_size_bits config);
  add_impl_int env "BYTESIZE" (Implementation_config.byte_size_bits config);
  add_impl_int env "FLOATPRECISION" config.float_precision;
  add_impl_int env "FIXEDPRECISION" config.fixed_precision;
  add_impl_int env "MAXINTSIZE" config.max_int_size;
  add_impl_int env "MAXBITS" config.max_bits;
  add_impl_int env "MAXBYTES" config.max_bytes

let err (loc : Ast.Loc.t) (kind : ctf_error_kind) (message : string) :
    ctf_result =
  Unknown [ { loc; kind; message } ]

let err_of_kind (loc : Ast.Loc.t) (kind : ctf_error_kind) : ctf_result =
  let message =
    match kind with
    | UnknownIdentifier name ->
        Printf.sprintf "unknown identifier %S" name
    | NonConstantReference name ->
        Printf.sprintf "%S is not a compile-time constant" name
    | ParseDamage damage -> damage.message
    | UnsupportedConstruct what ->
        Printf.sprintf "%s is not supported in compile-time formulas yet" what
    | UnsupportedBuiltin name ->
        Printf.sprintf
          "%s is a compile-time builtin whose target rules are not implemented yet"
          name
    | TypeMismatch what -> what
    | DivideByZero -> "division by zero in compile-time formula"
    | Overflow -> "integer overflow in compile-time formula"
    | UnsafeFloatComparison ->
        "floating-point comparisons are not evaluated exactly in compile-time \
         formulas yet"
  in
  err loc kind message

let unknown_construct loc what = err_of_kind loc (UnsupportedConstruct what)

let combine_unknowns (xs : ctf_result list) : ctf_result =
  let errors =
    List.fold_left
      (fun acc -> function
        | Known _ -> acc
        | Unknown es -> List.rev_append es acc)
      [] xs
    |> List.rev
  in
  Unknown errors

let int64_of_decimal_literal (s : string) : int64 option =
  let cleaned =
    s |> String.trim
    |> String.to_seq
    |> Seq.filter (fun c -> c <> '_')
    |> String.of_seq
  in
  if cleaned = "" || String.contains cleaned '\'' then None
  else Int64.of_string_opt cleaned

let literal_value (loc : Ast.Loc.t) (lit : Ast.literal) : ctf_result =
  match lit with
  | Ast.LInt s -> (
      match int64_of_decimal_literal s with
      | Some n -> Known (CtfInt n)
      | None -> err_of_kind loc (UnsupportedConstruct ("integer literal " ^ s)))
  | Ast.LFloat s -> Known (CtfFloat s)
  | Ast.LBit { raw; _ } ->
      err_of_kind loc (UnsupportedConstruct ("bit literal " ^ raw))
  | Ast.LString s -> Known (CtfString s)
  | Ast.LChar c -> Known (CtfChar c)
  | Ast.LBool b -> Known (CtfBool b)
  | Ast.LNull -> err_of_kind loc (UnsupportedConstruct "NULL literal")

let type_name_of_value = function
  | CtfInt _ -> "integer"
  | CtfFloat _ -> "float"
  | CtfString _ -> "string"
  | CtfChar _ -> "character"
  | CtfBool _ -> "boolean"

let mismatch loc expected got =
  err_of_kind loc
    (TypeMismatch
       (Printf.sprintf "expected %s in compile-time formula, got %s" expected
          (type_name_of_value got)))

let bool_result b = Known (CtfBool b)

let int_result n = Known (CtfInt n)

let checked_div loc lhs rhs =
  if rhs = 0L then err_of_kind loc DivideByZero
  else int_result (Int64.div lhs rhs)

let checked_rem loc lhs rhs =
  if rhs = 0L then err_of_kind loc DivideByZero
  else int_result (Int64.rem lhs rhs)

let int_pow loc base exp =
  if exp < 0L then
    err_of_kind loc
      (TypeMismatch
         "integer exponent must be non-negative in compile-time formula")
  else
    let rec loop acc b e =
      if e = 0L then Known (CtfInt acc)
      else
        let acc = if Int64.rem e 2L = 0L then acc else Int64.mul acc b in
        let e = Int64.div e 2L in
        if e = 0L then Known (CtfInt acc)
        else loop acc (Int64.mul b b) e
    in
    loop 1L base exp

let shift_count loc rhs =
  if rhs < 0L || rhs > 62L then
    Error
      (err_of_kind loc
         (TypeMismatch
            "integer shift count must be between 0 and 62 in compile-time formula"))
  else Ok (Int64.to_int rhs)

let eval_rel loc op lhs rhs =
  let rel cmp =
    match op with
    | Ast.BEq -> bool_result (cmp = 0)
    | Ast.BNe -> bool_result (cmp <> 0)
    | Ast.BLt -> bool_result (cmp < 0)
    | Ast.BLe -> bool_result (cmp <= 0)
    | Ast.BGt -> bool_result (cmp > 0)
    | Ast.BGe -> bool_result (cmp >= 0)
    | _ -> unknown_construct loc "relational operator"
  in
  match (lhs, rhs) with
  | CtfInt a, CtfInt b -> rel (Int64.compare a b)
  | CtfChar a, CtfChar b -> rel (Char.compare a b)
  | CtfString a, CtfString b -> rel (String.compare a b)
  | CtfBool a, CtfBool b -> (
      match op with
      | Ast.BEq -> bool_result (Bool.equal a b)
      | Ast.BNe -> bool_result (not (Bool.equal a b))
      | _ ->
          err_of_kind loc
            (TypeMismatch
               "only equality comparisons are supported for boolean compile-time formulas"))
  | CtfFloat _, CtfFloat _ -> err_of_kind loc UnsafeFloatComparison
  | _, _ ->
      err_of_kind loc
        (TypeMismatch
           (Printf.sprintf "cannot compare %s and %s in compile-time formula"
              (type_name_of_value lhs) (type_name_of_value rhs)))

let unsupported_compile_time_builtins =
  [
    "LOC";
    "NEXT";
    "BIT";
    "BYTE";
    "LBOUND";
    "UBOUND";
    "NWDSEN";
    "WORDSIZE";
    "BYTESIZE";
    "BITSIZE";
  ]

let is_unsupported_compile_time_builtin name =
  List.mem (normalize_name name) unsupported_compile_time_builtins

let rec eval ?env (expr : Ast.expr Ast.node) ~(stack : string list) : ctf_result =
  let eval_child child = eval ?env child ~stack in
  match expr.v with
  | Ast.ELit lit -> literal_value expr.loc lit
  | Ast.EParen inner -> eval_child inner
  | Ast.EName id -> (
      let key = normalize_name id.v in
      if key = "" then err_of_kind id.loc (UnknownIdentifier id.v)
      else if List.mem key stack then
        err_of_kind id.loc
          (UnsupportedConstruct
             (Printf.sprintf "recursive constant reference %S" id.v))
      else
        match env with
        | Some env -> (
            match Hashtbl.find_opt env.bindings key with
            | Some (Impl_param value) -> Known value
            | Some Non_constant -> err_of_kind id.loc (NonConstantReference id.v)
            | Some (Constant rhs) -> eval ~env rhs ~stack:(key :: stack)
            | None ->
                if is_unsupported_compile_time_builtin key then
                  err_of_kind id.loc (UnsupportedBuiltin key)
                else err_of_kind id.loc (UnknownIdentifier id.v))
        | None ->
            if is_unsupported_compile_time_builtin key then
              err_of_kind id.loc (UnsupportedBuiltin key)
            else err_of_kind id.loc (UnknownIdentifier id.v))
  | Ast.EUnop { op; rhs } -> (
      match eval_child rhs with
      | Unknown _ as u -> u
      | Known value -> (
          match (op, value) with
          | Ast.UPlus, CtfInt n -> int_result n
          | Ast.UMinus, CtfInt n -> int_result (Int64.neg n)
          | Ast.UPlus, CtfFloat s -> Known (CtfFloat s)
          | Ast.UMinus, CtfFloat s -> Known (CtfFloat ("-" ^ s))
          | Ast.UNot, CtfBool b -> bool_result (not b)
          | Ast.UBitNot, CtfInt n -> int_result (Int64.lognot n)
          | Ast.UPlus, got | Ast.UMinus, got -> mismatch expr.loc "numeric value" got
          | Ast.UNot, got -> mismatch expr.loc "boolean value" got
          | Ast.UBitNot, got -> mismatch expr.loc "integer value" got))
  | Ast.EBinop { op; lhs; rhs } -> (
      match (eval_child lhs, eval_child rhs) with
      | (Unknown _ as left), (Unknown _ as right) ->
          combine_unknowns [ left; right ]
      | (Unknown _ as u), _ | _, (Unknown _ as u) -> u
      | Known lval, Known rval -> (
          match (op, lval, rval) with
          | Ast.BAdd, CtfInt a, CtfInt b -> int_result (Int64.add a b)
          | Ast.BSub, CtfInt a, CtfInt b -> int_result (Int64.sub a b)
          | Ast.BMul, CtfInt a, CtfInt b -> int_result (Int64.mul a b)
          | Ast.BDiv, CtfInt a, CtfInt b -> checked_div expr.loc a b
          | Ast.BMod, CtfInt a, CtfInt b -> checked_rem expr.loc a b
          | Ast.BPow, CtfInt a, CtfInt b -> int_pow expr.loc a b
          | Ast.BBitAnd, CtfInt a, CtfInt b -> int_result (Int64.logand a b)
          | Ast.BBitOr, CtfInt a, CtfInt b -> int_result (Int64.logor a b)
          | Ast.BBitXor, CtfInt a, CtfInt b -> int_result (Int64.logxor a b)
          | Ast.BShl, CtfInt a, CtfInt b -> (
              match shift_count expr.loc b with
              | Ok count -> int_result (Int64.shift_left a count)
              | Error e -> e)
          | Ast.BShr, CtfInt a, CtfInt b -> (
              match shift_count expr.loc b with
              | Ok count -> int_result (Int64.shift_right a count)
              | Error e -> e)
          | Ast.BAnd, CtfBool a, CtfBool b -> bool_result (a && b)
          | Ast.BOr, CtfBool a, CtfBool b -> bool_result (a || b)
          | Ast.BBitXor, CtfBool a, CtfBool b -> bool_result (a <> b)
          | Ast.BEqv, CtfBool a, CtfBool b -> bool_result (Bool.equal a b)
          | (Ast.BEq | Ast.BNe | Ast.BLt | Ast.BLe | Ast.BGt | Ast.BGe), _, _
            ->
              eval_rel expr.loc op lval rval
          | _, _, _ ->
              err_of_kind expr.loc
                (TypeMismatch
                   (Printf.sprintf
                      "operator is not supported for %s and %s in compile-time formula"
                      (type_name_of_value lval) (type_name_of_value rval)))))
  | Ast.ECall { callee; args } ->
      let key = normalize_name callee.v in
      if is_unsupported_compile_time_builtin key then
        err_of_kind callee.loc (UnsupportedBuiltin key)
      else
        let child_results = List.map eval_child args in
        (match child_results with
        | [] ->
            unknown_construct expr.loc
              (Printf.sprintf "function call %S" callee.v)
        | _ when List.exists (function Unknown _ -> true | Known _ -> false) child_results
          ->
            combine_unknowns child_results
        | _ ->
            unknown_construct expr.loc
              (Printf.sprintf "function call %S" callee.v))
  | Ast.EIndex { base; index } ->
      combine_unknowns
        (eval_child base
        :: unknown_construct expr.loc "indexing"
        :: List.map eval_child index)
  | Ast.EField { base; field = _ } ->
      combine_unknowns
        [ eval_child base; unknown_construct expr.loc "field selection" ]
  | Ast.EConvert { ty = _; expr = inner } ->
      combine_unknowns [ eval_child inner; unknown_construct expr.loc "conversion" ]
  | Ast.EPreset { base; items } ->
      combine_unknowns
        (eval_child base
        :: unknown_construct expr.loc "preset"
        :: List.map eval_child items)
  | Ast.EOmitted -> unknown_construct expr.loc "omitted value"
  | Ast.ERepeat { count; items } ->
      combine_unknowns
        (eval_child count
        :: unknown_construct expr.loc "repeated preset"
        :: List.map eval_child items)
  | Ast.EPositioned { indexes; values } ->
      combine_unknowns
        (unknown_construct expr.loc "positioned preset"
        :: List.map eval_child (indexes @ values))
  | Ast.ERange { lo; hi } ->
      combine_unknowns
        [ eval_child lo; eval_child hi; unknown_construct expr.loc "range" ]
  | Ast.EAt { field; ptr } ->
      combine_unknowns
        [ eval_child field; eval_child ptr; unknown_construct expr.loc "@ access" ]
  | Ast.EDeref { ptr } ->
      combine_unknowns
        [ eval_child ptr; unknown_construct expr.loc "pointer dereference" ]
  | Ast.EError damage | Ast.EMissing damage -> err_of_kind expr.loc (ParseDamage damage)

let eval_expr ?env expr = eval ?env expr ~stack:[]

let int_value = function Known (CtfInt n) -> Some n | _ -> None

let is_diagnostic_worthy_error ?(diagnose_unknown_identifiers = false)
    ?(diagnose_unsupported_constructs = false) (error : ctf_error) : bool =
  match error.kind with
  | NonConstantReference _ | TypeMismatch _ | DivideByZero | Overflow
  | UnsafeFloatComparison ->
      true
  | UnknownIdentifier _ -> diagnose_unknown_identifiers
  | ParseDamage _ -> false
  | UnsupportedConstruct _ -> diagnose_unsupported_constructs
  | UnsupportedBuiltin _ -> false

let is_definitely_non_constant = function
  | Known _ -> false
  | Unknown errors ->
      List.exists
        (fun error ->
          match error.kind with
          | NonConstantReference _ -> true
          | _ -> false)
        errors

let diagnostic_for_required ?(diagnose_unknown_identifiers = false)
    ?(diagnose_unsupported_constructs = false)
    ?(message = "Compile-time constant expression required") result loc :
    T.Diagnostic.t option =
  match result with
  | Known _ -> None
  | Unknown errors -> (
      match
        List.find_opt
          (is_diagnostic_worthy_error ~diagnose_unknown_identifiers
             ~diagnose_unsupported_constructs)
          errors
      with
      | None -> None
      | Some error ->
          Some
            (Lsp_conv.diagnostic ~severity:T.DiagnosticSeverity.Error
               ~source:"semantic"
               ~message:(Printf.sprintf "%s: %s." message error.message)
               loc))
