(* Module overview: Typechecker for Jovial expressions, declarations, calls, and assignments. *)

module T = Lsp.Types
module JT = Jovial_type

type issue_kind =
  | ExplicitConversionRequired
  | IncompatibleAssignment
  | InvalidBitOperatorOperand
  | InvalidFixedIntegerMixing
  | InvalidPointerDereference
  | InvalidConversion
  | InvalidArithmeticOperand

type issue = {
  kind : issue_kind;
  loc : Ast.Loc.t;
  severity : T.DiagnosticSeverity.t;
  message : string;
}

type expression_result = {
  ty : JT.t;
  issues : issue list;
}

let diagnostic (issue : issue) : T.Diagnostic.t =
  Lsp_conv.diagnostic ~severity:issue.severity ~source:"semantic"
    ~message:issue.message issue.loc

let issue ?(severity = T.DiagnosticSeverity.Error) kind loc message =
  { kind; loc; severity; message }

let warning kind loc message =
  issue ~severity:T.DiagnosticSeverity.Warning kind loc message

let display = JT.display

let rec scalarize = function
  | JT.Table { entry; _ } -> scalarize entry
  | ty -> ty

let is_unknown = function JT.Unknown | JT.Named _ -> true | _ -> false

let is_integer = function JT.Integer _ -> true | _ -> false
let is_float = function JT.Float _ -> true | _ -> false
let is_fixed = function JT.Fixed _ -> true | _ -> false
let is_bit = function JT.BitString _ -> true | _ -> false

let is_numeric ty = is_integer ty || is_float ty || is_fixed ty

let option_equal lhs rhs =
  match (lhs, rhs) with
  | Some lhs, Some rhs -> lhs = rhs
  | _ -> true

let literal_type = function
  | Ast.LInt s ->
      let upper = String.uppercase_ascii (String.trim s) in
      if
        String.length upper >= 3
        && String.contains upper '\''
        && String.contains upper 'B'
      then JT.BitString { bits = None }
      else JT.Integer { kind = JT.Signed; bits = None }
  | Ast.LFloat _ -> JT.Float { precision = None }
  | Ast.LBit { bead_size; beads; _ } ->
      JT.BitString { bits = Some (bead_size * String.length beads) }
  | Ast.LString s -> JT.CharString { chars = Some (String.length s) }
  | Ast.LChar _ -> JT.CharString { chars = Some 1 }
  | Ast.LBool _ -> JT.BitString { bits = Some 1 }
  | Ast.LNull -> JT.Pointer { target = None; typed = false }

let same_shape lhs rhs = display lhs = display rhs

let conversion_hint lhs =
  Printf.sprintf "Hint: wrap the expression with (* %s *) when this conversion \
                  is intentional."
    (display lhs)

let assignment_issues ~(lhs : JT.t) ~(rhs : JT.t) ~(loc : Ast.Loc.t) :
    issue list =
  let lhs = scalarize lhs in
  let rhs = scalarize rhs in
  if is_unknown lhs || is_unknown rhs || same_shape lhs rhs then []
  else
    match (lhs, rhs) with
    | JT.Integer _, (JT.Float _ | JT.Fixed _)
    | JT.Float _, (JT.Integer _ | JT.Fixed _)
    | JT.Fixed _, (JT.Integer _ | JT.Float _) ->
        [
          warning ExplicitConversionRequired loc
            (Printf.sprintf
               "Explicit conversion required: cannot assign %s to %s without \
                a conversion. %s"
               (display rhs) (display lhs) (conversion_hint lhs));
        ]
    | JT.BitString { bits = lhs_bits }, JT.BitString { bits = rhs_bits }
      when not (option_equal lhs_bits rhs_bits) ->
        [
          issue IncompatibleAssignment loc
            (Printf.sprintf
               "Incompatible assignment: cannot assign %s to %s because the \
                bit lengths differ."
               (display rhs) (display lhs));
        ]
    | JT.CharString { chars = lhs_chars }, JT.CharString { chars = rhs_chars }
      when not (option_equal lhs_chars rhs_chars) ->
        [
          issue IncompatibleAssignment loc
            (Printf.sprintf
               "Incompatible assignment: cannot assign %s to %s because the \
                character lengths differ."
               (display rhs) (display lhs));
        ]
    | JT.Pointer { target = Some lhs_target; _ },
      JT.Pointer { target = Some rhs_target; _ }
      when not (JT.compatible ~lhs:lhs_target ~rhs:rhs_target) ->
        [
          issue IncompatibleAssignment loc
            (Printf.sprintf
               "Incompatible assignment: cannot assign %s to %s because the \
                pointer target types differ."
               (display rhs) (display lhs));
        ]
    | _ when JT.compatible ~lhs ~rhs -> []
    | _ ->
        [
          issue IncompatibleAssignment loc
            (Printf.sprintf "Incompatible assignment: cannot assign %s to %s."
               (display rhs) (display lhs));
        ]

let conversion_allowed ~(target : JT.t) ~(source : JT.t) : bool =
  let target = scalarize target in
  let source = scalarize source in
  is_unknown target || is_unknown source
  || same_shape target source
  ||
  match (target, source) with
  | lhs, rhs when is_numeric lhs && is_numeric rhs -> true
  | JT.BitString _, JT.BitString _ -> true
  | JT.BitString _, JT.Integer _ | JT.Integer _, JT.BitString _ -> true
  | JT.CharString _, JT.CharString _ -> true
  | JT.Pointer _, JT.Pointer _ -> true
  | _ -> false

let conversion_issues ~(target : JT.t) ~(source : JT.t) ~(loc : Ast.Loc.t) :
    issue list =
  if conversion_allowed ~target ~source then []
  else
    [
      issue InvalidConversion loc
        (Printf.sprintf
           "Invalid explicit conversion: cannot convert %s to %s."
           (display source) (display target));
    ]

let integer_bits lhs rhs =
  match (lhs, rhs) with
  | Some lhs, Some rhs -> Some (max lhs rhs)
  | Some bits, None | None, Some bits -> Some bits
  | None, None -> None

let integer_result lhs rhs =
  match (lhs, rhs) with
  | JT.Integer { kind = lhs_kind; bits = lhs_bits },
    JT.Integer { kind = rhs_kind; bits = rhs_bits } ->
      let kind =
        match (lhs_kind, rhs_kind) with
        | JT.Unsigned, JT.Unsigned -> JT.Unsigned
        | _ -> JT.Signed
      in
      JT.Integer { kind; bits = integer_bits lhs_bits rhs_bits }
  | _ -> JT.Integer { kind = JT.Signed; bits = None }

let fixed_integer_issue loc lhs rhs =
  warning InvalidFixedIntegerMixing loc
    (Printf.sprintf
       "Fixed/integer mixing is target-sensitive: %s with %s should use an \
        explicit conversion until implementation-specific rules are known."
       (display lhs) (display rhs))

let numeric_result loc lhs rhs =
  match (lhs, rhs) with
  | JT.Float _, _ | _, JT.Float _ -> (JT.Float { precision = None }, [])
  | JT.Fixed _, JT.Integer _ | JT.Integer _, JT.Fixed _ ->
      (JT.Fixed { scale = None; fraction = None }, [ fixed_integer_issue loc lhs rhs ])
  | JT.Fixed _, JT.Fixed _ -> (JT.Fixed { scale = None; fraction = None }, [])
  | JT.Integer _, JT.Integer _ -> (integer_result lhs rhs, [])
  | _ -> (JT.Unknown, [])

let bit_result loc lhs rhs =
  match (lhs, rhs) with
  | JT.BitString { bits = lhs_bits }, JT.BitString { bits = rhs_bits } ->
      if option_equal lhs_bits rhs_bits then
        (JT.BitString { bits = (match lhs_bits with Some _ -> lhs_bits | None -> rhs_bits) }, [])
      else
        ( JT.BitString { bits = None },
          [
            issue InvalidBitOperatorOperand loc
              (Printf.sprintf
                 "Invalid bit operator operand: %s and %s have incompatible \
                  bit lengths."
                 (display lhs) (display rhs));
          ] )
  | JT.Integer _, JT.Integer _ -> (integer_result lhs rhs, [])
  | JT.BitString _, other | other, JT.BitString _ ->
      ( JT.Unknown,
        [
          issue InvalidBitOperatorOperand loc
            (Printf.sprintf
               "Invalid bit operator operand: expected bit strings or integers, \
                got %s and %s."
               (display lhs) (display other));
        ] )
  | _ -> (JT.Unknown, [])

let relation_result loc lhs rhs =
  if is_unknown lhs || is_unknown rhs then (JT.BitString { bits = Some 1 }, [])
  else if (is_numeric lhs && is_numeric rhs) || JT.compatible ~lhs ~rhs then
    (JT.BitString { bits = Some 1 }, [])
  else
    ( JT.BitString { bits = Some 1 },
      [
        issue IncompatibleAssignment loc
          (Printf.sprintf "Cannot compare %s with %s." (display lhs) (display rhs));
      ] )

let unary_result ~(op : Ast.unop) ~(rhs : JT.t) ~(loc : Ast.Loc.t) :
    expression_result =
  let rhs = scalarize rhs in
  if is_unknown rhs then { ty = JT.Unknown; issues = [] }
  else
    match op with
    | Ast.UPlus | Ast.UMinus ->
        if is_numeric rhs then { ty = rhs; issues = [] }
        else
          {
            ty = JT.Unknown;
            issues =
              [
                issue InvalidArithmeticOperand loc
                  (Printf.sprintf
                     "Invalid arithmetic operand: expected numeric value, got \
                      %s."
                     (display rhs));
              ];
          }
    | Ast.UNot | Ast.UBitNot ->
        if is_bit rhs || is_integer rhs then { ty = rhs; issues = [] }
        else
          {
            ty = JT.Unknown;
            issues =
              [
                issue InvalidBitOperatorOperand loc
                  (Printf.sprintf
                     "Invalid bit operator operand: expected bit string or \
                      integer, got %s."
                     (display rhs));
              ];
          }

let binary_result ~(op : Ast.binop) ~(lhs : JT.t) ~(rhs : JT.t)
    ~(loc : Ast.Loc.t) : expression_result =
  let lhs = scalarize lhs in
  let rhs = scalarize rhs in
  if is_unknown lhs || is_unknown rhs then { ty = JT.Unknown; issues = [] }
  else
    match op with
    | Ast.BAdd | Ast.BSub | Ast.BMul | Ast.BDiv | Ast.BMod | Ast.BPow ->
        if is_numeric lhs && is_numeric rhs then
          let ty, issues = numeric_result loc lhs rhs in
          { ty; issues }
        else
          {
            ty = JT.Unknown;
            issues =
              [
                issue InvalidArithmeticOperand loc
                  (Printf.sprintf
                     "Invalid arithmetic operand: expected numeric values, got \
                      %s and %s."
                     (display lhs) (display rhs));
              ];
          }
    | Ast.BBitAnd | Ast.BBitOr | Ast.BBitXor | Ast.BAnd | Ast.BOr | Ast.BEqv
      ->
        let ty, issues = bit_result loc lhs rhs in
        { ty; issues }
    | Ast.BShl | Ast.BShr ->
        if (is_bit lhs || is_integer lhs) && is_integer rhs then
          { ty = lhs; issues = [] }
        else
          {
            ty = JT.Unknown;
            issues =
              [
                issue InvalidBitOperatorOperand loc
                  (Printf.sprintf
                     "Invalid bit shift operand: expected bit/integer left \
                      operand and integer shift count, got %s and %s."
                     (display lhs) (display rhs));
              ];
          }
    | Ast.BEq | Ast.BNe | Ast.BLt | Ast.BLe | Ast.BGt | Ast.BGe ->
        let ty, issues = relation_result loc lhs rhs in
        { ty; issues }

let dereference_result ~(ptr : JT.t) ~(loc : Ast.Loc.t) : expression_result =
  let ptr = scalarize ptr in
  match ptr with
  | JT.Unknown | JT.Named _ -> { ty = JT.Unknown; issues = [] }
  | JT.Pointer { target = Some target; typed = true } -> { ty = target; issues = [] }
  | JT.Pointer { target = Some target; typed = false } -> { ty = target; issues = [] }
  | JT.Pointer { target = None; _ } ->
      {
        ty = JT.Unknown;
        issues =
          [
            issue InvalidPointerDereference loc
              "Invalid pointer dereference: pointer target type is unknown.";
          ];
      }
  | other ->
      {
        ty = JT.Unknown;
        issues =
          [
            issue InvalidPointerDereference loc
              (Printf.sprintf
                 "Invalid pointer dereference: expected pointer, got %s."
                 (display other));
          ];
      }
