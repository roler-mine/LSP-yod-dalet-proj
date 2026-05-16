(** Module overview: Typechecker for Jovial expressions, declarations, calls, and assignments. *)

module T = Lsp.Types

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
  ty : Jovial_type.t;
  issues : issue list;
}

val diagnostic : issue -> T.Diagnostic.t
val literal_type : Ast.literal -> Jovial_type.t

val assignment_issues :
  lhs:Jovial_type.t -> rhs:Jovial_type.t -> loc:Ast.Loc.t -> issue list

val conversion_issues :
  target:Jovial_type.t -> source:Jovial_type.t -> loc:Ast.Loc.t -> issue list

val unary_result :
  op:Ast.unop -> rhs:Jovial_type.t -> loc:Ast.Loc.t -> expression_result

val binary_result :
  op:Ast.binop ->
  lhs:Jovial_type.t ->
  rhs:Jovial_type.t ->
  loc:Ast.Loc.t ->
  expression_result

val dereference_result : ptr:Jovial_type.t -> loc:Ast.Loc.t -> expression_result
