
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | XOR
    | WITH
    | WHILE
    | WHEN
    | TYPEATOM of (
# 39 "lib/parser.mly"
       (string)
# 19 "lib/parser.ml"
  )
    | TYPE
    | TRUE
    | TO
    | THEN
    | TERM
    | TABLE
    | STRING of (
# 35 "lib/parser.mly"
       (string)
# 30 "lib/parser.ml"
  )
    | STOP
    | STATUS
    | STATIC
    | START
    | STAR
    | SLASH
    | SEMI
    | RPAREN
    | RETURN
    | REP
    | RENT
    | REF
    | REC
    | RBRACK
    | PROGRAM
    | PROC
    | POS
    | PLUS
    | PARALLEL
    | OVERLAY
    | OTHERWISE
    | OR
    | OF
    | NULL
    | NQ
    | NOT
    | NEQ
    | MOD
    | MINUS
    | LT
    | LS
    | LQ
    | LPAREN
    | LIKE
    | LE
    | LBRACK
    | LABEL
    | ITEM
    | INT of (
# 36 "lib/parser.mly"
       (int)
# 73 "lib/parser.ml"
  )
    | INSTANCE
    | INLINE
    | IF
    | IDENT of (
# 34 "lib/parser.mly"
       (string)
# 81 "lib/parser.ml"
  )
    | ICOMPOOL
    | GT
    | GR
    | GQ
    | GOTO
    | GE
    | FUNCTION
    | FOR
    | FLOAT of (
# 37 "lib/parser.mly"
       (float)
# 94 "lib/parser.ml"
  )
    | FALSE
    | FALLTHRU
    | EXIT
    | EQV
    | EQUAL
    | EQ
    | EOF
    | END
    | ELSIF
    | ELSE
    | DIRECTIVE_NAME of (
# 32 "lib/parser.mly"
       (string)
# 109 "lib/parser.ml"
  )
    | DEFINE
    | DEFAULT
    | DEF
    | CONSTANT
    | COMPOOL
    | COMMA
    | COLON
    | CASE
    | BYVAL
    | BYRES
    | BYREF
    | BY
    | BLOCK
    | BEGIN
    | BEAD of (
# 38 "lib/parser.mly"
       (int * string)
# 128 "lib/parser.ml"
  )
    | BANG
    | AND
    | ABORT
  
end

include MenhirBasics

# 3 "lib/parser.mly"
  
  open Ast
  let mk_span = Ast.mk_span
  let mk_loc  = Ast.mk_loc

# 144 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState002 : ('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_state
    (** State 002.
        Stack shape : module_header.
        Start symbol: compilation_unit. *)

  | MenhirState003 : (('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_state
    (** State 003.
        Stack shape : module_header BEGIN.
        Start symbol: compilation_unit. *)

  | MenhirState004 : ((('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 004.
        Stack shape : module_header BEGIN top_items_opt.
        Start symbol: compilation_unit. *)

  | MenhirState005 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_state
    (** State 005.
        Stack shape : WHILE.
        Start symbol: compilation_unit. *)

  | MenhirState009 : (('s, _menhir_box_compilation_unit) _menhir_cell1_NOT, _menhir_box_compilation_unit) _menhir_state
    (** State 009.
        Stack shape : NOT.
        Start symbol: compilation_unit. *)

  | MenhirState010 : (('s, _menhir_box_compilation_unit) _menhir_cell1_MINUS, _menhir_box_compilation_unit) _menhir_state
    (** State 010.
        Stack shape : MINUS.
        Start symbol: compilation_unit. *)

  | MenhirState011 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_state
    (** State 011.
        Stack shape : LPAREN.
        Start symbol: compilation_unit. *)

  | MenhirState014 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 014.
        Stack shape : IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState021 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr_list, _menhir_box_compilation_unit) _menhir_state
    (** State 021.
        Stack shape : expr_list.
        Start symbol: compilation_unit. *)

  | MenhirState022 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr_list, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 022.
        Stack shape : expr_list expr.
        Start symbol: compilation_unit. *)

  | MenhirState023 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_XOR, _menhir_box_compilation_unit) _menhir_state
    (** State 023.
        Stack shape : expr XOR.
        Start symbol: compilation_unit. *)

  | MenhirState024 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_XOR, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 024.
        Stack shape : expr XOR expr.
        Start symbol: compilation_unit. *)

  | MenhirState025 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_STAR, _menhir_box_compilation_unit) _menhir_state
    (** State 025.
        Stack shape : expr STAR.
        Start symbol: compilation_unit. *)

  | MenhirState027 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_SLASH, _menhir_box_compilation_unit) _menhir_state
    (** State 027.
        Stack shape : expr SLASH.
        Start symbol: compilation_unit. *)

  | MenhirState029 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_PLUS, _menhir_box_compilation_unit) _menhir_state
    (** State 029.
        Stack shape : expr PLUS.
        Start symbol: compilation_unit. *)

  | MenhirState030 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_PLUS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 030.
        Stack shape : expr PLUS expr.
        Start symbol: compilation_unit. *)

  | MenhirState031 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MOD, _menhir_box_compilation_unit) _menhir_state
    (** State 031.
        Stack shape : expr MOD.
        Start symbol: compilation_unit. *)

  | MenhirState033 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NQ, _menhir_box_compilation_unit) _menhir_state
    (** State 033.
        Stack shape : expr NQ.
        Start symbol: compilation_unit. *)

  | MenhirState034 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 034.
        Stack shape : expr NQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState035 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MINUS, _menhir_box_compilation_unit) _menhir_state
    (** State 035.
        Stack shape : expr MINUS.
        Start symbol: compilation_unit. *)

  | MenhirState036 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MINUS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 036.
        Stack shape : expr MINUS expr.
        Start symbol: compilation_unit. *)

  | MenhirState037 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NEQ, _menhir_box_compilation_unit) _menhir_state
    (** State 037.
        Stack shape : expr NEQ.
        Start symbol: compilation_unit. *)

  | MenhirState038 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NEQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 038.
        Stack shape : expr NEQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState039 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LT, _menhir_box_compilation_unit) _menhir_state
    (** State 039.
        Stack shape : expr LT.
        Start symbol: compilation_unit. *)

  | MenhirState040 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 040.
        Stack shape : expr LT expr.
        Start symbol: compilation_unit. *)

  | MenhirState041 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LS, _menhir_box_compilation_unit) _menhir_state
    (** State 041.
        Stack shape : expr LS.
        Start symbol: compilation_unit. *)

  | MenhirState042 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 042.
        Stack shape : expr LS expr.
        Start symbol: compilation_unit. *)

  | MenhirState043 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LQ, _menhir_box_compilation_unit) _menhir_state
    (** State 043.
        Stack shape : expr LQ.
        Start symbol: compilation_unit. *)

  | MenhirState044 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 044.
        Stack shape : expr LQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState045 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LE, _menhir_box_compilation_unit) _menhir_state
    (** State 045.
        Stack shape : expr LE.
        Start symbol: compilation_unit. *)

  | MenhirState046 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 046.
        Stack shape : expr LE expr.
        Start symbol: compilation_unit. *)

  | MenhirState047 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GT, _menhir_box_compilation_unit) _menhir_state
    (** State 047.
        Stack shape : expr GT.
        Start symbol: compilation_unit. *)

  | MenhirState048 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 048.
        Stack shape : expr GT expr.
        Start symbol: compilation_unit. *)

  | MenhirState049 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GR, _menhir_box_compilation_unit) _menhir_state
    (** State 049.
        Stack shape : expr GR.
        Start symbol: compilation_unit. *)

  | MenhirState050 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GR, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 050.
        Stack shape : expr GR expr.
        Start symbol: compilation_unit. *)

  | MenhirState051 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GQ, _menhir_box_compilation_unit) _menhir_state
    (** State 051.
        Stack shape : expr GQ.
        Start symbol: compilation_unit. *)

  | MenhirState052 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 052.
        Stack shape : expr GQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState053 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GE, _menhir_box_compilation_unit) _menhir_state
    (** State 053.
        Stack shape : expr GE.
        Start symbol: compilation_unit. *)

  | MenhirState054 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 054.
        Stack shape : expr GE expr.
        Start symbol: compilation_unit. *)

  | MenhirState055 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQUAL, _menhir_box_compilation_unit) _menhir_state
    (** State 055.
        Stack shape : expr EQUAL.
        Start symbol: compilation_unit. *)

  | MenhirState056 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQUAL, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 056.
        Stack shape : expr EQUAL expr.
        Start symbol: compilation_unit. *)

  | MenhirState057 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQ, _menhir_box_compilation_unit) _menhir_state
    (** State 057.
        Stack shape : expr EQ.
        Start symbol: compilation_unit. *)

  | MenhirState058 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 058.
        Stack shape : expr EQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState059 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_AND, _menhir_box_compilation_unit) _menhir_state
    (** State 059.
        Stack shape : expr AND.
        Start symbol: compilation_unit. *)

  | MenhirState060 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_AND, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 060.
        Stack shape : expr AND expr.
        Start symbol: compilation_unit. *)

  | MenhirState061 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OR, _menhir_box_compilation_unit) _menhir_state
    (** State 061.
        Stack shape : expr OR.
        Start symbol: compilation_unit. *)

  | MenhirState062 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OR, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 062.
        Stack shape : expr OR expr.
        Start symbol: compilation_unit. *)

  | MenhirState063 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQV, _menhir_box_compilation_unit) _menhir_state
    (** State 063.
        Stack shape : expr EQV.
        Start symbol: compilation_unit. *)

  | MenhirState064 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQV, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 064.
        Stack shape : expr EQV expr.
        Start symbol: compilation_unit. *)

  | MenhirState065 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 065.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState066 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 066.
        Stack shape : LPAREN expr.
        Start symbol: compilation_unit. *)

  | MenhirState069 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_NOT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 069.
        Stack shape : NOT expr.
        Start symbol: compilation_unit. *)

  | MenhirState070 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 070.
        Stack shape : WHILE expr.
        Start symbol: compilation_unit. *)

  | MenhirState071 : (('s, _menhir_box_compilation_unit) _menhir_cell1_STOP, _menhir_box_compilation_unit) _menhir_state
    (** State 071.
        Stack shape : STOP.
        Start symbol: compilation_unit. *)

  | MenhirState074 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 074.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState076 : (('s, _menhir_box_compilation_unit) _menhir_cell1_RETURN, _menhir_box_compilation_unit) _menhir_state
    (** State 076.
        Stack shape : RETURN.
        Start symbol: compilation_unit. *)

  | MenhirState079 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_state
    (** State 079.
        Stack shape : IF.
        Start symbol: compilation_unit. *)

  | MenhirState080 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 080.
        Stack shape : IF expr.
        Start symbol: compilation_unit. *)

  | MenhirState081 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 081.
        Stack shape : IF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState084 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 084.
        Stack shape : IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState087 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 087.
        Stack shape : IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState090 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 090.
        Stack shape : IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState096 : (('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 096.
        Stack shape : FOR IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState097 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 097.
        Stack shape : FOR IDENT expr.
        Start symbol: compilation_unit. *)

  | MenhirState098 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_state
    (** State 098.
        Stack shape : FOR IDENT expr TO.
        Start symbol: compilation_unit. *)

  | MenhirState099 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 099.
        Stack shape : FOR IDENT expr TO expr.
        Start symbol: compilation_unit. *)

  | MenhirState100 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_BY, _menhir_box_compilation_unit) _menhir_state
    (** State 100.
        Stack shape : expr BY.
        Start symbol: compilation_unit. *)

  | MenhirState101 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_BY, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 101.
        Stack shape : expr BY expr.
        Start symbol: compilation_unit. *)

  | MenhirState102 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 102.
        Stack shape : FOR IDENT expr TO expr by_opt.
        Start symbol: compilation_unit. *)

  | MenhirState109 : (('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_state
    (** State 109.
        Stack shape : CASE.
        Start symbol: compilation_unit. *)

  | MenhirState110 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 110.
        Stack shape : CASE expr.
        Start symbol: compilation_unit. *)

  | MenhirState111 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_state
    (** State 111.
        Stack shape : CASE expr OF.
        Start symbol: compilation_unit. *)

  | MenhirState112 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_state
    (** State 112.
        Stack shape : WHEN.
        Start symbol: compilation_unit. *)

  | MenhirState113 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 113.
        Stack shape : WHEN expr.
        Start symbol: compilation_unit. *)

  | MenhirState115 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_state
    (** State 115.
        Stack shape : WHEN case_labels.
        Start symbol: compilation_unit. *)

  | MenhirState116 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 116.
        Stack shape : WHEN case_labels expr.
        Start symbol: compilation_unit. *)

  | MenhirState117 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_state
    (** State 117.
        Stack shape : WHEN case_labels.
        Start symbol: compilation_unit. *)

  | MenhirState118 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 118.
        Stack shape : WHEN case_labels stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState119 : (('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_state
    (** State 119.
        Stack shape : BEGIN.
        Start symbol: compilation_unit. *)

  | MenhirState120 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_state
    (** State 120.
        Stack shape : TYPE.
        Start symbol: compilation_unit. *)

  | MenhirState124 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 124.
        Stack shape : TYPE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState133 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_status_list, _menhir_box_compilation_unit) _menhir_state
    (** State 133.
        Stack shape : TYPE ident_loc status_list.
        Start symbol: compilation_unit. *)

  | MenhirState136 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 136.
        Stack shape : TYPE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState142 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_state
    (** State 142.
        Stack shape : TABLE.
        Start symbol: compilation_unit. *)

  | MenhirState144 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 144.
        Stack shape : TABLE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState150 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_dim_list, _menhir_box_compilation_unit) _menhir_state
    (** State 150.
        Stack shape : TABLE ident_loc dim_list.
        Start symbol: compilation_unit. *)

  | MenhirState153 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 153.
        Stack shape : TABLE ident_loc table_dims_opt.
        Start symbol: compilation_unit. *)

  | MenhirState154 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_state
    (** State 154.
        Stack shape : TYPE.
        Start symbol: compilation_unit. *)

  | MenhirState156 : (('s, _menhir_box_compilation_unit) _menhir_cell1_COLON, _menhir_box_compilation_unit) _menhir_state
    (** State 156.
        Stack shape : COLON.
        Start symbol: compilation_unit. *)

  | MenhirState158 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 158.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState160 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_state
    (** State 160.
        Stack shape : WITH.
        Start symbol: compilation_unit. *)

  | MenhirState163 : (('s, _menhir_box_compilation_unit) _menhir_cell1_REP, _menhir_box_compilation_unit) _menhir_state
    (** State 163.
        Stack shape : REP.
        Start symbol: compilation_unit. *)

  | MenhirState164 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_REP, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 164.
        Stack shape : REP expr.
        Start symbol: compilation_unit. *)

  | MenhirState169 : (('s, _menhir_box_compilation_unit) _menhir_cell1_POS, _menhir_box_compilation_unit) _menhir_state
    (** State 169.
        Stack shape : POS.
        Start symbol: compilation_unit. *)

  | MenhirState170 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_POS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 170.
        Stack shape : POS expr.
        Start symbol: compilation_unit. *)

  | MenhirState180 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT, _menhir_box_compilation_unit) _menhir_state
    (** State 180.
        Stack shape : DEFAULT.
        Start symbol: compilation_unit. *)

  | MenhirState181 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 181.
        Stack shape : DEFAULT expr.
        Start symbol: compilation_unit. *)

  | MenhirState185 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_cell1_decl_attr_list, _menhir_box_compilation_unit) _menhir_state
    (** State 185.
        Stack shape : WITH decl_attr_list.
        Start symbol: compilation_unit. *)

  | MenhirState190 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 190.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState191 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 191.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt decl_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState192 : (('s, _menhir_box_compilation_unit) _menhir_cell1_REF, _menhir_box_compilation_unit) _menhir_state
    (** State 192.
        Stack shape : REF.
        Start symbol: compilation_unit. *)

  | MenhirState193 : (('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_state
    (** State 193.
        Stack shape : PROC.
        Start symbol: compilation_unit. *)

  | MenhirState194 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 194.
        Stack shape : PROC IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState195 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_state
    (** State 195.
        Stack shape : LPAREN.
        Start symbol: compilation_unit. *)

  | MenhirState199 : (('s, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 199.
        Stack shape : param_mode_opt.
        Start symbol: compilation_unit. *)

  | MenhirState200 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 200.
        Stack shape : param_mode_opt ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState206 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_param_list, _menhir_box_compilation_unit) _menhir_state
    (** State 206.
        Stack shape : LPAREN param_list.
        Start symbol: compilation_unit. *)

  | MenhirState211 : (('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_state
    (** State 211.
        Stack shape : FUNCTION.
        Start symbol: compilation_unit. *)

  | MenhirState212 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 212.
        Stack shape : FUNCTION IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState213 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 213.
        Stack shape : FUNCTION IDENT formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState217 : (('s, _menhir_box_compilation_unit) _menhir_cell1_OVERLAY, _menhir_box_compilation_unit) _menhir_state
    (** State 217.
        Stack shape : OVERLAY.
        Start symbol: compilation_unit. *)

  | MenhirState220 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LABEL, _menhir_box_compilation_unit) _menhir_state
    (** State 220.
        Stack shape : LABEL.
        Start symbol: compilation_unit. *)

  | MenhirState222 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_LABEL, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_state
    (** State 222.
        Stack shape : LABEL ident_list.
        Start symbol: compilation_unit. *)

  | MenhirState224 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_COMMA, _menhir_box_compilation_unit) _menhir_state
    (** State 224.
        Stack shape : ident_list COMMA.
        Start symbol: compilation_unit. *)

  | MenhirState226 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_state
    (** State 226.
        Stack shape : ITEM.
        Start symbol: compilation_unit. *)

  | MenhirState227 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_state
    (** State 227.
        Stack shape : ITEM ident_list.
        Start symbol: compilation_unit. *)

  | MenhirState228 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 228.
        Stack shape : ITEM ident_list type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState231 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 231.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt decl_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState234 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_state
    (** State 234.
        Stack shape : DEFINE.
        Start symbol: compilation_unit. *)

  | MenhirState235 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 235.
        Stack shape : DEFINE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState237 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_EQUAL, _menhir_box_compilation_unit) _menhir_state
    (** State 237.
        Stack shape : DEFINE ident_loc EQUAL.
        Start symbol: compilation_unit. *)

  | MenhirState238 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 238.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState243 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_state
    (** State 243.
        Stack shape : DEF.
        Start symbol: compilation_unit. *)

  | MenhirState246 : (('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_state
    (** State 246.
        Stack shape : BLOCK.
        Start symbol: compilation_unit. *)

  | MenhirState247 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 247.
        Stack shape : BLOCK ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState250 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 250.
        Stack shape : BLOCK ident_loc decl_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState251 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 251.
        Stack shape : BLOCK ident_loc decl_attrs_opt decl_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState252 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 252.
        Stack shape : BLOCK ident_loc decl_attrs_opt decl_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState259 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ABORT, _menhir_box_compilation_unit) _menhir_state
    (** State 259.
        Stack shape : ABORT.
        Start symbol: compilation_unit. *)

  | MenhirState265 : (('s, _menhir_box_compilation_unit) _menhir_cell1_lvalue, _menhir_box_compilation_unit) _menhir_state
    (** State 265.
        Stack shape : lvalue.
        Start symbol: compilation_unit. *)

  | MenhirState266 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_lvalue, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 266.
        Stack shape : lvalue expr.
        Start symbol: compilation_unit. *)

  | MenhirState274 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_block_list_opt _menhir_cell0_END, _menhir_box_compilation_unit) _menhir_state
    (** State 274.
        Stack shape : BEGIN block_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState276 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_block_list, _menhir_box_compilation_unit) _menhir_state
    (** State 276.
        Stack shape : BEGIN block_list.
        Start symbol: compilation_unit. *)

  | MenhirState280 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_state
    (** State 280.
        Stack shape : CASE expr OF case_clauses.
        Start symbol: compilation_unit. *)

  | MenhirState282 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE, _menhir_box_compilation_unit) _menhir_state
    (** State 282.
        Stack shape : CASE expr OF case_clauses OTHERWISE.
        Start symbol: compilation_unit. *)

  | MenhirState283 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 283.
        Stack shape : CASE expr OF case_clauses OTHERWISE stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState291 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 291.
        Stack shape : FOR IDENT expr by_opt.
        Start symbol: compilation_unit. *)

  | MenhirState292 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 292.
        Stack shape : FOR IDENT expr by_opt expr.
        Start symbol: compilation_unit. *)

  | MenhirState297 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_state
    (** State 297.
        Stack shape : IF expr THEN stmt.
        Start symbol: compilation_unit. *)

  | MenhirState298 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_state
    (** State 298.
        Stack shape : IF expr THEN stmt ELSIF.
        Start symbol: compilation_unit. *)

  | MenhirState299 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 299.
        Stack shape : IF expr THEN stmt ELSIF expr.
        Start symbol: compilation_unit. *)

  | MenhirState300 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 300.
        Stack shape : IF expr THEN stmt ELSIF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState302 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ELSE, _menhir_box_compilation_unit) _menhir_state
    (** State 302.
        Stack shape : ELSE.
        Start symbol: compilation_unit. *)

  | MenhirState304 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_state
    (** State 304.
        Stack shape : IF expr THEN stmt elsif_list.
        Start symbol: compilation_unit. *)

  | MenhirState305 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_state
    (** State 305.
        Stack shape : IF expr THEN stmt elsif_list ELSIF.
        Start symbol: compilation_unit. *)

  | MenhirState306 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 306.
        Stack shape : IF expr THEN stmt elsif_list ELSIF expr.
        Start symbol: compilation_unit. *)

  | MenhirState307 : ((((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 307.
        Stack shape : IF expr THEN stmt elsif_list ELSIF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState313 : (((('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 313.
        Stack shape : module_header BEGIN top_items_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState315 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_state
    (** State 315.
        Stack shape : top_items_opt DEF.
        Start symbol: compilation_unit. *)

  | MenhirState316 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_state
    (** State 316.
        Stack shape : top_items_opt DEF PROC.
        Start symbol: compilation_unit. *)

  | MenhirState317 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 317.
        Stack shape : top_items_opt DEF PROC IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState318 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 318.
        Stack shape : top_items_opt DEF PROC ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState319 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 319.
        Stack shape : top_items_opt DEF PROC ident_loc formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState322 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 322.
        Stack shape : top_items_opt DEF PROC ident_loc formal_params_opt use_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState323 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_state
    (** State 323.
        Stack shape : use_attrs_opt BEGIN.
        Start symbol: compilation_unit. *)

  | MenhirState324 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 324.
        Stack shape : use_attrs_opt BEGIN stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState325 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 325.
        Stack shape : use_attrs_opt BEGIN stmt_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState327 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 327.
        Stack shape : top_items_opt DEF PROC ident_loc formal_params_opt use_attrs_opt proc_body_opt.
        Start symbol: compilation_unit. *)

  | MenhirState330 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END _menhir_cell0_proc_end_name_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 330.
        Stack shape : use_attrs_opt proc_body_opt END proc_end_name_opt.
        Start symbol: compilation_unit. *)

  | MenhirState338 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_state
    (** State 338.
        Stack shape : top_items_opt DEF FUNCTION.
        Start symbol: compilation_unit. *)

  | MenhirState339 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 339.
        Stack shape : top_items_opt DEF FUNCTION IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState340 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 340.
        Stack shape : top_items_opt DEF FUNCTION ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState341 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 341.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState342 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 342.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState344 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 344.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt type_spec_opt use_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState345 : ((((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 345.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt type_spec_opt use_attrs_opt proc_body_opt.
        Start symbol: compilation_unit. *)

  | MenhirState351 : (('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 351.
        Stack shape : module_header top_items_opt.
        Start symbol: compilation_unit. *)

  | MenhirState360 : ('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_state
    (** State 360.
        Stack shape : directives_opt DIRECTIVE_NAME.
        Start symbol: compilation_unit. *)

  | MenhirState364 : (('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_state
    (** State 364.
        Stack shape : directives_opt DIRECTIVE_NAME LPAREN.
        Start symbol: compilation_unit. *)

  | MenhirState373 : ((('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_directive_arg_list, _menhir_box_compilation_unit) _menhir_state
    (** State 373.
        Stack shape : directives_opt DIRECTIVE_NAME LPAREN directive_arg_list.
        Start symbol: compilation_unit. *)

  | MenhirState378 : (('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_directive_args, _menhir_box_compilation_unit) _menhir_state
    (** State 378.
        Stack shape : directives_opt DIRECTIVE_NAME directive_args.
        Start symbol: compilation_unit. *)

  | MenhirState383 : ('s _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 383.
        Stack shape : directives_opt module_kind_opt module_name_opt.
        Start symbol: compilation_unit. *)


and ('s, 'r) _menhir_cell1_block_list = 
  | MenhirCell1_block_list of 's * ('s, 'r) _menhir_state * (Ast.stmt list)

and ('s, 'r) _menhir_cell1_block_list_opt = 
  | MenhirCell1_block_list_opt of 's * ('s, 'r) _menhir_state * (Ast.stmt list)

and ('s, 'r) _menhir_cell1_by_opt = 
  | MenhirCell1_by_opt of 's * ('s, 'r) _menhir_state * (Ast.expr option)

and ('s, 'r) _menhir_cell1_case_clauses = 
  | MenhirCell1_case_clauses of 's * ('s, 'r) _menhir_state * ((Ast.expr list * Ast.stmt list) list)

and ('s, 'r) _menhir_cell1_case_labels = 
  | MenhirCell1_case_labels of 's * ('s, 'r) _menhir_state * (Ast.expr list)

and ('s, 'r) _menhir_cell1_decl_attr_list = 
  | MenhirCell1_decl_attr_list of 's * ('s, 'r) _menhir_state * (Ast.decl_attr list)

and ('s, 'r) _menhir_cell1_decl_attrs_opt = 
  | MenhirCell1_decl_attrs_opt of 's * ('s, 'r) _menhir_state * (Ast.decl_attr list)

and ('s, 'r) _menhir_cell1_decl_list_opt = 
  | MenhirCell1_decl_list_opt of 's * ('s, 'r) _menhir_state * (Ast.decl list)

and ('s, 'r) _menhir_cell1_dim_list = 
  | MenhirCell1_dim_list of 's * ('s, 'r) _menhir_state * (Ast.dim list)

and ('s, 'r) _menhir_cell1_directive_arg_list = 
  | MenhirCell1_directive_arg_list of 's * ('s, 'r) _menhir_state * (Ast.literal list)

and ('s, 'r) _menhir_cell1_directive_args = 
  | MenhirCell1_directive_args of 's * ('s, 'r) _menhir_state * (Ast.literal list)

and 's _menhir_cell0_directives_opt = 
  | MenhirCell0_directives_opt of 's * (Ast.directive list)

and ('s, 'r) _menhir_cell1_elsif_list = 
  | MenhirCell1_elsif_list of 's * ('s, 'r) _menhir_state * ((Ast.expr * Ast.stmt) list)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_expr_list = 
  | MenhirCell1_expr_list of 's * ('s, 'r) _menhir_state * (Ast.expr list)

and ('s, 'r) _menhir_cell1_formal_params_opt = 
  | MenhirCell1_formal_params_opt of 's * ('s, 'r) _menhir_state * (Ast.param list)

and ('s, 'r) _menhir_cell1_ident_list = 
  | MenhirCell1_ident_list of 's * ('s, 'r) _menhir_state * (Ast.ident_loc list)

and ('s, 'r) _menhir_cell1_ident_loc = 
  | MenhirCell1_ident_loc of 's * ('s, 'r) _menhir_state * (Ast.ident_loc)

and ('s, 'r) _menhir_cell1_lvalue = 
  | MenhirCell1_lvalue of 's * ('s, 'r) _menhir_state * (Ast.lvalue)

and 's _menhir_cell0_module_header = 
  | MenhirCell0_module_header of 's * (Ast.module_kind * Ast.ident_loc option * Ast.directive list *
  Ast.use_attr list)

and 's _menhir_cell0_module_kind_opt = 
  | MenhirCell0_module_kind_opt of 's * (Ast.module_kind)

and 's _menhir_cell0_module_name_opt = 
  | MenhirCell0_module_name_opt of 's * (Ast.ident_loc option)

and ('s, 'r) _menhir_cell1_param_list = 
  | MenhirCell1_param_list of 's * ('s, 'r) _menhir_state * (Ast.param list)

and ('s, 'r) _menhir_cell1_param_mode_opt = 
  | MenhirCell1_param_mode_opt of 's * ('s, 'r) _menhir_state * (Ast.param_mode option)

and ('s, 'r) _menhir_cell1_proc_body_opt = 
  | MenhirCell1_proc_body_opt of 's * ('s, 'r) _menhir_state * (Ast.stmt list option)

and 's _menhir_cell0_proc_end_name_opt = 
  | MenhirCell0_proc_end_name_opt of 's * (string option)

and ('s, 'r) _menhir_cell1_semis_opt = 
  | MenhirCell1_semis_opt of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_status_list = 
  | MenhirCell1_status_list of 's * ('s, 'r) _menhir_state * (Ast.status_item list)

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (Ast.stmt)

and ('s, 'r) _menhir_cell1_stmt_list_opt = 
  | MenhirCell1_stmt_list_opt of 's * ('s, 'r) _menhir_state * (Ast.stmt list)

and 's _menhir_cell0_table_dims_opt = 
  | MenhirCell0_table_dims_opt of 's * (Ast.dim list)

and ('s, 'r) _menhir_cell1_top_items_opt = 
  | MenhirCell1_top_items_opt of 's * ('s, 'r) _menhir_state * (Ast.decl list * Ast.stmt list * Ast.proc list)

and ('s, 'r) _menhir_cell1_type_spec_opt = 
  | MenhirCell1_type_spec_opt of 's * ('s, 'r) _menhir_state * (Ast.type_spec option)

and ('s, 'r) _menhir_cell1_use_attrs_opt = 
  | MenhirCell1_use_attrs_opt of 's * ('s, 'r) _menhir_state * (Ast.use_attr list)

and ('s, 'r) _menhir_cell1_ABORT = 
  | MenhirCell1_ABORT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_AND = 
  | MenhirCell1_AND of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BEGIN = 
  | MenhirCell1_BEGIN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BLOCK = 
  | MenhirCell1_BLOCK of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BY = 
  | MenhirCell1_BY of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CASE = 
  | MenhirCell1_CASE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COLON = 
  | MenhirCell1_COLON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COMMA = 
  | MenhirCell1_COMMA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DEF = 
  | MenhirCell1_DEF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_DEFAULT = 
  | MenhirCell1_DEFAULT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DEFINE = 
  | MenhirCell1_DEFINE of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_DIRECTIVE_NAME = 
  | MenhirCell0_DIRECTIVE_NAME of 's * (
# 32 "lib/parser.mly"
       (string)
# 1148 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_ELSE = 
  | MenhirCell1_ELSE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ELSIF = 
  | MenhirCell1_ELSIF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_END = 
  | MenhirCell1_END of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_END = 
  | MenhirCell0_END of 's * Lexing.position

and ('s, 'r) _menhir_cell1_EQ = 
  | MenhirCell1_EQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EQUAL = 
  | MenhirCell1_EQUAL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EQV = 
  | MenhirCell1_EQV of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EXIT = 
  | MenhirCell1_EXIT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FOR = 
  | MenhirCell1_FOR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FUNCTION = 
  | MenhirCell1_FUNCTION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GE = 
  | MenhirCell1_GE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GQ = 
  | MenhirCell1_GQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GR = 
  | MenhirCell1_GR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GT = 
  | MenhirCell1_GT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 34 "lib/parser.mly"
       (string)
# 1197 "lib/parser.ml"
) * Lexing.position * Lexing.position

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 34 "lib/parser.mly"
       (string)
# 1204 "lib/parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ITEM = 
  | MenhirCell1_ITEM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LABEL = 
  | MenhirCell1_LABEL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LE = 
  | MenhirCell1_LE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LQ = 
  | MenhirCell1_LQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LS = 
  | MenhirCell1_LS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LT = 
  | MenhirCell1_LT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MOD = 
  | MenhirCell1_MOD of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NEQ = 
  | MenhirCell1_NEQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NQ = 
  | MenhirCell1_NQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OF = 
  | MenhirCell1_OF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OR = 
  | MenhirCell1_OR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OTHERWISE = 
  | MenhirCell1_OTHERWISE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OVERLAY = 
  | MenhirCell1_OVERLAY of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_POS = 
  | MenhirCell1_POS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PROC = 
  | MenhirCell1_PROC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REF = 
  | MenhirCell1_REF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_REP = 
  | MenhirCell1_REP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SLASH = 
  | MenhirCell1_SLASH of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STAR = 
  | MenhirCell1_STAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STOP = 
  | MenhirCell1_STOP of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TABLE = 
  | MenhirCell1_TABLE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_THEN = 
  | MenhirCell1_THEN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TO = 
  | MenhirCell1_TO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHEN = 
  | MenhirCell1_WHEN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WITH = 
  | MenhirCell1_WITH of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_XOR = 
  | MenhirCell1_XOR of 's * ('s, 'r) _menhir_state

and _menhir_box_compilation_unit = 
  | MenhirBox_compilation_unit of (Ast.module_) [@@unboxed]

let _menhir_action_001 =
  fun () ->
    (
# 221 "lib/parser.mly"
                ( [] )
# 1317 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_002 =
  fun _2 ->
    (
# 222 "lib/parser.mly"
                                      ( _2 )
# 1325 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_003 =
  fun () ->
    (
# 399 "lib/parser.mly"
         ( None )
# 1333 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_004 =
  fun _1 ->
    (
# 400 "lib/parser.mly"
         ( Some _1 )
# 1341 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_005 =
  fun () ->
    (
# 401 "lib/parser.mly"
         ( Some SNoop )
# 1349 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_006 =
  fun _1 ->
    (
# 389 "lib/parser.mly"
               ( (match _1 with None -> [] | Some s -> [s]) )
# 1357 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_007 =
  fun _1 _2 ->
    (
# 391 "lib/parser.mly"
      ( _1 @ (match _2 with None -> [] | Some s -> [s]) )
# 1365 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_008 =
  fun () ->
    (
# 385 "lib/parser.mly"
                ( [] )
# 1373 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_009 =
  fun _1 ->
    (
# 386 "lib/parser.mly"
               ( _1 )
# 1381 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_010 =
  fun () ->
    (
# 451 "lib/parser.mly"
                ( None )
# 1389 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_011 =
  fun _2 ->
    (
# 452 "lib/parser.mly"
            ( Some _2 )
# 1397 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_012 =
  fun () ->
    (
# 415 "lib/parser.mly"
                ( [] )
# 1405 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_013 =
  fun _2 ->
    (
# 416 "lib/parser.mly"
                                ( _2 )
# 1413 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_014 =
  fun _3 ->
    (
# 417 "lib/parser.mly"
                                     ( _3 )
# 1421 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_015 =
  fun _1 _2 ->
    (
# 412 "lib/parser.mly"
                             ( SCall (_1, _2) )
# 1429 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_016 =
  fun _2 _4 ->
    (
# 463 "lib/parser.mly"
                                         ( (_2, _4) )
# 1437 "lib/parser.ml"
     : (Ast.expr list * Ast.stmt list))

let _menhir_action_017 =
  fun _1 ->
    (
# 459 "lib/parser.mly"
                ( [_1] )
# 1445 "lib/parser.ml"
     : ((Ast.expr list * Ast.stmt list) list))

let _menhir_action_018 =
  fun _1 _2 ->
    (
# 460 "lib/parser.mly"
                             ( _1 @ [_2] )
# 1453 "lib/parser.ml"
     : ((Ast.expr list * Ast.stmt list) list))

let _menhir_action_019 =
  fun _1 ->
    (
# 466 "lib/parser.mly"
         ( [_1] )
# 1461 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_020 =
  fun _1 _3 ->
    (
# 467 "lib/parser.mly"
                           ( _1 @ [_3] )
# 1469 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_021 =
  fun _2 _4 _5 ->
    (
# 456 "lib/parser.mly"
      ( SCase { expr = _2; clauses = _4; otherwise_ = _5 } )
# 1477 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_022 =
  fun _1 ->
    (
# 65 "lib/parser.mly"
              ( _1 )
# 1485 "lib/parser.ml"
     : (Ast.module_))

let _menhir_action_023 =
  fun () ->
    (
# 145 "lib/parser.mly"
                ( [] )
# 1493 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_024 =
  fun _1 _3 ->
    (
# 147 "lib/parser.mly"
      ( _1 @ [ { d_name = "ICOMPOOL"; d_args = [LString _3] } ] )
# 1501 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_025 =
  fun _2 _3 _4 ->
    (
# 179 "lib/parser.mly"
      ( DItem { names = _2; typ = _3; attrs = _4 } )
# 1509 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_026 =
  fun _2 _3 _4 _5 _7 ->
    (
# 182 "lib/parser.mly"
      ( DTable { name = _2; dims = _3; typ = _4; attrs = _5; body = _7 } )
# 1517 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_027 =
  fun _2 _3 _5 ->
    (
# 185 "lib/parser.mly"
      ( DBlock { name = _2; attrs = _3; body = _5 } )
# 1525 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_028 =
  fun _2 _5 ->
    (
# 188 "lib/parser.mly"
      ( DTypeStatus { name = _2; items = _5 } )
# 1533 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_029 =
  fun _2 _4 ->
    (
# 191 "lib/parser.mly"
      ( DTypeAlias { name = _2; target = _4 } )
# 1541 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_030 =
  fun _2 ->
    (
# 194 "lib/parser.mly"
      ( DOverlayDecl _2 )
# 1549 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_031 =
  fun _2 _3 ->
    (
# 197 "lib/parser.mly"
    ( DDefine { name = _2; rhs = _3 } )
# 1557 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_032 =
  fun _2 _4 ->
    (
# 200 "lib/parser.mly"
      ( DDefine { name = _2; rhs = _4 } )
# 1565 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_033 =
  fun _1 ->
    (
# 202 "lib/parser.mly"
                 ( _1 )
# 1573 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_034 =
  fun _2 ->
    (
# 205 "lib/parser.mly"
      ( DLabelDecl _2 )
# 1581 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_035 =
  fun () ->
    (
# 265 "lib/parser.mly"
                           ( DStatic )
# 1589 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_036 =
  fun () ->
    (
# 266 "lib/parser.mly"
                           ( DConstant )
# 1597 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_037 =
  fun _2 ->
    (
# 267 "lib/parser.mly"
                           ( DDefault _2 )
# 1605 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_038 =
  fun _2 ->
    (
# 268 "lib/parser.mly"
                           ( DLike _2 )
# 1613 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_039 =
  fun _3 ->
    (
# 269 "lib/parser.mly"
                           ( DPos _3 )
# 1621 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_040 =
  fun _3 ->
    (
# 270 "lib/parser.mly"
                           ( DRep _3 )
# 1629 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_041 =
  fun _2 ->
    (
# 271 "lib/parser.mly"
                           ( DOverlay _2 )
# 1637 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_042 =
  fun _2 ->
    (
# 272 "lib/parser.mly"
                           ( DInstance _2 )
# 1645 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_043 =
  fun () ->
    (
# 273 "lib/parser.mly"
                           ( DRec )
# 1653 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_044 =
  fun () ->
    (
# 274 "lib/parser.mly"
                           ( DRent )
# 1661 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_045 =
  fun () ->
    (
# 275 "lib/parser.mly"
                           ( DInline )
# 1669 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_046 =
  fun () ->
    (
# 276 "lib/parser.mly"
                           ( DParallel )
# 1677 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_047 =
  fun _1 ->
    (
# 261 "lib/parser.mly"
            ( [_1] )
# 1685 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_048 =
  fun _1 _3 ->
    (
# 262 "lib/parser.mly"
                                 ( _1 @ [_3] )
# 1693 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_049 =
  fun () ->
    (
# 257 "lib/parser.mly"
                ( [] )
# 1701 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_050 =
  fun _3 ->
    (
# 258 "lib/parser.mly"
                                      ( _3 )
# 1709 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_051 =
  fun () ->
    (
# 225 "lib/parser.mly"
                ( [] )
# 1717 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_052 =
  fun _1 _2 ->
    (
# 226 "lib/parser.mly"
                       ( _1 @ [_2] )
# 1725 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_053 =
  fun _1 ->
    (
# 208 "lib/parser.mly"
           ( DefString _1 )
# 1733 "lib/parser.ml"
     : (Ast.define_rhs))

let _menhir_action_054 =
  fun _1 ->
    (
# 209 "lib/parser.mly"
           ( DefExpr _1 )
# 1741 "lib/parser.ml"
     : (Ast.define_rhs))

let _menhir_action_055 =
  fun () ->
    (
# 241 "lib/parser.mly"
          ( DimStar )
# 1749 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_056 =
  fun _1 ->
    (
# 242 "lib/parser.mly"
          ( DimInt _1 )
# 1757 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_057 =
  fun _1 ->
    (
# 243 "lib/parser.mly"
          ( DimId _1 )
# 1765 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_058 =
  fun _1 ->
    (
# 237 "lib/parser.mly"
      ( [_1] )
# 1773 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_059 =
  fun _1 _3 ->
    (
# 238 "lib/parser.mly"
                     ( _1 @ [_3] )
# 1781 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_060 =
  fun _2 _3 ->
    (
# 110 "lib/parser.mly"
    ( { d_name = _2; d_args = _3 } )
# 1789 "lib/parser.ml"
     : (Ast.directive))

let _menhir_action_061 =
  fun _1 ->
    (
# 135 "lib/parser.mly"
         ( LString _1 )
# 1797 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_062 =
  fun _1 ->
    (
# 136 "lib/parser.mly"
         ( LInt _1 )
# 1805 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_063 =
  fun _1 ->
    (
# 137 "lib/parser.mly"
         ( LFloat _1 )
# 1813 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_064 =
  fun _1 ->
    (
# 138 "lib/parser.mly"
         ( let (b,v) = _1 in LBead (b,v) )
# 1821 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_065 =
  fun () ->
    (
# 139 "lib/parser.mly"
         ( LNull )
# 1829 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_066 =
  fun () ->
    (
# 140 "lib/parser.mly"
         ( LBool true )
# 1837 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_067 =
  fun () ->
    (
# 141 "lib/parser.mly"
         ( LBool false )
# 1845 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_068 =
  fun _1 ->
    (
# 142 "lib/parser.mly"
         ( LString _1 )
# 1853 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_069 =
  fun _1 ->
    (
# 127 "lib/parser.mly"
                ( [_1] )
# 1861 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_070 =
  fun _1 _3 ->
    (
# 128 "lib/parser.mly"
                                         ( _1 @ [_3] )
# 1869 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_071 =
  fun () ->
    (
# 123 "lib/parser.mly"
                ( [] )
# 1877 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_072 =
  fun _1 ->
    (
# 124 "lib/parser.mly"
                       ( _1 )
# 1885 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_073 =
  fun _1 ->
    (
# 131 "lib/parser.mly"
                ( [_1] )
# 1893 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_074 =
  fun _1 _2 ->
    (
# 132 "lib/parser.mly"
                               ( _1 @ [_2] )
# 1901 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_075 =
  fun () ->
    (
# 118 "lib/parser.mly"
                ( [] )
# 1909 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_076 =
  fun _1 ->
    (
# 119 "lib/parser.mly"
                   ( _1 )
# 1917 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_077 =
  fun _2 ->
    (
# 120 "lib/parser.mly"
                                         ( _2 )
# 1925 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_078 =
  fun () ->
    (
# 105 "lib/parser.mly"
                ( [] )
# 1933 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_079 =
  fun _1 _2 ->
    (
# 106 "lib/parser.mly"
                             ( _1 @ [_2] )
# 1941 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_080 =
  fun () ->
    (
# 437 "lib/parser.mly"
                ( None )
# 1949 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_081 =
  fun _2 ->
    (
# 438 "lib/parser.mly"
              ( Some _2 )
# 1957 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_082 =
  fun _2 _4 ->
    (
# 433 "lib/parser.mly"
                         ( [(_2, _4)] )
# 1965 "lib/parser.ml"
     : ((Ast.expr * Ast.stmt) list))

let _menhir_action_083 =
  fun _1 _3 _5 ->
    (
# 434 "lib/parser.mly"
                                    ( _1 @ [(_3, _5)] )
# 1973 "lib/parser.ml"
     : ((Ast.expr * Ast.stmt) list))

let _menhir_action_084 =
  fun _1 ->
    (
# 404 "lib/parser.mly"
          ( Some _1 )
# 1981 "lib/parser.ml"
     : (string option))

let _menhir_action_085 =
  fun () ->
    (
# 405 "lib/parser.mly"
                ( None )
# 1989 "lib/parser.ml"
     : (string option))

let _menhir_action_086 =
  fun _1 ->
    (
# 480 "lib/parser.mly"
           ( EInt _1 )
# 1997 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_087 =
  fun _1 ->
    (
# 481 "lib/parser.mly"
           ( EFloat _1 )
# 2005 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_088 =
  fun _1 ->
    (
# 482 "lib/parser.mly"
           ( EString _1 )
# 2013 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_089 =
  fun _1 ->
    (
# 483 "lib/parser.mly"
           ( let (b,v) = _1 in EBead (b,v) )
# 2021 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_090 =
  fun () ->
    (
# 484 "lib/parser.mly"
           ( ENull )
# 2029 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_091 =
  fun () ->
    (
# 485 "lib/parser.mly"
           ( EBool true )
# 2037 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_092 =
  fun () ->
    (
# 486 "lib/parser.mly"
           ( EBool false )
# 2045 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_093 =
  fun _1 ->
    (
# 487 "lib/parser.mly"
           ( EVar _1 )
# 2053 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_094 =
  fun _1 _3 ->
    (
# 488 "lib/parser.mly"
                                      ( ECall (_1, _3) )
# 2061 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_095 =
  fun _2 ->
    (
# 489 "lib/parser.mly"
                       ( EParen _2 )
# 2069 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_096 =
  fun _2 ->
    (
# 491 "lib/parser.mly"
                            ( EUn ("-", _2) )
# 2077 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_097 =
  fun _2 ->
    (
# 492 "lib/parser.mly"
             ( EUn ("NOT", _2) )
# 2085 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_098 =
  fun _1 _3 ->
    (
# 494 "lib/parser.mly"
                    ( EBin ("*", _1, _3) )
# 2093 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_099 =
  fun _1 _3 ->
    (
# 495 "lib/parser.mly"
                    ( EBin ("/", _1, _3) )
# 2101 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_100 =
  fun _1 _3 ->
    (
# 496 "lib/parser.mly"
                    ( EBin ("MOD", _1, _3) )
# 2109 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_101 =
  fun _1 _3 ->
    (
# 497 "lib/parser.mly"
                    ( EBin ("+", _1, _3) )
# 2117 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_102 =
  fun _1 _3 ->
    (
# 498 "lib/parser.mly"
                    ( EBin ("-", _1, _3) )
# 2125 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_103 =
  fun _1 _3 ->
    (
# 500 "lib/parser.mly"
                    ( EBin ("<", _1, _3) )
# 2133 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_104 =
  fun _1 _3 ->
    (
# 501 "lib/parser.mly"
                    ( EBin ("<=", _1, _3) )
# 2141 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_105 =
  fun _1 _3 ->
    (
# 502 "lib/parser.mly"
                    ( EBin (">", _1, _3) )
# 2149 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_106 =
  fun _1 _3 ->
    (
# 503 "lib/parser.mly"
                    ( EBin (">=", _1, _3) )
# 2157 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_107 =
  fun _1 _3 ->
    (
# 504 "lib/parser.mly"
                    ( EBin ("<>", _1, _3) )
# 2165 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_108 =
  fun _1 _3 ->
    (
# 505 "lib/parser.mly"
                    ( EBin ("=", _1, _3) )
# 2173 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_109 =
  fun _1 _3 ->
    (
# 507 "lib/parser.mly"
                    ( EBin ("EQ", _1, _3) )
# 2181 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_110 =
  fun _1 _3 ->
    (
# 508 "lib/parser.mly"
                    ( EBin ("NQ", _1, _3) )
# 2189 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_111 =
  fun _1 _3 ->
    (
# 509 "lib/parser.mly"
                    ( EBin ("LS", _1, _3) )
# 2197 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_112 =
  fun _1 _3 ->
    (
# 510 "lib/parser.mly"
                    ( EBin ("LQ", _1, _3) )
# 2205 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_113 =
  fun _1 _3 ->
    (
# 511 "lib/parser.mly"
                    ( EBin ("GR", _1, _3) )
# 2213 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_114 =
  fun _1 _3 ->
    (
# 512 "lib/parser.mly"
                    ( EBin ("GQ", _1, _3) )
# 2221 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_115 =
  fun _1 _3 ->
    (
# 514 "lib/parser.mly"
                    ( EBin ("AND", _1, _3) )
# 2229 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_116 =
  fun _1 _3 ->
    (
# 515 "lib/parser.mly"
                    ( EBin ("OR", _1, _3) )
# 2237 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_117 =
  fun _1 _3 ->
    (
# 516 "lib/parser.mly"
                    ( EBin ("XOR", _1, _3) )
# 2245 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_118 =
  fun _1 _3 ->
    (
# 517 "lib/parser.mly"
                    ( EBin ("EQV", _1, _3) )
# 2253 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_119 =
  fun _1 ->
    (
# 424 "lib/parser.mly"
         ( [_1] )
# 2261 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_120 =
  fun _1 _3 ->
    (
# 425 "lib/parser.mly"
                         ( _1 @ [_3] )
# 2269 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_121 =
  fun () ->
    (
# 420 "lib/parser.mly"
                ( [] )
# 2277 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_122 =
  fun _1 ->
    (
# 421 "lib/parser.mly"
              ( _1 )
# 2285 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_123 =
  fun () ->
    (
# 408 "lib/parser.mly"
                ( None )
# 2293 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_124 =
  fun _1 ->
    (
# 409 "lib/parser.mly"
         ( Some _1 )
# 2301 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_125 =
  fun _2 _4 _6 _7 _8 ->
    (
# 445 "lib/parser.mly"
      ( SForTo { var = _2; start_ = _4; stop_ = _6; step = _7; body = _8 } )
# 2309 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_126 =
  fun _2 _4 _5 _7 _8 ->
    (
# 448 "lib/parser.mly"
      ( SForWhile { var = _2; start_ = _4; step = _5; cond = _7; body = _8 } )
# 2317 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_127 =
  fun () ->
    (
# 326 "lib/parser.mly"
                ( [] )
# 2325 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_128 =
  fun _2 ->
    (
# 327 "lib/parser.mly"
                                 ( _2 )
# 2333 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_129 =
  fun _1 ->
    (
# 283 "lib/parser.mly"
              ( [_1] )
# 2341 "lib/parser.ml"
     : (Ast.ident_loc list))

let _menhir_action_130 =
  fun _1 _3 ->
    (
# 284 "lib/parser.mly"
                               ( _1 @ [_3] )
# 2349 "lib/parser.ml"
     : (Ast.ident_loc list))

let _menhir_action_131 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 279 "lib/parser.mly"
          ( mk_loc _1 _startpos _endpos )
# 2359 "lib/parser.ml"
     : (Ast.ident_loc))

let _menhir_action_132 =
  fun _2 _4 _5 ->
    (
# 428 "lib/parser.mly"
                               ( SIf (_2, _4, _5) )
# 2367 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_133 =
  fun _2 _4 _5 _6 ->
    (
# 430 "lib/parser.mly"
      ( SIfElsif (((_2,_4) :: _5), _6) )
# 2375 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_134 =
  fun _2 ->
    (
# 212 "lib/parser.mly"
                            ( DLinkage { kind = LDef; target = _2 } )
# 2383 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_135 =
  fun _2 ->
    (
# 213 "lib/parser.mly"
                            ( DLinkage { kind = LRef; target = _2 } )
# 2391 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_136 =
  fun _1 ->
    (
# 216 "lib/parser.mly"
          ( LName _1 )
# 2399 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_137 =
  fun _2 _3 ->
    (
# 217 "lib/parser.mly"
                                 ( LProcSig (_2, _3) )
# 2407 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_138 =
  fun _2 _3 _4 ->
    (
# 218 "lib/parser.mly"
                                                   ( LFunSig (_2, _3, _4) )
# 2415 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_139 =
  fun _1 ->
    (
# 474 "lib/parser.mly"
          ( LVar _1 )
# 2423 "lib/parser.ml"
     : (Ast.lvalue))

let _menhir_action_140 =
  fun _1 _3 ->
    (
# 475 "lib/parser.mly"
                                      ( LIndex (_1, _3) )
# 2431 "lib/parser.ml"
     : (Ast.lvalue))

let _menhir_action_141 =
  fun _2 _3 ->
    (
# 69 "lib/parser.mly"
    (
      let (kind, name, directives, attrs) = _2 in
      let (decls, stmts, procs) = _3 in
      ({ kind; name; directives; attrs; decls; stmts; procs } : Ast.module_)
    )
# 2443 "lib/parser.ml"
     : (Ast.module_))

let _menhir_action_142 =
  fun _2 ->
    (
# 150 "lib/parser.mly"
                                      ( _2 )
# 2451 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_143 =
  fun _1 ->
    (
# 151 "lib/parser.mly"
                  ( _1 )
# 2459 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_144 =
  fun _1 _2 _3 _4 _5 ->
    (
# 77 "lib/parser.mly"
    (
      let directives = _1 @ _5 in
      (_2, _3, directives, _4)
    )
# 2470 "lib/parser.ml"
     : (Ast.module_kind * Ast.ident_loc option * Ast.directive list *
  Ast.use_attr list))

let _menhir_action_145 =
  fun () ->
    (
# 83 "lib/parser.mly"
            ( Program )
# 2479 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_146 =
  fun () ->
    (
# 84 "lib/parser.mly"
            ( Compool )
# 2487 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_147 =
  fun () ->
    (
# 85 "lib/parser.mly"
            ( Proc_module )
# 2495 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_148 =
  fun () ->
    (
# 86 "lib/parser.mly"
             ( Function_module )
# 2503 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_149 =
  fun () ->
    (
# 87 "lib/parser.mly"
                ( Unknown )
# 2511 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_150 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 90 "lib/parser.mly"
          ( Some (mk_loc _1 _startpos _endpos) )
# 2521 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_151 =
  fun () ->
    (
# 91 "lib/parser.mly"
                ( None )
# 2529 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_152 =
  fun () ->
    (
# 470 "lib/parser.mly"
                ( None )
# 2537 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_153 =
  fun _3 ->
    (
# 471 "lib/parser.mly"
                                  ( Some _3 )
# 2545 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_154 =
  fun _1 _2 _3 ->
    (
# 339 "lib/parser.mly"
      ( { pmode = _1; pname = _2; ptype = _3 } )
# 2553 "lib/parser.ml"
     : (Ast.param))

let _menhir_action_155 =
  fun _1 ->
    (
# 334 "lib/parser.mly"
          ( [_1] )
# 2561 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_156 =
  fun _1 _3 ->
    (
# 335 "lib/parser.mly"
                           ( _1 @ [_3] )
# 2569 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_157 =
  fun () ->
    (
# 330 "lib/parser.mly"
                ( [] )
# 2577 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_158 =
  fun _1 ->
    (
# 331 "lib/parser.mly"
               ( _1 )
# 2585 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_159 =
  fun () ->
    (
# 342 "lib/parser.mly"
          ( Some ByRef )
# 2593 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_160 =
  fun () ->
    (
# 343 "lib/parser.mly"
          ( Some ByVal )
# 2601 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_161 =
  fun () ->
    (
# 344 "lib/parser.mly"
          ( Some ByRes )
# 2609 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_162 =
  fun () ->
    (
# 345 "lib/parser.mly"
                ( None )
# 2617 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_163 =
  fun () ->
    (
# 348 "lib/parser.mly"
                ( None )
# 2625 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_164 =
  fun _2 ->
    (
# 349 "lib/parser.mly"
                                      ( Some _2 )
# 2633 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_165 =
  fun _3 _4 _5 _7 _8 _startpos__1_ ->
    let _startpos = _startpos__1_ in
    (
# 298 "lib/parser.mly"
      (
        let endpos = _8 in
        { pkind = PProc
        ; pr_name = _3
        ; pr_span = mk_span _startpos endpos
        ; params = _4
        ; rettype = None
        ; pattrs = _5
        ; directives = []
        ; body = _7
        }
      )
# 2653 "lib/parser.ml"
     : (Ast.proc))

let _menhir_action_166 =
  fun _3 _4 _5 _6 _8 _9 _startpos__1_ ->
    let _startpos = _startpos__1_ in
    (
# 312 "lib/parser.mly"
      (
        let endpos = _9 in
        { pkind = PFunction
        ; pr_name = _3
        ; pr_span = mk_span _startpos endpos
        ; params = _4
        ; rettype = _5
        ; pattrs = _6
        ; directives = []
        ; body = _8
        }
      )
# 2673 "lib/parser.ml"
     : (Ast.proc))

let _menhir_action_167 =
  fun _endpos_e_ e ->
    (
# 353 "lib/parser.mly"
                                      ( ignore e; _endpos_e_ )
# 2681 "lib/parser.ml"
     : (Lexing.position))

let _menhir_action_168 =
  fun _1 ->
    (
# 357 "lib/parser.mly"
          ( Some _1 )
# 2689 "lib/parser.ml"
     : (string option))

let _menhir_action_169 =
  fun () ->
    (
# 358 "lib/parser.mly"
                ( None )
# 2697 "lib/parser.ml"
     : (string option))

let _menhir_action_170 =
  fun () ->
    (
# 154 "lib/parser.mly"
                ( () )
# 2705 "lib/parser.ml"
     : (unit))

let _menhir_action_171 =
  fun () ->
    (
# 155 "lib/parser.mly"
                   ( () )
# 2713 "lib/parser.ml"
     : (unit))

let _menhir_action_172 =
  fun _1 ->
    (
# 291 "lib/parser.mly"
          ( SName _1 )
# 2721 "lib/parser.ml"
     : (Ast.status_item))

let _menhir_action_173 =
  fun _3 ->
    (
# 292 "lib/parser.mly"
                                 ( SVal _3 )
# 2729 "lib/parser.ml"
     : (Ast.status_item))

let _menhir_action_174 =
  fun _1 ->
    (
# 287 "lib/parser.mly"
              ( [_1] )
# 2737 "lib/parser.ml"
     : (Ast.status_item list))

let _menhir_action_175 =
  fun _1 _3 ->
    (
# 288 "lib/parser.mly"
                                ( _1 @ [_3] )
# 2745 "lib/parser.ml"
     : (Ast.status_item list))

let _menhir_action_176 =
  fun _1 _3 ->
    (
# 367 "lib/parser.mly"
                     ( SLabel (_1, _3) )
# 2753 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_177 =
  fun _1 _3 ->
    (
# 368 "lib/parser.mly"
                           ( SAssign (_1, _3) )
# 2761 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_178 =
  fun _1 ->
    (
# 369 "lib/parser.mly"
              ( _1 )
# 2769 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_179 =
  fun _1 ->
    (
# 370 "lib/parser.mly"
            ( _1 )
# 2777 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_180 =
  fun _1 ->
    (
# 371 "lib/parser.mly"
               ( _1 )
# 2785 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_181 =
  fun _1 ->
    (
# 372 "lib/parser.mly"
             ( _1 )
# 2793 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_182 =
  fun _1 ->
    (
# 373 "lib/parser.mly"
              ( _1 )
# 2801 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_183 =
  fun _2 ->
    (
# 374 "lib/parser.mly"
                    ( SGoto _2 )
# 2809 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_184 =
  fun _2 ->
    (
# 375 "lib/parser.mly"
                         ( SReturn _2 )
# 2817 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_185 =
  fun _2 ->
    (
# 376 "lib/parser.mly"
                            ( SExit _2 )
# 2825 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_186 =
  fun _2 ->
    (
# 377 "lib/parser.mly"
                       ( SStop _2 )
# 2833 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_187 =
  fun _2 ->
    (
# 378 "lib/parser.mly"
                        ( SAbort _2 )
# 2841 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_188 =
  fun () ->
    (
# 379 "lib/parser.mly"
                  ( SFallthru )
# 2849 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_189 =
  fun _2 ->
    (
# 380 "lib/parser.mly"
                                       ( SBlock _2 )
# 2857 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_190 =
  fun () ->
    (
# 381 "lib/parser.mly"
         ( SNoop )
# 2865 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_191 =
  fun () ->
    (
# 361 "lib/parser.mly"
                ( [] )
# 2873 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_192 =
  fun _1 _2 ->
    (
# 362 "lib/parser.mly"
                       ( _1 @ [_2] )
# 2881 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_193 =
  fun () ->
    (
# 229 "lib/parser.mly"
                ( [] )
# 2889 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_194 =
  fun _2 ->
    (
# 230 "lib/parser.mly"
                                      ( _2 )
# 2897 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_195 =
  fun () ->
    (
# 233 "lib/parser.mly"
                ( [] )
# 2905 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_196 =
  fun _2 ->
    (
# 234 "lib/parser.mly"
                           ( _2 )
# 2913 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_197 =
  fun _1 ->
    (
# 169 "lib/parser.mly"
         ( `Decl _1 )
# 2921 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_198 =
  fun _1 ->
    (
# 170 "lib/parser.mly"
             ( `Proc _1 )
# 2929 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_199 =
  fun _1 ->
    (
# 171 "lib/parser.mly"
         ( `Stmt _1 )
# 2937 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_200 =
  fun () ->
    (
# 172 "lib/parser.mly"
         ( `Stmt SNoop )
# 2945 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_201 =
  fun () ->
    (
# 158 "lib/parser.mly"
                ( (([] : Ast.decl list), ([] : Ast.stmt list), ([] : Ast.proc list)) )
# 2953 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_202 =
  fun _1 _2 ->
    (
# 160 "lib/parser.mly"
      (
        let (ds, ss, ps) = _1 in
        match _2 with
        | `Decl d -> (ds @ [d], ss, ps)
        | `Stmt s -> (ds, ss @ [s], ps)
        | `Proc p -> (ds, ss, ps @ [p])
      )
# 2967 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_203 =
  fun _1 _2 ->
    (
# 252 "lib/parser.mly"
                 ( TAtom (_1 ^ " " ^ string_of_int _2) )
# 2975 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_204 =
  fun _1 ->
    (
# 253 "lib/parser.mly"
                 ( TAtom _1 )
# 2983 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_205 =
  fun _1 ->
    (
# 254 "lib/parser.mly"
                 ( TNamed _1 )
# 2991 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_206 =
  fun () ->
    (
# 246 "lib/parser.mly"
                ( None )
# 2999 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_207 =
  fun _1 ->
    (
# 247 "lib/parser.mly"
              ( Some _1 )
# 3007 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_208 =
  fun _2 ->
    (
# 248 "lib/parser.mly"
                    ( Some _2 )
# 3015 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_209 =
  fun _2 ->
    (
# 249 "lib/parser.mly"
                   ( Some _2 )
# 3023 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_210 =
  fun () ->
    (
# 98 "lib/parser.mly"
             ( ARec )
# 3031 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_211 =
  fun () ->
    (
# 99 "lib/parser.mly"
             ( ARent )
# 3039 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_212 =
  fun () ->
    (
# 100 "lib/parser.mly"
             ( AStatic )
# 3047 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_213 =
  fun () ->
    (
# 101 "lib/parser.mly"
             ( AParallel )
# 3055 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_214 =
  fun () ->
    (
# 102 "lib/parser.mly"
             ( AInline )
# 3063 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_215 =
  fun () ->
    (
# 94 "lib/parser.mly"
                ( [] )
# 3071 "lib/parser.ml"
     : (Ast.use_attr list))

let _menhir_action_216 =
  fun _1 _2 ->
    (
# 95 "lib/parser.mly"
                           ( _1 @ [_2] )
# 3079 "lib/parser.ml"
     : (Ast.use_attr list))

let _menhir_action_217 =
  fun _2 _3 ->
    (
# 441 "lib/parser.mly"
                    ( SWhile (_2, _3) )
# 3087 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ABORT ->
        "ABORT"
    | AND ->
        "AND"
    | BANG ->
        "BANG"
    | BEAD _ ->
        "BEAD"
    | BEGIN ->
        "BEGIN"
    | BLOCK ->
        "BLOCK"
    | BY ->
        "BY"
    | BYREF ->
        "BYREF"
    | BYRES ->
        "BYRES"
    | BYVAL ->
        "BYVAL"
    | CASE ->
        "CASE"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | COMPOOL ->
        "COMPOOL"
    | CONSTANT ->
        "CONSTANT"
    | DEF ->
        "DEF"
    | DEFAULT ->
        "DEFAULT"
    | DEFINE ->
        "DEFINE"
    | DIRECTIVE_NAME _ ->
        "DIRECTIVE_NAME"
    | ELSE ->
        "ELSE"
    | ELSIF ->
        "ELSIF"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQUAL ->
        "EQUAL"
    | EQV ->
        "EQV"
    | EXIT ->
        "EXIT"
    | FALLTHRU ->
        "FALLTHRU"
    | FALSE ->
        "FALSE"
    | FLOAT _ ->
        "FLOAT"
    | FOR ->
        "FOR"
    | FUNCTION ->
        "FUNCTION"
    | GE ->
        "GE"
    | GOTO ->
        "GOTO"
    | GQ ->
        "GQ"
    | GR ->
        "GR"
    | GT ->
        "GT"
    | ICOMPOOL ->
        "ICOMPOOL"
    | IDENT _ ->
        "IDENT"
    | IF ->
        "IF"
    | INLINE ->
        "INLINE"
    | INSTANCE ->
        "INSTANCE"
    | INT _ ->
        "INT"
    | ITEM ->
        "ITEM"
    | LABEL ->
        "LABEL"
    | LBRACK ->
        "LBRACK"
    | LE ->
        "LE"
    | LIKE ->
        "LIKE"
    | LPAREN ->
        "LPAREN"
    | LQ ->
        "LQ"
    | LS ->
        "LS"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | MOD ->
        "MOD"
    | NEQ ->
        "NEQ"
    | NOT ->
        "NOT"
    | NQ ->
        "NQ"
    | NULL ->
        "NULL"
    | OF ->
        "OF"
    | OR ->
        "OR"
    | OTHERWISE ->
        "OTHERWISE"
    | OVERLAY ->
        "OVERLAY"
    | PARALLEL ->
        "PARALLEL"
    | PLUS ->
        "PLUS"
    | POS ->
        "POS"
    | PROC ->
        "PROC"
    | PROGRAM ->
        "PROGRAM"
    | RBRACK ->
        "RBRACK"
    | REC ->
        "REC"
    | REF ->
        "REF"
    | RENT ->
        "RENT"
    | REP ->
        "REP"
    | RETURN ->
        "RETURN"
    | RPAREN ->
        "RPAREN"
    | SEMI ->
        "SEMI"
    | SLASH ->
        "SLASH"
    | STAR ->
        "STAR"
    | START ->
        "START"
    | STATIC ->
        "STATIC"
    | STATUS ->
        "STATUS"
    | STOP ->
        "STOP"
    | STRING _ ->
        "STRING"
    | TABLE ->
        "TABLE"
    | TERM ->
        "TERM"
    | THEN ->
        "THEN"
    | TO ->
        "TO"
    | TRUE ->
        "TRUE"
    | TYPE ->
        "TYPE"
    | TYPEATOM _ ->
        "TYPEATOM"
    | WHEN ->
        "WHEN"
    | WHILE ->
        "WHILE"
    | WITH ->
        "WITH"
    | XOR ->
        "XOR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_goto_module_body : type  ttv_stack. ttv_stack _menhir_cell0_module_header -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_module_header (_menhir_stack, _2) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_141 _2 _3 in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_022 _1 in
          MenhirBox_compilation_unit _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_384 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RENT ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_334 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_336 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_023 () in
          _menhir_goto_compool_includes_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_321 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_212 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_use_attr : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_216 _1 _2 in
      _menhir_goto_use_attrs_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_use_attrs_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState383 ->
          _menhir_run_384 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState342 ->
          _menhir_run_343 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState319 ->
          _menhir_run_320 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_343 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              _menhir_run_323 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState344
          | END ->
              let _v_0 = _menhir_action_163 () in
              _menhir_run_345 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState344
          | _ ->
              _eRR ())
      | RENT ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_334 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_336 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_323 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_191 () in
      _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323 _tok
  
  and _menhir_run_324 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState324
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState324, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_170 () in
          _menhir_run_326 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState325 _tok
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
      | _ ->
          _eRR ()
  
  and _menhir_run_005 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState005 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_091 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState305 ->
          _menhir_run_306 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_299 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState235 ->
          _menhir_run_238 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState237 ->
          _menhir_run_238 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState169 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState259 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState076 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState011 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState014 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState059 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState045 ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState041 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState023 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState021 ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_306 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState306) in
          let _menhir_s = MenhirState307 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_XOR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState023 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_088 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_090 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_009 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState009 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState010 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState011 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_086 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | STRING _v_0 ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState014
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | INT _v_1 ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState014
          | IDENT _v_2 ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState014
          | FLOAT _v_3 ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState014
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | BEAD _v_4 ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState014
          | RPAREN ->
              let _v_5 = _menhir_action_121 () in
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
          | _ ->
              _eRR ())
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | MOD | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | SLASH | STAR | STOP | THEN | TO | WHILE | XOR ->
          let _1 = _v in
          let _v = _menhir_action_093 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_087 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_092 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_018 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_094 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STOP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | SEMI ->
          let _v = _menhir_action_123 () in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_STOP -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_STOP (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_186 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState351 ->
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState004 ->
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState070 ->
          _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState307 ->
          _menhir_run_308 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState302 ->
          _menhir_run_303 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState300 ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState292 ->
          _menhir_run_293 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState102 ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState324 ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState283 ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState118 ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState276 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_348 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_199 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_top_item : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_202 _1 _2 in
      _menhir_goto_top_items_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_top_items_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState002 ->
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_351 : type  ttv_stack. (ttv_stack _menhir_cell0_module_header as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | TYPE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | TABLE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | STOP ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | SEMI ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RETURN ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | REF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | OVERLAY ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | LABEL ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | ITEM ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | IF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState351
      | GOTO ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | FOR ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | EXIT ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | DEFINE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | DEF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_315 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | CASE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | BLOCK ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | BEGIN ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | ABORT ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | TERM ->
          let _1 = _v in
          let _v = _menhir_action_143 _1 in
          _menhir_goto_module_body _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_120 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState120 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_1, _endpos__1_, _startpos__1_) = (_v, _endpos, _startpos) in
      let _v = _menhir_action_131 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_ident_loc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident_loc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState338 ->
          _menhir_run_340 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState316 ->
          _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState246 ->
          _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState234 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState224 ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState226 ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState220 ->
          _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState217 ->
          _menhir_run_218 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState199 ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_340 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState340
      | COLON | IDENT _ | INLINE | PARALLEL | REC | RENT | SEMI | STATIC | TYPE | TYPEATOM _ ->
          let _v_0 = _menhir_action_127 () in
          _menhir_run_341 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState340 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_195 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState195 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | BYVAL ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BYRES ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BYREF ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          let _v = _menhir_action_157 () in
          _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | IDENT _ ->
          _menhir_reduce_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_196 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_160 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_mode_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_param_mode_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState199
      | _ ->
          _eRR ()
  
  and _menhir_run_197 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_161 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_198 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_159 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list_opt : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_128 _2 in
      _menhir_goto_formal_params_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_formal_params_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState340 ->
          _menhir_run_341 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState318 ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState339 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState194 ->
          _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_341 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState341
      | TYPE ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState341
      | IDENT _v_1 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState341
      | COLON ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState341
      | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
          let _v_2 = _menhir_action_206 () in
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState341 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_137 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _2) = (_v, _v_0) in
          let _v = _menhir_action_203 _1 _2 in
          _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA | INLINE | PARALLEL | REC | RENT | RPAREN | SEMI | STATIC | WITH ->
          let _1 = _v in
          let _v = _menhir_action_204 _1 in
          _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState341 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState227 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState213 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState200 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_202 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_207 _1 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_spec_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState341 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState227 ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState213 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState200 ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState153 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_342 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_215 () in
      _menhir_run_343 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState342 _tok
  
  and _menhir_run_228 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | SEMI ->
          let _v_0 = _menhir_action_049 () in
          _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WITH (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState160 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATIC ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REP ->
              _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RENT ->
              _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REC ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | POS ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARALLEL ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OVERLAY ->
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIKE ->
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INSTANCE ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INLINE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEFAULT ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_161 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_035 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_attr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState160 ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_187 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_047 _1 in
      _menhir_goto_decl_attr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_attr_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_WITH (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_050 _3 in
          _menhir_goto_decl_attrs_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_decl_attr_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState185 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATIC ->
              _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REP ->
              _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RENT ->
              _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REC ->
              _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | POS ->
              _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARALLEL ->
              _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OVERLAY ->
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIKE ->
              _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INSTANCE ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INLINE ->
              _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEFAULT ->
              _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT ->
              _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl_attrs_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState247 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState228 ->
          _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_248 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_051 () in
              _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState250 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v = _menhir_action_001 () in
              _menhir_goto_block_decl_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_251 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | TABLE ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | REF ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | OVERLAY ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | LABEL ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | ITEM ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState251, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_170 () in
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState252 _tok
      | DEFINE ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | DEF ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | BLOCK ->
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
      | _ ->
          _eRR ()
  
  and _menhir_run_142 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TABLE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState142 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_192 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState192 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_193 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PROC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState193, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState194
          | SEMI ->
              let _v_0 = _menhir_action_127 () in
              _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_209 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_PROC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_137 _2 _3 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_linkage_target : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState315 ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState243 ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState192 ->
          _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_244 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEF -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_DEF (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_134 _2 in
          _menhir_goto_linkage_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_linkage_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_033 _1 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState351 ->
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState004 ->
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState276 ->
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState251 ->
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_350 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_197 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_270 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_003 () in
      _menhir_goto_block_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_block_item : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState119 ->
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_278 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_006 _1 in
      _menhir_goto_block_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_block_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | TYPE ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | TABLE ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | STOP ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | SEMI ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | RETURN ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | REF ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | OVERLAY ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | LABEL ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | ITEM ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | IF ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState276
      | GOTO ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | FOR ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | EXIT ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | DEFINE ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | DEF ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | CASE ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | BLOCK ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | BEGIN ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | ABORT ->
          let _menhir_stack = MenhirCell1_block_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | END ->
          let _1 = _v in
          let _v = _menhir_action_009 _1 in
          _menhir_goto_block_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_258 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_190 () in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState076
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState076
      | SEMI ->
          let _v = _menhir_action_123 () in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_RETURN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_184 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_217 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OVERLAY (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState217 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_220 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LABEL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState220 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_226 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ITEM (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState226 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_079 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState079 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_082 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
              | STRING _v_0 ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState084
              | NULL ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
              | NOT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
              | MINUS ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
              | LPAREN ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
              | INT _v_1 ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState084
              | IDENT _v_2 ->
                  _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState084
              | FLOAT _v_3 ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState084
              | FALSE ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
              | BEAD _v_4 ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState084
              | RPAREN ->
                  let _v_5 = _menhir_action_121 () in
                  _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState087
          | STRING _v_6 ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState087
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState087
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState087
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState087
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState087
          | INT _v_7 ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState087
          | IDENT _v_8 ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState087
          | FLOAT _v_9 ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9 MenhirState087
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState087
          | BEAD _v_10 ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10 MenhirState087
          | RPAREN ->
              let _v_11 = _menhir_action_121 () in
              _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
          | _ ->
              _eRR ())
      | COLON ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState090 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SEMI ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _v = _menhir_action_012 () in
          _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQUAL ->
          let _1 = _v in
          let _v = _menhir_action_139 _1 in
          _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _3 = _v in
      let _v = _menhir_action_014 _3 in
      _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_call_args_opt : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_015 _1 _2 in
          let _1 = _v in
          let _v = _menhir_action_178 _1 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _2 = _v in
          let _v = _menhir_action_013 _2 in
          _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQUAL ->
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_140 _1 _3 in
          _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_lvalue : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState265 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_190 () in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_091 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _2 = _v in
              let _v = _menhir_action_183 _2 in
              _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FOR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _menhir_s = MenhirState096 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING _v ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NULL ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FLOAT _v ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FALSE ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BEAD _v ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_103 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_188 () in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EXIT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_084 _1 in
          _menhir_goto_exit_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | SEMI ->
          let _v = _menhir_action_085 () in
          _menhir_goto_exit_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_exit_name_opt : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_EXIT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_EXIT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_185 _2 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_109 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState109 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_119 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState119 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TYPE ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TABLE ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEMI ->
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | REF ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OVERLAY ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LABEL ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ITEM ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DEFINE ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DEF ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BLOCK ->
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | END ->
          let _v = _menhir_action_008 () in
          _menhir_goto_block_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_234 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFINE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState234 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_243 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_DEF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState243 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_210 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_136 _1 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_211 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState211, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
          | COLON | IDENT _ | SEMI | TYPE | TYPEATOM _ ->
              let _v_0 = _menhir_action_127 () in
              _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState212 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_213 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState213
      | TYPE ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | IDENT _v_1 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState213
      | COLON ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | SEMI ->
          let _v_2 = _menhir_action_206 () in
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_154 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState154 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_205 _1 in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_156 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COLON (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState156 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_214 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_IDENT (_menhir_stack, _, _2, _, _) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_138 _2 _3 _4 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_246 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BLOCK (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState246 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_259 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ABORT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState259
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState259
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState259
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState259
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState259
      | SEMI ->
          let _v = _menhir_action_123 () in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_260 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ABORT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ABORT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_187 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_block_list_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_block_list_opt (_menhir_stack, _menhir_s, _v) in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_END (_menhir_stack, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v_0 = _menhir_action_170 () in
      _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState274 _tok
  
  and _menhir_run_275 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_block_list_opt _menhir_cell0_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | ELSE | ELSIF | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let MenhirCell0_END (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_block_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_189 _2 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_233 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_semis_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_171 () in
      _menhir_goto_semis_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_semis_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState330 ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_326 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState313 ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState274 ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState252 ->
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState231 ->
          _menhir_run_232 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_331 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END _menhir_cell0_proc_end_name_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell0_proc_end_name_opt (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_END (_menhir_stack, _menhir_s, _endpos_e_) = _menhir_stack in
          let e = () in
          let _v = _menhir_action_167 _endpos_e_ e in
          _menhir_goto_proc_end _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_end : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState345 ->
          _menhir_run_346 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState327 ->
          _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_346 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc_body_opt (_menhir_stack, _, _8) = _menhir_stack in
      let MenhirCell1_use_attrs_opt (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_type_spec_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_DEF (_menhir_stack, _, _startpos__1_) = _menhir_stack in
      let _9 = _v in
      let _v = _menhir_action_166 _3 _4 _5 _6 _8 _9 _startpos__1_ in
      _menhir_goto_proc_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_proc_def : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_198 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_332 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc_body_opt (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_use_attrs_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_PROC (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_DEF (_menhir_stack, _, _startpos__1_) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_165 _3 _4 _5 _7 _8 _startpos__1_ in
      _menhir_goto_proc_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_326 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_stmt_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_164 _2 in
          _menhir_goto_proc_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_body_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState344 ->
          _menhir_run_345 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState322 ->
          _menhir_run_327 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_345 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_proc_body_opt (_menhir_stack, _menhir_s, _v) in
      _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState345
  
  and _menhir_run_328 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_END (_menhir_stack, _menhir_s, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_168 _1 in
          _menhir_goto_proc_end_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_169 () in
          _menhir_goto_proc_end_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_end_name_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_proc_end_name_opt (_menhir_stack, _v) in
      let _v_0 = _menhir_action_170 () in
      _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState330 _tok
  
  and _menhir_run_327 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_proc_body_opt (_menhir_stack, _menhir_s, _v) in
      _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
  
  and _menhir_run_314 : type  ttv_stack. ((((ttv_stack _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TERM ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_top_items_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _) = _menhir_stack in
          let _v = _menhir_action_142 _2 in
          _menhir_goto_module_body _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_253 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_decl_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let _v = _menhir_action_002 _2 in
          _menhir_goto_block_decl_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_block_decl_body_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attrs_opt (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_BLOCK (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_027 _2 _3 _5 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_232 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_decl_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let _v = _menhir_action_194 _2 in
          _menhir_goto_table_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_table_body_opt : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attrs_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_type_spec_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_table_dims_opt (_menhir_stack, _3) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_TABLE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_026 _2 _3 _4 _5 _7 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_277 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_block_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_block_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_007 _1 _2 in
      _menhir_goto_block_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_255 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_052 _1 _2 in
      _menhir_goto_decl_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_list_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState250 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_191 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | TABLE ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | REF ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | OVERLAY ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | LABEL ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | ITEM ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState191, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_170 () in
          _menhir_run_232 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState231 _tok
      | DEFINE ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | DEF ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | BLOCK ->
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | _ ->
          _eRR ()
  
  and _menhir_run_215 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_REF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_135 _2 in
          _menhir_goto_linkage_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_229 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_spec_opt (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_ident_list (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_ITEM (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_025 _2 _3 _4 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_188 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_051 () in
              _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState190 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v = _menhir_action_193 () in
              _menhir_goto_table_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_162 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState163 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_166 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_044 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_167 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_043 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_168 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_POS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState169 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_172 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_046 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_173 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_041 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_175 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_038 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_042 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_179 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_045 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_180 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState180 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_182 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_036 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_186 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_cell1_decl_attr_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attr_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_048 _1 _3 in
      _menhir_goto_decl_attr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_201 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_param_mode_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_154 _1 _2 _3 in
      _menhir_goto_param _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState195 ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState206 ->
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_208 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_155 _1 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState206 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BYVAL ->
              _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BYRES ->
              _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BYREF ->
              _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _ ->
              _menhir_reduce_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_158 _1 in
          _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_reduce_162 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_162 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_207 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_param_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_param_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_156 _1 _3 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_158 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | SEMI ->
          let _v_0 = _menhir_action_049 () in
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState158 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_COLON -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COLON (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_208 _2 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_155 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_209 _2 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_140 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_029 _2 _4 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_319 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_215 () in
      _menhir_run_320 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState319 _tok
  
  and _menhir_run_320 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              _menhir_run_323 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState322
          | END ->
              let _v_0 = _menhir_action_163 () in
              _menhir_run_327 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState322
          | _ ->
              _eRR ())
      | RENT ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_334 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_336 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_333 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_211 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_334 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_210 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_335 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_213 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_336 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_214 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_318 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState318
      | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
          let _v_0 = _menhir_action_127 () in
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState318 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_247 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
      | SEMI ->
          let _v_0 = _menhir_action_049 () in
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState247 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_235 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState235
      | STRING _v_0 ->
          _menhir_run_236 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState235
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState235
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState235
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState235
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState235
      | INT _v_1 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState235
      | IDENT _v_2 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState235
      | FLOAT _v_3 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState235
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState235
      | EQUAL ->
          let _menhir_stack = MenhirCell1_EQUAL (_menhir_stack, MenhirState235) in
          let _menhir_s = MenhirState237 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_236 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | BEAD _v_9 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9 MenhirState235
      | _ ->
          _eRR ()
  
  and _menhir_run_236 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_053 _1 in
          _menhir_goto_define_rhs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AND | EQ | EQUAL | EQV | GE | GQ | GR | GT | LE | LQ | LS | LT | MINUS | MOD | NEQ | NQ | OR | PLUS | SLASH | STAR | XOR ->
          let _1 = _v in
          let _v = _menhir_action_088 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_define_rhs : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState235 ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState237 ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_241 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_DEFINE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_031 _2 _3 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_239 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_EQUAL -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_EQUAL (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_DEFINE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_032 _2 _4 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_225 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_COMMA -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_130 _1 _3 in
      _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState226 ->
          _menhir_run_227 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState220 ->
          _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_227 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState227
      | TYPE ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
      | IDENT _v_1 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState227
      | COMMA ->
          _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
      | COLON ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState227
      | SEMI | WITH ->
          let _v_2 = _menhir_action_206 () in
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState227 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_224 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_list as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState224 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_222 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LABEL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LABEL (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_034 _2 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState222
      | _ ->
          _eRR ()
  
  and _menhir_run_221 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_129 _1 in
      _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_218 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_OVERLAY -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_OVERLAY (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_030 _2 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_200 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState200
      | TYPE ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState200
      | IDENT _v_1 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState200
      | COLON ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState200
      | COMMA | RPAREN ->
          let _v_2 = _menhir_action_206 () in
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState144 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLON | IDENT _ | SEMI | TYPE | TYPEATOM _ | WITH ->
          let _v = _menhir_action_195 () in
          _menhir_goto_table_dims_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_055 () in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dim : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState144 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_152 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_058 _1 in
      _menhir_goto_dim_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dim_list : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_196 _2 in
          _menhir_goto_table_dims_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_dim_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState150 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_table_dims_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_table_dims_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState153
      | TYPE ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | IDENT _v_1 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState153
      | COLON ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | SEMI | WITH ->
          let _v_2 = _menhir_action_206 () in
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState153 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_146 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_147 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_057 _1 in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_151 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_dim_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_dim_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_059 _1 _3 in
      _menhir_goto_dim_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_122 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATUS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState124 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEATOM _ ->
                  _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | IDENT _v ->
                  _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_s = MenhirState136 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEATOM _v ->
              _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RPAREN ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _3 = _v_0 in
                  let _v = _menhir_action_173 _3 in
                  _menhir_goto_status_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_status_item : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState124 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState133 ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_135 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_174 _1 in
      _menhir_goto_status_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_status_list : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
              let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
              let _5 = _v in
              let _v = _menhir_action_028 _2 _5 in
              _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | COMMA ->
          let _menhir_stack = MenhirCell1_status_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState133 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEATOM _ ->
              _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_172 _1 in
      _menhir_goto_status_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_134 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_status_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_status_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_175 _1 _3 in
      _menhir_goto_status_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_312 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_200 () in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_315 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_DEF (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          let _menhir_stack = MenhirCell1_PROC (_menhir_stack, MenhirState315) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState316, _v, _startpos_0, _endpos) in
                  _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
              | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
                  let _v =
                    let (_1, _endpos__1_, _startpos__1_) = (_v, _endpos, _startpos_0) in
                    _menhir_action_131 _1 _endpos__1_ _startpos__1_
                  in
                  _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState316 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | IDENT _v ->
          _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState315
      | FUNCTION ->
          let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, MenhirState315) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _startpos_1 = _menhir_lexbuf.Lexing.lex_start_p in
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, MenhirState338, _v, _startpos_1, _endpos) in
                  _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState339
              | COLON | IDENT _ | INLINE | PARALLEL | REC | RENT | SEMI | STATIC | TYPE | TYPEATOM _ ->
                  let _v =
                    let (_1, _endpos__1_, _startpos__1_) = (_v, _endpos, _startpos_1) in
                    _menhir_action_131 _1 _endpos__1_ _startpos__1_
                  in
                  _menhir_run_340 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState338 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. ((ttv_stack _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | TYPE ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | TABLE ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | SEMI ->
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | REF ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | OVERLAY ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | LABEL ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | ITEM ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState004
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState004, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_170 () in
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState313 _tok
      | DEFINE ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | DEF ->
          _menhir_run_315 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | BLOCK ->
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | _ ->
          _eRR ()
  
  and _menhir_run_311 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_217 _2 _3 in
      let _1 = _v in
      let _v = _menhir_action_180 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_308 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ELSIF (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_elsif_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_083 _1 _3 _5 in
      _menhir_goto_elsif_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_elsif_list : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_elsif_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSIF ->
          let _menhir_stack = MenhirCell1_ELSIF (_menhir_stack, MenhirState304) in
          let _menhir_s = MenhirState305 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ELSE ->
          _menhir_run_302 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState304
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let _v_5 = _menhir_action_080 () in
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_302 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ELSE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState302 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_309 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_elsif_list (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_stmt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_133 _2 _4 _5 _6 in
      _menhir_goto_if_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_if_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_179 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_303 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ELSE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ELSE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_081 _2 in
      _menhir_goto_else_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_else_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState297 ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState304 ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_310 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_132 _2 _4 _5 in
      _menhir_goto_if_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_301 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_ELSIF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_082 _2 _4 in
      _menhir_goto_elsif_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_297 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSIF ->
          let _menhir_stack = MenhirCell1_ELSIF (_menhir_stack, MenhirState297) in
          let _menhir_s = MenhirState298 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ELSE ->
          _menhir_run_302 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState297
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let _v_5 = _menhir_action_080 () in
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_294 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1, _, _) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_176 _1 _3 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_293 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_by_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2, _, _) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_126 _2 _4 _5 _7 _8 in
      _menhir_goto_for_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_for_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_181 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_289 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_by_opt (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_TO (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2, _, _) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_125 _2 _4 _6 _7 _8 in
      _menhir_goto_for_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_279 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_192 _1 _2 in
      _menhir_goto_stmt_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt_list_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState323 ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_283 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | STOP ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | SEMI ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | RETURN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | IF ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState283
      | GOTO ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | FOR ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | EXIT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | CASE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | BEGIN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | ABORT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | END ->
          let MenhirCell1_OTHERWISE (_menhir_stack, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_153 _3 in
          _menhir_goto_otherwise_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_otherwise_opt : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_case_clauses (_menhir_stack, _, _4) = _menhir_stack in
          let MenhirCell1_OF (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_CASE (_menhir_stack, _menhir_s) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_021 _2 _4 _5 in
          let _1 = _v in
          let _v = _menhir_action_182 _1 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_118 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | STOP ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | SEMI ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | RETURN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | IF ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState118
      | GOTO ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | FOR ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | EXIT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | CASE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | BEGIN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | ABORT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | END | OTHERWISE | WHEN ->
          let MenhirCell1_case_labels (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_WHEN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_016 _2 _4 in
          _menhir_goto_case_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_case_clause : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState111 ->
          _menhir_run_288 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState280 ->
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_288 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_017 _1 in
      _menhir_goto_case_clauses _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_case_clauses : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_case_clauses (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHEN ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState280
      | OTHERWISE ->
          let _menhir_stack = MenhirCell1_OTHERWISE (_menhir_stack, MenhirState280) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_191 () in
              _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState282 _tok
          | _ ->
              _eRR ())
      | END ->
          let _v = _menhir_action_152 () in
          _menhir_goto_otherwise_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_112 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHEN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState112 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_287 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_case_clauses (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_018 _1 _2 in
      _menhir_goto_case_clauses _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_263 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_004 _1 in
      _menhir_goto_block_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_025 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState025 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SLASH (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState027 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState061 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState033 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NEQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MOD (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState031 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState035 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState039 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState041 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState043 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState045 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState047 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState049 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState051 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState053 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQV (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState063 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQUAL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState055 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState057 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AND (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState059 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_299 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState299) in
          let _menhir_s = MenhirState300 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | _ ->
          _eRR ()
  
  and _menhir_run_292 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState292
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState292
      | _ ->
          _eRR ()
  
  and _menhir_run_266 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_lvalue (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_177 _1 _3 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState266
      | _ ->
          _eRR ()
  
  and _menhir_run_238 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_054 _1 in
          _menhir_goto_define_rhs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_181 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | COMMA | RPAREN ->
          let MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_037 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_170 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_POS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_POS (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_039 _3 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState170
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_REP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_REP (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_040 _3 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | COLON | COMMA ->
          let MenhirCell1_case_labels (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_020 _1 _3 in
          _menhir_goto_case_labels _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_case_labels : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_case_labels (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState115 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_191 () in
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState117 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_113 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | COLON | COMMA ->
          let _1 = _v in
          let _v = _menhir_action_019 _1 in
          _menhir_goto_case_labels _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | OF ->
          let _menhir_stack = MenhirCell1_OF (_menhir_stack, MenhirState110) in
          let _menhir_s = MenhirState111 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHEN ->
              _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_BY as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | ABORT | BEGIN | CASE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | RETURN | SEMI | STOP | WHILE ->
          let MenhirCell1_BY (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_011 _2 in
          _menhir_goto_by_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_by_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState097 ->
          _menhir_run_290 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_290 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_by_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_s = MenhirState291 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_by_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState102
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_099 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | BY ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | ABORT | BEGIN | CASE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | RETURN | SEMI | STOP | WHILE ->
          let _v_0 = _menhir_action_010 () in
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState099 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_100 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BY (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState100 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | TO ->
          let _menhir_stack = MenhirCell1_TO (_menhir_stack, MenhirState097) in
          let _menhir_s = MenhirState098 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | BY ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState097
      | WHILE ->
          let _v_5 = _menhir_action_010 () in
          _menhir_run_290 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState097 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState080) in
          let _menhir_s = MenhirState081 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
      | _ ->
          _eRR ()
  
  and _menhir_run_074 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState074
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_124 _1 in
          _menhir_goto_expr_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState259 ->
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState076 ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState071 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_070 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState070
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | ABORT ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_NOT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState069
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_097 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_096 _2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_066 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_095 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | COMMA | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_119 _1 in
          _menhir_goto_expr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState021 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_122 _1 in
          _menhir_goto_expr_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_expr_list_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState014 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_064 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQV as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | ABORT | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_EQV (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_118 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState062
      | ABORT | BEGIN | BY | CASE | COLON | COMMA | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE ->
          let MenhirCell1_OR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_116 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_AND as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_AND (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_115 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState058
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_109 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQUAL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState056
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_EQUAL (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_108 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_106 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState052
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_114 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState050
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_113 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState048
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_105 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState046
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState046
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState046
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState046
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState046
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_104 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState044
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_112 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState042
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_111 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState040
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_103 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NEQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState038
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_NEQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_107 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_102 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_NQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_110 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MOD -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MOD (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_100 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_030 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_PLUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_PLUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_101 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_SLASH -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SLASH (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_099 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_STAR -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_STAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_098 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_024 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_XOR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState024
      | ABORT | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_XOR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_117 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_022 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState022
      | COMMA | RPAREN ->
          let MenhirCell1_expr_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_120 _1 _3 in
          _menhir_goto_expr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_compool_includes_opt : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | ICOMPOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMI ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let (_1, _3) = (_v, _v_0) in
                  let _v = _menhir_action_024 _1 _3 in
                  _menhir_goto_compool_includes_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_use_attrs_opt (_menhir_stack, _, _4) = _menhir_stack in
          let MenhirCell0_module_name_opt (_menhir_stack, _3) = _menhir_stack in
          let MenhirCell0_module_kind_opt (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell0_directives_opt (_menhir_stack, _1) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_144 _1 _2 _3 _4 _5 in
          let _menhir_stack = MenhirCell0_module_header (_menhir_stack, _v) in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, MenhirState002) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_201 () in
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v_1 = _menhir_action_201 () in
              _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState002 _tok
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  let _menhir_goto_module_name_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_module_name_opt (_menhir_stack, _v) in
      let _v_0 = _menhir_action_215 () in
      _menhir_run_384 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState383 _tok
  
  let _menhir_goto_module_kind_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_module_kind_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _endpos__1_, _startpos__1_) = (_v_0, _endpos, _startpos) in
          let _v = _menhir_action_150 _1 _endpos__1_ _startpos__1_ in
          _menhir_goto_module_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IF | INLINE | ITEM | LABEL | OVERLAY | PARALLEL | REC | REF | RENT | RETURN | SEMI | STATIC | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_151 () in
          _menhir_goto_module_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_goto_directives_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_directives_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PROGRAM ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_145 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PROC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_147 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUNCTION ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_148 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMPOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_146 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DIRECTIVE_NAME _v ->
              let _menhir_stack = MenhirCell0_DIRECTIVE_NAME (_menhir_stack, _v) in
              let _menhir_s = MenhirState360 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING _v ->
                  _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NULL ->
                  _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
                  let _menhir_s = MenhirState364 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TRUE ->
                      _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | STRING _v ->
                      _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | NULL ->
                      _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IDENT _v ->
                      _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FLOAT _v ->
                      _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FALSE ->
                      _menhir_run_368 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | BEAD _v ->
                      _menhir_run_369 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | RPAREN ->
                      let _v = _menhir_action_071 () in
                      _menhir_goto_directive_arg_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
                  | _ ->
                      _eRR ())
              | INT _v ->
                  _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FLOAT _v ->
                  _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FALSE ->
                  _menhir_run_368 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BEAD _v ->
                  _menhir_run_369 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEMI ->
                  let _v = _menhir_action_075 () in
                  _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IDENT _ | IF | INLINE | ITEM | LABEL | OVERLAY | PARALLEL | REC | REF | RENT | RETURN | SEMI | STATIC | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_149 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_361 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_066 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_arg : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState360 ->
          _menhir_run_380 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState378 ->
          _menhir_run_379 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState364 ->
          _menhir_run_375 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState373 ->
          _menhir_run_374 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_380 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_073 _1 in
      _menhir_goto_directive_args _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_args : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState378
      | STRING _v_0 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState378
      | NULL ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState378
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState378
      | IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState378
      | FLOAT _v_3 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState378
      | FALSE ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_368 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState378
      | BEAD _v_4 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_369 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState378
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_076 _1 in
          _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_362 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_061 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_363 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_065 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_365 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_062 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_366 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_367 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_063 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_368 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_067 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_369 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_064 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_args_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_DIRECTIVE_NAME (_menhir_stack, _2) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_060 _2 _3 in
          let MenhirCell0_directives_opt (_menhir_stack, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_079 _1 _2 in
          _menhir_goto_directives_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_379 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_directive_args -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_directive_args (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_074 _1 _2 in
      _menhir_goto_directive_args _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_375 : type  ttv_stack. ((ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_069 _1 in
      _menhir_goto_directive_arg_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_arg_list : type  ttv_stack. ((ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_directive_arg_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState373 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_367 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_368 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_369 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_072 _1 in
          _menhir_goto_directive_arg_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_directive_arg_list_opt : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_077 _2 in
      _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_374 : type  ttv_stack. ((ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_directive_arg_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_directive_arg_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_070 _1 _3 in
      _menhir_goto_directive_arg_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | START ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_078 () in
          _menhir_goto_directives_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
end

let compilation_unit =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_compilation_unit v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
