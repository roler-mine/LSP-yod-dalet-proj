
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
# 36 "lib/parser.mly"
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
# 32 "lib/parser.mly"
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
# 33 "lib/parser.mly"
       (int)
# 73 "lib/parser.ml"
  )
    | INSTANCE
    | INLINE
    | IF
    | IDENT of (
# 31 "lib/parser.mly"
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
# 34 "lib/parser.mly"
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
# 29 "lib/parser.mly"
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
# 35 "lib/parser.mly"
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

# 142 "lib/parser.ml"

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

  | MenhirState120 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 120.
        Stack shape : BEGIN stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState121 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 121.
        Stack shape : BEGIN stmt_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState124 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ABORT, _menhir_box_compilation_unit) _menhir_state
    (** State 124.
        Stack shape : ABORT.
        Start symbol: compilation_unit. *)

  | MenhirState130 : (('s, _menhir_box_compilation_unit) _menhir_cell1_lvalue, _menhir_box_compilation_unit) _menhir_state
    (** State 130.
        Stack shape : lvalue.
        Start symbol: compilation_unit. *)

  | MenhirState131 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_lvalue, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 131.
        Stack shape : lvalue expr.
        Start symbol: compilation_unit. *)

  | MenhirState137 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_state
    (** State 137.
        Stack shape : CASE expr OF case_clauses.
        Start symbol: compilation_unit. *)

  | MenhirState139 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE, _menhir_box_compilation_unit) _menhir_state
    (** State 139.
        Stack shape : CASE expr OF case_clauses OTHERWISE.
        Start symbol: compilation_unit. *)

  | MenhirState140 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 140.
        Stack shape : CASE expr OF case_clauses OTHERWISE stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState148 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 148.
        Stack shape : FOR IDENT expr by_opt.
        Start symbol: compilation_unit. *)

  | MenhirState149 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 149.
        Stack shape : FOR IDENT expr by_opt expr.
        Start symbol: compilation_unit. *)

  | MenhirState154 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_state
    (** State 154.
        Stack shape : IF expr THEN stmt.
        Start symbol: compilation_unit. *)

  | MenhirState155 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_state
    (** State 155.
        Stack shape : IF expr THEN stmt ELSIF.
        Start symbol: compilation_unit. *)

  | MenhirState156 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 156.
        Stack shape : IF expr THEN stmt ELSIF expr.
        Start symbol: compilation_unit. *)

  | MenhirState157 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 157.
        Stack shape : IF expr THEN stmt ELSIF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState159 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ELSE, _menhir_box_compilation_unit) _menhir_state
    (** State 159.
        Stack shape : ELSE.
        Start symbol: compilation_unit. *)

  | MenhirState161 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_state
    (** State 161.
        Stack shape : IF expr THEN stmt elsif_list.
        Start symbol: compilation_unit. *)

  | MenhirState162 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_state
    (** State 162.
        Stack shape : IF expr THEN stmt elsif_list ELSIF.
        Start symbol: compilation_unit. *)

  | MenhirState163 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 163.
        Stack shape : IF expr THEN stmt elsif_list ELSIF expr.
        Start symbol: compilation_unit. *)

  | MenhirState164 : ((((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 164.
        Stack shape : IF expr THEN stmt elsif_list ELSIF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState172 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 172.
        Stack shape : TYPE IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState181 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_status_list, _menhir_box_compilation_unit) _menhir_state
    (** State 181.
        Stack shape : TYPE IDENT status_list.
        Start symbol: compilation_unit. *)

  | MenhirState184 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 184.
        Stack shape : TYPE IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState191 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 191.
        Stack shape : TABLE IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState197 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_dim_list, _menhir_box_compilation_unit) _menhir_state
    (** State 197.
        Stack shape : TABLE IDENT dim_list.
        Start symbol: compilation_unit. *)

  | MenhirState200 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 200.
        Stack shape : TABLE IDENT table_dims_opt.
        Start symbol: compilation_unit. *)

  | MenhirState201 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_state
    (** State 201.
        Stack shape : TYPE.
        Start symbol: compilation_unit. *)

  | MenhirState203 : (('s, _menhir_box_compilation_unit) _menhir_cell1_COLON, _menhir_box_compilation_unit) _menhir_state
    (** State 203.
        Stack shape : COLON.
        Start symbol: compilation_unit. *)

  | MenhirState205 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 205.
        Stack shape : TABLE IDENT table_dims_opt type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState207 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_state
    (** State 207.
        Stack shape : WITH.
        Start symbol: compilation_unit. *)

  | MenhirState210 : (('s, _menhir_box_compilation_unit) _menhir_cell1_REP, _menhir_box_compilation_unit) _menhir_state
    (** State 210.
        Stack shape : REP.
        Start symbol: compilation_unit. *)

  | MenhirState211 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_REP, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 211.
        Stack shape : REP expr.
        Start symbol: compilation_unit. *)

  | MenhirState216 : (('s, _menhir_box_compilation_unit) _menhir_cell1_POS, _menhir_box_compilation_unit) _menhir_state
    (** State 216.
        Stack shape : POS.
        Start symbol: compilation_unit. *)

  | MenhirState217 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_POS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 217.
        Stack shape : POS expr.
        Start symbol: compilation_unit. *)

  | MenhirState227 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT, _menhir_box_compilation_unit) _menhir_state
    (** State 227.
        Stack shape : DEFAULT.
        Start symbol: compilation_unit. *)

  | MenhirState228 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 228.
        Stack shape : DEFAULT expr.
        Start symbol: compilation_unit. *)

  | MenhirState232 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_cell1_decl_attr_list, _menhir_box_compilation_unit) _menhir_state
    (** State 232.
        Stack shape : WITH decl_attr_list.
        Start symbol: compilation_unit. *)

  | MenhirState237 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 237.
        Stack shape : TABLE IDENT table_dims_opt type_spec_opt decl_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState238 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 238.
        Stack shape : TABLE IDENT table_dims_opt type_spec_opt decl_attrs_opt decl_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState239 : (('s, _menhir_box_compilation_unit) _menhir_cell1_REF, _menhir_box_compilation_unit) _menhir_state
    (** State 239.
        Stack shape : REF.
        Start symbol: compilation_unit. *)

  | MenhirState241 : (('s, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 241.
        Stack shape : PROC IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState242 : (('s _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_state
    (** State 242.
        Stack shape : IDENT LPAREN.
        Start symbol: compilation_unit. *)

  | MenhirState247 : (('s, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 247.
        Stack shape : param_mode_opt IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState252 : ((('s _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_param_list, _menhir_box_compilation_unit) _menhir_state
    (** State 252.
        Stack shape : IDENT LPAREN param_list.
        Start symbol: compilation_unit. *)

  | MenhirState258 : (('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 258.
        Stack shape : FUNCTION IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState259 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 259.
        Stack shape : FUNCTION IDENT formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState266 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LABEL, _menhir_box_compilation_unit) _menhir_state
    (** State 266.
        Stack shape : LABEL.
        Start symbol: compilation_unit. *)

  | MenhirState268 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_LABEL, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_state
    (** State 268.
        Stack shape : LABEL ident_list.
        Start symbol: compilation_unit. *)

  | MenhirState272 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_state
    (** State 272.
        Stack shape : ITEM.
        Start symbol: compilation_unit. *)

  | MenhirState273 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_state
    (** State 273.
        Stack shape : ITEM ident_list.
        Start symbol: compilation_unit. *)

  | MenhirState274 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 274.
        Stack shape : ITEM ident_list type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState277 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 277.
        Stack shape : TABLE IDENT table_dims_opt type_spec_opt decl_attrs_opt decl_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState281 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 281.
        Stack shape : DEFINE IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState283 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 283.
        Stack shape : DEFINE IDENT expr.
        Start symbol: compilation_unit. *)

  | MenhirState286 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_state
    (** State 286.
        Stack shape : decl_attrs_opt decl_list_opt DEF.
        Start symbol: compilation_unit. *)

  | MenhirState290 : (('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 290.
        Stack shape : BLOCK IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState293 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 293.
        Stack shape : BLOCK IDENT decl_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState294 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 294.
        Stack shape : BLOCK IDENT decl_attrs_opt decl_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState295 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 295.
        Stack shape : BLOCK IDENT decl_attrs_opt decl_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState302 : (((('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 302.
        Stack shape : module_header BEGIN top_items_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState304 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_state
    (** State 304.
        Stack shape : top_items_opt DEF.
        Start symbol: compilation_unit. *)

  | MenhirState306 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 306.
        Stack shape : top_items_opt DEF PROC IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState307 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 307.
        Stack shape : top_items_opt DEF PROC IDENT formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState310 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 310.
        Stack shape : top_items_opt DEF PROC IDENT formal_params_opt use_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState311 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_state
    (** State 311.
        Stack shape : use_attrs_opt BEGIN.
        Start symbol: compilation_unit. *)

  | MenhirState312 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 312.
        Stack shape : use_attrs_opt BEGIN stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState313 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 313.
        Stack shape : use_attrs_opt BEGIN stmt_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState315 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 315.
        Stack shape : top_items_opt DEF PROC IDENT formal_params_opt use_attrs_opt proc_body_opt.
        Start symbol: compilation_unit. *)

  | MenhirState318 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END _menhir_cell0_proc_end_name_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 318.
        Stack shape : use_attrs_opt proc_body_opt END proc_end_name_opt.
        Start symbol: compilation_unit. *)

  | MenhirState327 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_state
    (** State 327.
        Stack shape : top_items_opt DEF FUNCTION IDENT.
        Start symbol: compilation_unit. *)

  | MenhirState328 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 328.
        Stack shape : top_items_opt DEF FUNCTION IDENT formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState329 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 329.
        Stack shape : top_items_opt DEF FUNCTION IDENT formal_params_opt type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState331 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 331.
        Stack shape : top_items_opt DEF FUNCTION IDENT formal_params_opt type_spec_opt use_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState332 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 332.
        Stack shape : top_items_opt DEF FUNCTION IDENT formal_params_opt type_spec_opt use_attrs_opt proc_body_opt.
        Start symbol: compilation_unit. *)

  | MenhirState338 : (('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 338.
        Stack shape : module_header top_items_opt.
        Start symbol: compilation_unit. *)

  | MenhirState347 : ('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_state
    (** State 347.
        Stack shape : directives_opt DIRECTIVE_NAME.
        Start symbol: compilation_unit. *)

  | MenhirState358 : (('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_directive_args, _menhir_box_compilation_unit) _menhir_state
    (** State 358.
        Stack shape : directives_opt DIRECTIVE_NAME directive_args.
        Start symbol: compilation_unit. *)

  | MenhirState363 : ('s _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 363.
        Stack shape : directives_opt module_kind_opt module_name_opt.
        Start symbol: compilation_unit. *)


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
  | MenhirCell1_ident_list of 's * ('s, 'r) _menhir_state * (string list)

and ('s, 'r) _menhir_cell1_lvalue = 
  | MenhirCell1_lvalue of 's * ('s, 'r) _menhir_state * (Ast.lvalue)

and 's _menhir_cell0_module_header = 
  | MenhirCell0_module_header of 's * (Ast.module_kind * string option * Ast.directive list * Ast.use_attr list)

and 's _menhir_cell0_module_kind_opt = 
  | MenhirCell0_module_kind_opt of 's * (Ast.module_kind)

and 's _menhir_cell0_module_name_opt = 
  | MenhirCell0_module_name_opt of 's * (string option)

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

and ('s, 'r) _menhir_cell1_DEF = 
  | MenhirCell1_DEF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DEFAULT = 
  | MenhirCell1_DEFAULT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DEFINE = 
  | MenhirCell1_DEFINE of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_DIRECTIVE_NAME = 
  | MenhirCell0_DIRECTIVE_NAME of 's * (
# 29 "lib/parser.mly"
       (string)
# 1050 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_ELSE = 
  | MenhirCell1_ELSE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ELSIF = 
  | MenhirCell1_ELSIF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_END = 
  | MenhirCell1_END of 's * ('s, 'r) _menhir_state

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
# 31 "lib/parser.mly"
       (string)
# 1096 "lib/parser.ml"
)

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 31 "lib/parser.mly"
       (string)
# 1103 "lib/parser.ml"
)

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
# 197 "lib/parser.mly"
                ( [] )
# 1213 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_002 =
  fun _2 ->
    (
# 198 "lib/parser.mly"
                                      ( _2 )
# 1221 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_003 =
  fun () ->
    (
# 395 "lib/parser.mly"
                ( None )
# 1229 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_004 =
  fun _2 ->
    (
# 396 "lib/parser.mly"
            ( Some _2 )
# 1237 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_005 =
  fun () ->
    (
# 359 "lib/parser.mly"
                ( [] )
# 1245 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_006 =
  fun _2 ->
    (
# 360 "lib/parser.mly"
                                ( _2 )
# 1253 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_007 =
  fun _3 ->
    (
# 361 "lib/parser.mly"
                                     ( _3 )
# 1261 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_008 =
  fun _1 _2 ->
    (
# 356 "lib/parser.mly"
                             ( SCall (_1, _2) )
# 1269 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_009 =
  fun _2 _4 ->
    (
# 407 "lib/parser.mly"
                                         ( (_2, _4) )
# 1277 "lib/parser.ml"
     : (Ast.expr list * Ast.stmt list))

let _menhir_action_010 =
  fun _1 ->
    (
# 403 "lib/parser.mly"
                ( [_1] )
# 1285 "lib/parser.ml"
     : ((Ast.expr list * Ast.stmt list) list))

let _menhir_action_011 =
  fun _1 _2 ->
    (
# 404 "lib/parser.mly"
                             ( _1 @ [_2] )
# 1293 "lib/parser.ml"
     : ((Ast.expr list * Ast.stmt list) list))

let _menhir_action_012 =
  fun _1 ->
    (
# 410 "lib/parser.mly"
         ( [_1] )
# 1301 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_013 =
  fun _1 _3 ->
    (
# 411 "lib/parser.mly"
                           ( _1 @ [_3] )
# 1309 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_014 =
  fun _2 _4 _5 ->
    (
# 400 "lib/parser.mly"
      ( SCase { expr = _2; clauses = _4; otherwise_ = _5 } )
# 1317 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_015 =
  fun _1 ->
    (
# 59 "lib/parser.mly"
              ( _1 )
# 1325 "lib/parser.ml"
     : (Ast.module_))

let _menhir_action_016 =
  fun () ->
    (
# 125 "lib/parser.mly"
                ( [] )
# 1333 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_017 =
  fun _1 _3 ->
    (
# 127 "lib/parser.mly"
      ( _1 @ [ { d_name = "ICOMPOOL"; d_args = [LString _3] } ] )
# 1341 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_018 =
  fun _2 _3 _4 ->
    (
# 158 "lib/parser.mly"
      ( DItem { names = _2; typ = _3; attrs = _4 } )
# 1349 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_019 =
  fun _2 _3 _4 _5 _7 ->
    (
# 161 "lib/parser.mly"
      ( DTable { name = _2; dims = _3; typ = _4; attrs = _5; body = _7 } )
# 1357 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_020 =
  fun _2 _3 _5 ->
    (
# 164 "lib/parser.mly"
      ( DBlock { name = _2; attrs = _3; body = _5 } )
# 1365 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_021 =
  fun _2 _5 ->
    (
# 167 "lib/parser.mly"
      ( DTypeStatus { name = _2; items = _5 } )
# 1373 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_022 =
  fun _2 _4 ->
    (
# 170 "lib/parser.mly"
      ( DTypeAlias { name = _2; target = _4 } )
# 1381 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_023 =
  fun _2 ->
    (
# 173 "lib/parser.mly"
      ( DOverlayDecl _2 )
# 1389 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_024 =
  fun _2 _4 ->
    (
# 176 "lib/parser.mly"
      ( DDefine { name = _2; rhs = _4 } )
# 1397 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_025 =
  fun _1 ->
    (
# 178 "lib/parser.mly"
                 ( _1 )
# 1405 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_026 =
  fun _2 ->
    (
# 181 "lib/parser.mly"
      ( DLabelDecl _2 )
# 1413 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_027 =
  fun () ->
    (
# 239 "lib/parser.mly"
                           ( DStatic )
# 1421 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_028 =
  fun () ->
    (
# 240 "lib/parser.mly"
                           ( DConstant )
# 1429 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_029 =
  fun _2 ->
    (
# 241 "lib/parser.mly"
                           ( DDefault _2 )
# 1437 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_030 =
  fun _2 ->
    (
# 242 "lib/parser.mly"
                           ( DLike _2 )
# 1445 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_031 =
  fun _3 ->
    (
# 243 "lib/parser.mly"
                           ( DPos _3 )
# 1453 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_032 =
  fun _3 ->
    (
# 244 "lib/parser.mly"
                           ( DRep _3 )
# 1461 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_033 =
  fun _2 ->
    (
# 245 "lib/parser.mly"
                           ( DOverlay _2 )
# 1469 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_034 =
  fun _2 ->
    (
# 246 "lib/parser.mly"
                           ( DInstance _2 )
# 1477 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_035 =
  fun () ->
    (
# 247 "lib/parser.mly"
                           ( DRec )
# 1485 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_036 =
  fun () ->
    (
# 248 "lib/parser.mly"
                           ( DRent )
# 1493 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_037 =
  fun () ->
    (
# 249 "lib/parser.mly"
                           ( DInline )
# 1501 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_038 =
  fun () ->
    (
# 250 "lib/parser.mly"
                           ( DParallel )
# 1509 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_039 =
  fun _1 ->
    (
# 235 "lib/parser.mly"
            ( [_1] )
# 1517 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_040 =
  fun _1 _3 ->
    (
# 236 "lib/parser.mly"
                                 ( _1 @ [_3] )
# 1525 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_041 =
  fun () ->
    (
# 231 "lib/parser.mly"
                ( [] )
# 1533 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_042 =
  fun _3 ->
    (
# 232 "lib/parser.mly"
                                      ( _3 )
# 1541 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_043 =
  fun () ->
    (
# 201 "lib/parser.mly"
                ( [] )
# 1549 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_044 =
  fun _1 _2 ->
    (
# 202 "lib/parser.mly"
                       ( _1 @ [_2] )
# 1557 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_045 =
  fun _1 ->
    (
# 184 "lib/parser.mly"
           ( DefString _1 )
# 1565 "lib/parser.ml"
     : (Ast.define_rhs))

let _menhir_action_046 =
  fun _1 ->
    (
# 185 "lib/parser.mly"
           ( DefExpr _1 )
# 1573 "lib/parser.ml"
     : (Ast.define_rhs))

let _menhir_action_047 =
  fun () ->
    (
# 217 "lib/parser.mly"
          ( DimStar )
# 1581 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_048 =
  fun _1 ->
    (
# 218 "lib/parser.mly"
          ( DimInt _1 )
# 1589 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_049 =
  fun _1 ->
    (
# 219 "lib/parser.mly"
          ( DimId _1 )
# 1597 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_050 =
  fun _1 ->
    (
# 213 "lib/parser.mly"
      ( [_1] )
# 1605 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_051 =
  fun _1 _3 ->
    (
# 214 "lib/parser.mly"
                     ( _1 @ [_3] )
# 1613 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_052 =
  fun _2 _3 ->
    (
# 104 "lib/parser.mly"
    ( { d_name = _2; d_args = _3 } )
# 1621 "lib/parser.ml"
     : (Ast.directive))

let _menhir_action_053 =
  fun _1 ->
    (
# 115 "lib/parser.mly"
         ( LString _1 )
# 1629 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_054 =
  fun _1 ->
    (
# 116 "lib/parser.mly"
         ( LInt _1 )
# 1637 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_055 =
  fun _1 ->
    (
# 117 "lib/parser.mly"
         ( LFloat _1 )
# 1645 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_056 =
  fun _1 ->
    (
# 118 "lib/parser.mly"
         ( let (b,v) = _1 in LBead (b,v) )
# 1653 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_057 =
  fun () ->
    (
# 119 "lib/parser.mly"
         ( LNull )
# 1661 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_058 =
  fun () ->
    (
# 120 "lib/parser.mly"
         ( LBool true )
# 1669 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_059 =
  fun () ->
    (
# 121 "lib/parser.mly"
         ( LBool false )
# 1677 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_060 =
  fun _1 ->
    (
# 122 "lib/parser.mly"
         ( LString _1 )
# 1685 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_061 =
  fun _1 ->
    (
# 111 "lib/parser.mly"
                ( [_1] )
# 1693 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_062 =
  fun _1 _2 ->
    (
# 112 "lib/parser.mly"
                               ( _1 @ [_2] )
# 1701 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_063 =
  fun () ->
    (
# 107 "lib/parser.mly"
                ( [] )
# 1709 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_064 =
  fun _1 ->
    (
# 108 "lib/parser.mly"
                   ( _1 )
# 1717 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_065 =
  fun () ->
    (
# 99 "lib/parser.mly"
                ( [] )
# 1725 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_066 =
  fun _1 _2 ->
    (
# 100 "lib/parser.mly"
                             ( _1 @ [_2] )
# 1733 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_067 =
  fun () ->
    (
# 381 "lib/parser.mly"
                ( None )
# 1741 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_068 =
  fun _2 ->
    (
# 382 "lib/parser.mly"
              ( Some _2 )
# 1749 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_069 =
  fun _2 _4 ->
    (
# 377 "lib/parser.mly"
                         ( [(_2, _4)] )
# 1757 "lib/parser.ml"
     : ((Ast.expr * Ast.stmt) list))

let _menhir_action_070 =
  fun _1 _3 _5 ->
    (
# 378 "lib/parser.mly"
                                    ( _1 @ [(_3, _5)] )
# 1765 "lib/parser.ml"
     : ((Ast.expr * Ast.stmt) list))

let _menhir_action_071 =
  fun _1 ->
    (
# 348 "lib/parser.mly"
          ( Some _1 )
# 1773 "lib/parser.ml"
     : (string option))

let _menhir_action_072 =
  fun () ->
    (
# 349 "lib/parser.mly"
                ( None )
# 1781 "lib/parser.ml"
     : (string option))

let _menhir_action_073 =
  fun _1 ->
    (
# 424 "lib/parser.mly"
           ( EInt _1 )
# 1789 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_074 =
  fun _1 ->
    (
# 425 "lib/parser.mly"
           ( EFloat _1 )
# 1797 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_075 =
  fun _1 ->
    (
# 426 "lib/parser.mly"
           ( EString _1 )
# 1805 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_076 =
  fun _1 ->
    (
# 427 "lib/parser.mly"
           ( let (b,v) = _1 in EBead (b,v) )
# 1813 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_077 =
  fun () ->
    (
# 428 "lib/parser.mly"
           ( ENull )
# 1821 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_078 =
  fun () ->
    (
# 429 "lib/parser.mly"
           ( EBool true )
# 1829 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_079 =
  fun () ->
    (
# 430 "lib/parser.mly"
           ( EBool false )
# 1837 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_080 =
  fun _1 ->
    (
# 431 "lib/parser.mly"
           ( EVar _1 )
# 1845 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_081 =
  fun _1 _3 ->
    (
# 432 "lib/parser.mly"
                                      ( ECall (_1, _3) )
# 1853 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_082 =
  fun _2 ->
    (
# 433 "lib/parser.mly"
                       ( EParen _2 )
# 1861 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_083 =
  fun _2 ->
    (
# 435 "lib/parser.mly"
                            ( EUn ("-", _2) )
# 1869 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_084 =
  fun _2 ->
    (
# 436 "lib/parser.mly"
             ( EUn ("NOT", _2) )
# 1877 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_085 =
  fun _1 _3 ->
    (
# 438 "lib/parser.mly"
                    ( EBin ("*", _1, _3) )
# 1885 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_086 =
  fun _1 _3 ->
    (
# 439 "lib/parser.mly"
                    ( EBin ("/", _1, _3) )
# 1893 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_087 =
  fun _1 _3 ->
    (
# 440 "lib/parser.mly"
                    ( EBin ("MOD", _1, _3) )
# 1901 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_088 =
  fun _1 _3 ->
    (
# 441 "lib/parser.mly"
                    ( EBin ("+", _1, _3) )
# 1909 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_089 =
  fun _1 _3 ->
    (
# 442 "lib/parser.mly"
                    ( EBin ("-", _1, _3) )
# 1917 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_090 =
  fun _1 _3 ->
    (
# 444 "lib/parser.mly"
                    ( EBin ("<", _1, _3) )
# 1925 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_091 =
  fun _1 _3 ->
    (
# 445 "lib/parser.mly"
                    ( EBin ("<=", _1, _3) )
# 1933 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_092 =
  fun _1 _3 ->
    (
# 446 "lib/parser.mly"
                    ( EBin (">", _1, _3) )
# 1941 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_093 =
  fun _1 _3 ->
    (
# 447 "lib/parser.mly"
                    ( EBin (">=", _1, _3) )
# 1949 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_094 =
  fun _1 _3 ->
    (
# 448 "lib/parser.mly"
                    ( EBin ("<>", _1, _3) )
# 1957 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_095 =
  fun _1 _3 ->
    (
# 449 "lib/parser.mly"
                    ( EBin ("=", _1, _3) )
# 1965 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_096 =
  fun _1 _3 ->
    (
# 451 "lib/parser.mly"
                    ( EBin ("EQ", _1, _3) )
# 1973 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_097 =
  fun _1 _3 ->
    (
# 452 "lib/parser.mly"
                    ( EBin ("NQ", _1, _3) )
# 1981 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_098 =
  fun _1 _3 ->
    (
# 453 "lib/parser.mly"
                    ( EBin ("LS", _1, _3) )
# 1989 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_099 =
  fun _1 _3 ->
    (
# 454 "lib/parser.mly"
                    ( EBin ("LQ", _1, _3) )
# 1997 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_100 =
  fun _1 _3 ->
    (
# 455 "lib/parser.mly"
                    ( EBin ("GR", _1, _3) )
# 2005 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_101 =
  fun _1 _3 ->
    (
# 456 "lib/parser.mly"
                    ( EBin ("GQ", _1, _3) )
# 2013 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_102 =
  fun _1 _3 ->
    (
# 458 "lib/parser.mly"
                    ( EBin ("AND", _1, _3) )
# 2021 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_103 =
  fun _1 _3 ->
    (
# 459 "lib/parser.mly"
                    ( EBin ("OR", _1, _3) )
# 2029 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_104 =
  fun _1 _3 ->
    (
# 460 "lib/parser.mly"
                    ( EBin ("XOR", _1, _3) )
# 2037 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_105 =
  fun _1 _3 ->
    (
# 461 "lib/parser.mly"
                    ( EBin ("EQV", _1, _3) )
# 2045 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_106 =
  fun _1 ->
    (
# 368 "lib/parser.mly"
         ( [_1] )
# 2053 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_107 =
  fun _1 _3 ->
    (
# 369 "lib/parser.mly"
                         ( _1 @ [_3] )
# 2061 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_108 =
  fun () ->
    (
# 364 "lib/parser.mly"
                ( [] )
# 2069 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_109 =
  fun _1 ->
    (
# 365 "lib/parser.mly"
              ( _1 )
# 2077 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_110 =
  fun () ->
    (
# 352 "lib/parser.mly"
                ( None )
# 2085 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_111 =
  fun _1 ->
    (
# 353 "lib/parser.mly"
         ( Some _1 )
# 2093 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_112 =
  fun _2 _4 _6 _7 _8 ->
    (
# 389 "lib/parser.mly"
      ( SForTo { var = _2; start_ = _4; stop_ = _6; step = _7; body = _8 } )
# 2101 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_113 =
  fun _2 _4 _5 _7 _8 ->
    (
# 392 "lib/parser.mly"
      ( SForWhile { var = _2; start_ = _4; step = _5; cond = _7; body = _8 } )
# 2109 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_114 =
  fun () ->
    (
# 292 "lib/parser.mly"
                ( [] )
# 2117 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_115 =
  fun _2 ->
    (
# 293 "lib/parser.mly"
                                 ( _2 )
# 2125 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_116 =
  fun _1 ->
    (
# 253 "lib/parser.mly"
          ( [_1] )
# 2133 "lib/parser.ml"
     : (string list))

let _menhir_action_117 =
  fun _1 _3 ->
    (
# 254 "lib/parser.mly"
                           ( _1 @ [_3] )
# 2141 "lib/parser.ml"
     : (string list))

let _menhir_action_118 =
  fun _2 _4 _5 ->
    (
# 372 "lib/parser.mly"
                               ( SIf (_2, _4, _5) )
# 2149 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_119 =
  fun _2 _4 _5 _6 ->
    (
# 374 "lib/parser.mly"
      ( SIfElsif (((_2,_4) :: _5), _6) )
# 2157 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_120 =
  fun _2 ->
    (
# 188 "lib/parser.mly"
                            ( DLinkage { kind = LDef; target = _2 } )
# 2165 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_121 =
  fun _2 ->
    (
# 189 "lib/parser.mly"
                            ( DLinkage { kind = LRef; target = _2 } )
# 2173 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_122 =
  fun _1 ->
    (
# 192 "lib/parser.mly"
          ( LName _1 )
# 2181 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_123 =
  fun _2 _3 ->
    (
# 193 "lib/parser.mly"
                                 ( LProcSig (_2, _3) )
# 2189 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_124 =
  fun _2 _3 _4 ->
    (
# 194 "lib/parser.mly"
                                                   ( LFunSig (_2, _3, _4) )
# 2197 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_125 =
  fun _1 ->
    (
# 418 "lib/parser.mly"
          ( LVar _1 )
# 2205 "lib/parser.ml"
     : (Ast.lvalue))

let _menhir_action_126 =
  fun _1 _3 ->
    (
# 419 "lib/parser.mly"
                                      ( LIndex (_1, _3) )
# 2213 "lib/parser.ml"
     : (Ast.lvalue))

let _menhir_action_127 =
  fun _2 _3 ->
    (
# 63 "lib/parser.mly"
    (
      let (kind, name, directives, attrs) = _2 in
      let (decls, stmts, procs) = _3 in
      ({ kind; name; directives; attrs; decls; stmts; procs } : Ast.module_)
    )
# 2225 "lib/parser.ml"
     : (Ast.module_))

let _menhir_action_128 =
  fun _2 ->
    (
# 130 "lib/parser.mly"
                                      ( _2 )
# 2233 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_129 =
  fun _1 ->
    (
# 131 "lib/parser.mly"
                  ( _1 )
# 2241 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_130 =
  fun _1 _2 _3 _4 _5 ->
    (
# 71 "lib/parser.mly"
    (
      let directives = _1 @ _5 in
      (_2, _3, directives, _4)
    )
# 2252 "lib/parser.ml"
     : (Ast.module_kind * string option * Ast.directive list * Ast.use_attr list))

let _menhir_action_131 =
  fun () ->
    (
# 77 "lib/parser.mly"
            ( Program )
# 2260 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_132 =
  fun () ->
    (
# 78 "lib/parser.mly"
            ( Compool )
# 2268 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_133 =
  fun () ->
    (
# 79 "lib/parser.mly"
            ( Proc_module )
# 2276 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_134 =
  fun () ->
    (
# 80 "lib/parser.mly"
             ( Function_module )
# 2284 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_135 =
  fun () ->
    (
# 81 "lib/parser.mly"
                ( Unknown )
# 2292 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_136 =
  fun _1 ->
    (
# 84 "lib/parser.mly"
          ( Some _1 )
# 2300 "lib/parser.ml"
     : (string option))

let _menhir_action_137 =
  fun () ->
    (
# 85 "lib/parser.mly"
                ( None )
# 2308 "lib/parser.ml"
     : (string option))

let _menhir_action_138 =
  fun () ->
    (
# 414 "lib/parser.mly"
                ( None )
# 2316 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_139 =
  fun _3 ->
    (
# 415 "lib/parser.mly"
                                  ( Some _3 )
# 2324 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_140 =
  fun _1 _2 _3 ->
    (
# 305 "lib/parser.mly"
      ( { pmode = _1; pname = _2; ptype = _3 } )
# 2332 "lib/parser.ml"
     : (Ast.param))

let _menhir_action_141 =
  fun _1 ->
    (
# 300 "lib/parser.mly"
          ( [_1] )
# 2340 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_142 =
  fun _1 _3 ->
    (
# 301 "lib/parser.mly"
                           ( _1 @ [_3] )
# 2348 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_143 =
  fun () ->
    (
# 296 "lib/parser.mly"
                ( [] )
# 2356 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_144 =
  fun _1 ->
    (
# 297 "lib/parser.mly"
               ( _1 )
# 2364 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_145 =
  fun () ->
    (
# 308 "lib/parser.mly"
          ( Some ByRef )
# 2372 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_146 =
  fun () ->
    (
# 309 "lib/parser.mly"
          ( Some ByVal )
# 2380 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_147 =
  fun () ->
    (
# 310 "lib/parser.mly"
          ( Some ByRes )
# 2388 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_148 =
  fun () ->
    (
# 311 "lib/parser.mly"
                ( None )
# 2396 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_149 =
  fun () ->
    (
# 314 "lib/parser.mly"
                ( None )
# 2404 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_150 =
  fun _2 ->
    (
# 315 "lib/parser.mly"
                                      ( Some _2 )
# 2412 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_151 =
  fun _3 _4 _5 _7 ->
    (
# 268 "lib/parser.mly"
      (
        { pkind = PProc
        ; pr_name = _3
        ; params = _4
        ; rettype = None
        ; pattrs = _5
        ; directives = []
        ; body = _7
        }
      )
# 2429 "lib/parser.ml"
     : (Ast.proc))

let _menhir_action_152 =
  fun _3 _4 _5 _6 _8 ->
    (
# 280 "lib/parser.mly"
      (
        { pkind = PFunction
        ; pr_name = _3
        ; params = _4
        ; rettype = _5
        ; pattrs = _6
        ; directives = []
        ; body = _8
        }
      )
# 2446 "lib/parser.ml"
     : (Ast.proc))

let _menhir_action_153 =
  fun () ->
    (
# 318 "lib/parser.mly"
                                    ( () )
# 2454 "lib/parser.ml"
     : (unit))

let _menhir_action_154 =
  fun _1 ->
    (
# 321 "lib/parser.mly"
          ( Some _1 )
# 2462 "lib/parser.ml"
     : (string option))

let _menhir_action_155 =
  fun () ->
    (
# 322 "lib/parser.mly"
                ( None )
# 2470 "lib/parser.ml"
     : (string option))

let _menhir_action_156 =
  fun () ->
    (
# 134 "lib/parser.mly"
                ( () )
# 2478 "lib/parser.ml"
     : (unit))

let _menhir_action_157 =
  fun () ->
    (
# 135 "lib/parser.mly"
                   ( () )
# 2486 "lib/parser.ml"
     : (unit))

let _menhir_action_158 =
  fun _1 ->
    (
# 261 "lib/parser.mly"
          ( SName _1 )
# 2494 "lib/parser.ml"
     : (Ast.status_item))

let _menhir_action_159 =
  fun _3 ->
    (
# 262 "lib/parser.mly"
                                 ( SVal _3 )
# 2502 "lib/parser.ml"
     : (Ast.status_item))

let _menhir_action_160 =
  fun _1 ->
    (
# 257 "lib/parser.mly"
              ( [_1] )
# 2510 "lib/parser.ml"
     : (Ast.status_item list))

let _menhir_action_161 =
  fun _1 _3 ->
    (
# 258 "lib/parser.mly"
                                ( _1 @ [_3] )
# 2518 "lib/parser.ml"
     : (Ast.status_item list))

let _menhir_action_162 =
  fun _1 _3 ->
    (
# 331 "lib/parser.mly"
                     ( SLabel (_1, _3) )
# 2526 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_163 =
  fun _1 _3 ->
    (
# 332 "lib/parser.mly"
                           ( SAssign (_1, _3) )
# 2534 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_164 =
  fun _1 ->
    (
# 333 "lib/parser.mly"
              ( _1 )
# 2542 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_165 =
  fun _1 ->
    (
# 334 "lib/parser.mly"
            ( _1 )
# 2550 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_166 =
  fun _1 ->
    (
# 335 "lib/parser.mly"
               ( _1 )
# 2558 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_167 =
  fun _1 ->
    (
# 336 "lib/parser.mly"
             ( _1 )
# 2566 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_168 =
  fun _1 ->
    (
# 337 "lib/parser.mly"
              ( _1 )
# 2574 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_169 =
  fun _2 ->
    (
# 338 "lib/parser.mly"
                    ( SGoto _2 )
# 2582 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_170 =
  fun _2 ->
    (
# 339 "lib/parser.mly"
                         ( SReturn _2 )
# 2590 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_171 =
  fun _2 ->
    (
# 340 "lib/parser.mly"
                            ( SExit _2 )
# 2598 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_172 =
  fun _2 ->
    (
# 341 "lib/parser.mly"
                       ( SStop _2 )
# 2606 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_173 =
  fun _2 ->
    (
# 342 "lib/parser.mly"
                        ( SAbort _2 )
# 2614 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_174 =
  fun () ->
    (
# 343 "lib/parser.mly"
                  ( SFallthru )
# 2622 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_175 =
  fun _2 ->
    (
# 344 "lib/parser.mly"
                                      ( SBlock _2 )
# 2630 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_176 =
  fun () ->
    (
# 345 "lib/parser.mly"
         ( SNoop )
# 2638 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_177 =
  fun () ->
    (
# 325 "lib/parser.mly"
                ( [] )
# 2646 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_178 =
  fun _1 _2 ->
    (
# 326 "lib/parser.mly"
                       ( _1 @ [_2] )
# 2654 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_179 =
  fun () ->
    (
# 205 "lib/parser.mly"
                ( [] )
# 2662 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_180 =
  fun _2 ->
    (
# 206 "lib/parser.mly"
                                      ( _2 )
# 2670 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_181 =
  fun () ->
    (
# 209 "lib/parser.mly"
                ( [] )
# 2678 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_182 =
  fun _2 ->
    (
# 210 "lib/parser.mly"
                           ( _2 )
# 2686 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_183 =
  fun _1 ->
    (
# 149 "lib/parser.mly"
         ( `Decl _1 )
# 2694 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_184 =
  fun _1 ->
    (
# 150 "lib/parser.mly"
             ( `Proc _1 )
# 2702 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_185 =
  fun _1 ->
    (
# 151 "lib/parser.mly"
         ( `Stmt _1 )
# 2710 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_186 =
  fun () ->
    (
# 152 "lib/parser.mly"
         ( `Stmt SNoop )
# 2718 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_187 =
  fun () ->
    (
# 138 "lib/parser.mly"
                ( (([] : Ast.decl list), ([] : Ast.stmt list), ([] : Ast.proc list)) )
# 2726 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_188 =
  fun _1 _2 ->
    (
# 140 "lib/parser.mly"
      (
        let (ds, ss, ps) = _1 in
        match _2 with
        | `Decl d -> (ds @ [d], ss, ps)
        | `Stmt s -> (ds, ss @ [s], ps)
        | `Proc p -> (ds, ss, ps @ [p])
      )
# 2740 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_189 =
  fun _1 ->
    (
# 227 "lib/parser.mly"
             ( TAtom _1 )
# 2748 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_190 =
  fun _1 ->
    (
# 228 "lib/parser.mly"
             ( TNamed _1 )
# 2756 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_191 =
  fun () ->
    (
# 222 "lib/parser.mly"
                ( None )
# 2764 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_192 =
  fun _2 ->
    (
# 223 "lib/parser.mly"
                    ( Some _2 )
# 2772 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_193 =
  fun _2 ->
    (
# 224 "lib/parser.mly"
                   ( Some _2 )
# 2780 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_194 =
  fun () ->
    (
# 92 "lib/parser.mly"
             ( ARec )
# 2788 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_195 =
  fun () ->
    (
# 93 "lib/parser.mly"
             ( ARent )
# 2796 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_196 =
  fun () ->
    (
# 94 "lib/parser.mly"
             ( AStatic )
# 2804 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_197 =
  fun () ->
    (
# 95 "lib/parser.mly"
             ( AParallel )
# 2812 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_198 =
  fun () ->
    (
# 96 "lib/parser.mly"
             ( AInline )
# 2820 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_199 =
  fun () ->
    (
# 88 "lib/parser.mly"
                ( [] )
# 2828 "lib/parser.ml"
     : (Ast.use_attr list))

let _menhir_action_200 =
  fun _1 _2 ->
    (
# 89 "lib/parser.mly"
                           ( _1 @ [_2] )
# 2836 "lib/parser.ml"
     : (Ast.use_attr list))

let _menhir_action_201 =
  fun _2 _3 ->
    (
# 385 "lib/parser.mly"
                    ( SWhile (_2, _3) )
# 2844 "lib/parser.ml"
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
      let _v = _menhir_action_127 _2 _3 in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_015 _1 in
          MenhirBox_compilation_unit _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_364 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RENT ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_323 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_016 () in
          _menhir_goto_compool_includes_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_309 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_196 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_use_attr : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_200 _1 _2 in
      _menhir_goto_use_attrs_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_use_attrs_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState363 ->
          _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState307 ->
          _menhir_run_308 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_330 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState331
          | END ->
              let _v_0 = _menhir_action_149 () in
              _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState331
          | _ ->
              _eRR ())
      | RENT ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_323 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_311 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_177 () in
      _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState311 _tok
  
  and _menhir_run_312 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState312
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | END ->
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState312) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_156 () in
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState313 _tok
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
      | ABORT ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState312
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
      let _v = _menhir_action_078 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState281 ->
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState227 ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState210 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState155 ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
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
      | MenhirState124 ->
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
  
  and _menhir_run_283 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_046 _1 in
          _menhir_goto_define_rhs _menhir_stack _menhir_lexbuf _menhir_lexer _v
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
      let _v = _menhir_action_075 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_077 () in
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
      let _v = _menhir_action_073 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
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
              let _v_5 = _menhir_action_108 () in
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
          | _ ->
              _eRR ())
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | MOD | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | SLASH | STAR | STOP | THEN | TO | WHILE | XOR ->
          let _1 = _v in
          let _v = _menhir_action_080 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_074 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_079 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_076 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_018 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_081 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
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
  
  and _menhir_goto_define_rhs : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE _menhir_cell0_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_DEFINE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_024 _2 _4 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState338 ->
          _menhir_run_337 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState004 ->
          _menhir_run_337 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState238 ->
          _menhir_run_298 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState294 ->
          _menhir_run_298 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_337 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_183 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_top_item : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_188 _1 _2 in
      _menhir_goto_top_items_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_top_items_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState002 ->
          _menhir_run_338 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_338 : type  ttv_stack. (ttv_stack _menhir_cell0_module_header as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | TYPE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | TABLE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | STOP ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | SEMI ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RETURN ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | REF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | OVERLAY ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | LABEL ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | ITEM ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | IF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState338
      | GOTO ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | FOR ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | EXIT ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | DEFINE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | DEF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_304 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | CASE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | BLOCK ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | BEGIN ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | ABORT ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState338
      | TERM ->
          let _1 = _v in
          let _v = _menhir_action_129 _1 in
          _menhir_goto_module_body _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_169 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATUS ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  let _menhir_s = MenhirState172 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TYPEATOM _ ->
                      _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | IDENT _v ->
                      _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | EQUAL ->
              let _menhir_s = MenhirState184 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEATOM _v ->
                  _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_173 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
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
                  let _v = _menhir_action_159 _3 in
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
      | MenhirState172 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_183 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_160 _1 in
      _menhir_goto_status_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_status_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
              let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
              let _5 = _v in
              let _v = _menhir_action_021 _2 _5 in
              _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | COMMA ->
          let _menhir_stack = MenhirCell1_status_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState181 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEATOM _ ->
              _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_177 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_158 _1 in
      _menhir_goto_status_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_182 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_status_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_status_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_161 _1 _3 in
      _menhir_goto_status_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_185 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_189 _1 in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState203 ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState201 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState184 ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_204 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_COLON -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COLON (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_192 _2 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_spec_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState328 ->
          _menhir_run_329 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState273 ->
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState259 ->
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState247 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState200 ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_329 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_199 () in
      _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState329 _tok
  
  and _menhir_run_274 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState274
      | SEMI ->
          let _v_0 = _menhir_action_041 () in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_206 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WITH (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState207 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATIC ->
              _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REP ->
              _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RENT ->
              _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | POS ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARALLEL ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OVERLAY ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIKE ->
              _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INSTANCE ->
              _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INLINE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEFAULT ->
              _menhir_run_227 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT ->
              _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_208 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_027 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_attr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState207 ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState232 ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_234 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_039 _1 in
      _menhir_goto_decl_attr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_attr_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_WITH (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_042 _3 in
          _menhir_goto_decl_attrs_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_decl_attr_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState232 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATIC ->
              _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REP ->
              _menhir_run_209 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RENT ->
              _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REC ->
              _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | POS ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARALLEL ->
              _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OVERLAY ->
              _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIKE ->
              _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INSTANCE ->
              _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INLINE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEFAULT ->
              _menhir_run_227 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT ->
              _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl_attrs_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState290 ->
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState274 ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState205 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_291 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_043 () in
              _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState293 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v = _menhir_action_001 () in
              _menhir_goto_block_decl_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_294 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | TABLE ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | REF ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | OVERLAY ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | LABEL ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | ITEM ->
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | END ->
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState294) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_156 () in
          _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState295 _tok
      | DEFINE ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | DEF ->
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | BLOCK ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | _ ->
          _eRR ()
  
  and _menhir_run_189 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TABLE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState191 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | STAR ->
                  _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | COLON | SEMI | TYPE | WITH ->
              let _v = _menhir_action_181 () in
              _menhir_goto_table_dims_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_192 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_047 () in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dim : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState191 ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState197 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_199 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_050 _1 in
      _menhir_goto_dim_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dim_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_182 _2 in
          _menhir_goto_table_dims_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_dim_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState197 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_table_dims_opt : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_table_dims_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState200
      | COLON ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState200
      | SEMI | WITH ->
          let _v_0 = _menhir_action_191 () in
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState200 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_201 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState201 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_186 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_190 _1 in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_203 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COLON (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState203 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_205 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
      | SEMI ->
          let _v_0 = _menhir_action_041 () in
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState205 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_235 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_043 () in
              _menhir_run_238 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState237 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v = _menhir_action_179 () in
              _menhir_goto_table_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_238 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | TABLE ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | REF ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | OVERLAY ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | LABEL ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | ITEM ->
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | END ->
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState238) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_156 () in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState277 _tok
      | DEFINE ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | DEF ->
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | BLOCK ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState238
      | _ ->
          _eRR ()
  
  and _menhir_run_239 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState239 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_240 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PROC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState241
          | SEMI ->
              let _v_0 = _menhir_action_114 () in
              _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_242 : type  ttv_stack. (ttv_stack _menhir_cell0_IDENT as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState242 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | BYVAL ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BYRES ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BYREF ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          let _v = _menhir_action_143 () in
          _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | IDENT _ ->
          _menhir_reduce_148 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_243 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_146 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_mode_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_param_mode_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE ->
              _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
          | COLON ->
              _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState247
          | COMMA | RPAREN ->
              let _v_1 = _menhir_action_191 () in
              _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_248 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_param_mode_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_140 _1 _2 _3 in
      _menhir_goto_param _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState242 ->
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState252 ->
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_254 : type  ttv_stack. ((ttv_stack _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_141 _1 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list : type  ttv_stack. ((ttv_stack _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState252 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BYVAL ->
              _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BYRES ->
              _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BYREF ->
              _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _ ->
              _menhir_reduce_148 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_144 _1 in
          _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_244 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_147 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_245 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_145 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_reduce_148 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_148 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list_opt : type  ttv_stack. (ttv_stack _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_115 _2 in
      _menhir_goto_formal_params_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_formal_params_opt : type  ttv_stack. (ttv_stack _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState327 ->
          _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState306 ->
          _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState258 ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_328 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState328
      | COLON ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState328
      | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
          let _v_0 = _menhir_action_191 () in
          _menhir_run_329 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState328 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_307 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_199 () in
      _menhir_run_308 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState307 _tok
  
  and _menhir_run_308 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
          | END ->
              let _v_0 = _menhir_action_149 () in
              _menhir_run_315 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState310
          | _ ->
              _eRR ())
      | RENT ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_323 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_315 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_proc_body_opt (_menhir_stack, _menhir_s, _v) in
      _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState315
  
  and _menhir_run_316 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_END (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_154 _1 in
          _menhir_goto_proc_end_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_155 () in
          _menhir_goto_proc_end_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_end_name_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_proc_end_name_opt (_menhir_stack, _v) in
      let _v_0 = _menhir_action_156 () in
      _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState318 _tok
  
  and _menhir_run_319 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END _menhir_cell0_proc_end_name_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell0_proc_end_name_opt (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_END (_menhir_stack, _menhir_s) = _menhir_stack in
          let _ = _menhir_action_153 () in
          _menhir_goto_proc_end _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_123 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_semis_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_157 () in
      _menhir_goto_semis_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_semis_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState318 ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState313 ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_303 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState295 ->
          _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState277 ->
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_314 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let MenhirCell1_END (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_stmt_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_150 _2 in
          _menhir_goto_proc_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_body_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState331 ->
          _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState310 ->
          _menhir_run_315 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_332 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_proc_body_opt (_menhir_stack, _menhir_s, _v) in
      _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState332
  
  and _menhir_run_303 : type  ttv_stack. ((((ttv_stack _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TERM ->
          let MenhirCell1_END (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_top_items_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _) = _menhir_stack in
          let _v = _menhir_action_128 _2 in
          _menhir_goto_module_body _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_296 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_END (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_decl_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let _v = _menhir_action_002 _2 in
          _menhir_goto_block_decl_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_block_decl_body_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attrs_opt (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_BLOCK (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_020 _2 _3 _5 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_278 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_END (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_decl_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let _v = _menhir_action_180 _2 in
          _menhir_goto_table_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_table_body_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attrs_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_type_spec_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_table_dims_opt (_menhir_stack, _3) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_TABLE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_019 _2 _3 _4 _5 _7 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_122 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | ELSE | ELSIF | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let MenhirCell1_END (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_stmt_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_175 _2 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState338 ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState004 ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState070 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState159 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState157 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState149 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState102 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState312 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState140 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState118 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState120 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_335 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_185 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_168 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_201 _2 _3 in
      let _1 = _v in
      let _v = _menhir_action_166 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_165 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ELSIF (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_elsif_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_070 _1 _3 _5 in
      _menhir_goto_elsif_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_elsif_list : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_elsif_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSIF ->
          let _menhir_stack = MenhirCell1_ELSIF (_menhir_stack, MenhirState161) in
          let _menhir_s = MenhirState162 in
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
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState161
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let _v_5 = _menhir_action_067 () in
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_159 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ELSE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState159 in
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
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
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
          let _v = _menhir_action_110 () in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_STOP -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_STOP (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_172 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_075 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_176 () in
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
          let _v = _menhir_action_110 () in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_077 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_RETURN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_170 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
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
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
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
                  let _v_5 = _menhir_action_108 () in
                  _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
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
              let _v_11 = _menhir_action_108 () in
              _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
          | _ ->
              _eRR ())
      | COLON ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
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
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SEMI ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          let _v = _menhir_action_005 () in
          _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQUAL ->
          let _1 = _v in
          let _v = _menhir_action_125 _1 in
          _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _3 = _v in
      let _v = _menhir_action_007 _3 in
      _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_call_args_opt : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_008 _1 _2 in
          let _1 = _v in
          let _v = _menhir_action_164 _1 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _2 = _v in
          let _v = _menhir_action_006 _2 in
          _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQUAL ->
          let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_126 _1 _3 in
          _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_lvalue : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState130 in
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
              let _v = _menhir_action_169 _2 in
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
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
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
          let _v = _menhir_action_174 () in
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
          let _v = _menhir_action_071 _1 in
          _menhir_goto_exit_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | SEMI ->
          let _v = _menhir_action_072 () in
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
          let _v = _menhir_action_171 _2 in
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
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_177 () in
      _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState119 _tok
  
  and _menhir_run_120 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState120
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | END ->
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState120) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_156 () in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState121 _tok
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | ABORT ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState120
      | _ ->
          _eRR ()
  
  and _menhir_run_124 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ABORT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState124
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState124
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState124
      | FLOAT _v ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState124
      | FALSE ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | BEAD _v ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState124
      | SEMI ->
          let _v = _menhir_action_110 () in
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_125 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ABORT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ABORT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_173 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_166 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_elsif_list (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_stmt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_119 _2 _4 _5 _6 in
      _menhir_goto_if_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_if_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_165 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_160 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ELSE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ELSE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_068 _2 in
      _menhir_goto_else_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_else_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState154 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState161 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_167 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_118 _2 _4 _5 in
      _menhir_goto_if_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_158 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_ELSIF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_069 _2 _4 in
      _menhir_goto_elsif_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_154 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSIF ->
          let _menhir_stack = MenhirCell1_ELSIF (_menhir_stack, MenhirState154) in
          let _menhir_s = MenhirState155 in
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
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState154
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let _v_5 = _menhir_action_067 () in
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_162 _1 _3 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_150 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_by_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_113 _2 _4 _5 _7 _8 in
      _menhir_goto_for_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_for_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_167 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_146 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_by_opt (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_TO (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_112 _2 _4 _6 _7 _8 in
      _menhir_goto_for_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_128 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_178 _1 _2 in
      _menhir_goto_stmt_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt_list_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState311 ->
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState119 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState117 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_140 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | STOP ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | SEMI ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | RETURN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | IF ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState140
      | GOTO ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | FOR ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | EXIT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | CASE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | BEGIN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | ABORT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState140
      | END ->
          let MenhirCell1_OTHERWISE (_menhir_stack, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_139 _3 in
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
          let _v = _menhir_action_014 _2 _4 _5 in
          let _1 = _v in
          let _v = _menhir_action_168 _1 in
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
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | END | OTHERWISE | WHEN ->
          let MenhirCell1_case_labels (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_WHEN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_009 _2 _4 in
          _menhir_goto_case_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_case_clause : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState111 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState137 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_145 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_010 _1 in
      _menhir_goto_case_clauses _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_case_clauses : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_case_clauses (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHEN ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState137
      | OTHERWISE ->
          let _menhir_stack = MenhirCell1_OTHERWISE (_menhir_stack, MenhirState137) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_177 () in
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState139 _tok
          | _ ->
              _eRR ())
      | END ->
          let _v = _menhir_action_138 () in
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
  
  and _menhir_run_144 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_case_clauses (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_011 _1 _2 in
      _menhir_goto_case_clauses _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_proc_end : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match _menhir_s with
      | MenhirState332 ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState315 ->
          _menhir_run_320 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_333 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_proc_body_opt (_menhir_stack, _, _8) = _menhir_stack in
      let MenhirCell1_use_attrs_opt (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_type_spec_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _3) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_DEF (_menhir_stack, _) = _menhir_stack in
      let _v = _menhir_action_152 _3 _4 _5 _6 _8 in
      _menhir_goto_proc_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_proc_def : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_184 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_320 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_proc_body_opt (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_use_attrs_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _3) = _menhir_stack in
      let MenhirCell1_PROC (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_DEF (_menhir_stack, _) = _menhir_stack in
      let _v = _menhir_action_151 _3 _4 _5 _7 in
      _menhir_goto_proc_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_321 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_195 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_322 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_194 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_323 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_197 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_324 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_198 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_259 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | COLON ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState259
      | SEMI ->
          let _v_0 = _menhir_action_191 () in
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_260 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_124 _2 _3 _4 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_linkage_target : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState304 ->
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState286 ->
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState239 ->
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_287 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEF -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_DEF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_120 _2 in
          _menhir_goto_linkage_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_linkage_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_025 _1 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_261 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_REF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_121 _2 in
          _menhir_goto_linkage_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_255 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
      let MenhirCell1_PROC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_123 _2 _3 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_253 : type  ttv_stack. ((ttv_stack _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_param_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_param_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_142 _1 _3 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_256 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_122 _1 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_257 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState258
          | COLON | SEMI | TYPE ->
              let _v_0 = _menhir_action_114 () in
              _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState258 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_263 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _2 = _v in
              let _v = _menhir_action_023 _2 in
              _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_266 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LABEL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState266 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_267 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_116 _1 in
      _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState272 ->
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState266 ->
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_273 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState273
      | COMMA ->
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState273
      | SEMI | WITH ->
          let _v_0 = _menhir_action_191 () in
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState273 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_270 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_list -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ident_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_117 _1 _3 in
          _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_268 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LABEL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LABEL (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_026 _2 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_272 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ITEM (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState272 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_279 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFINE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EQUAL ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState281
              | STRING _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | SEMI ->
                      let _1 = _v_0 in
                      let _v = _menhir_action_045 _1 in
                      _menhir_goto_define_rhs _menhir_stack _menhir_lexbuf _menhir_lexer _v
                  | AND | EQ | EQUAL | EQV | GE | GQ | GR | GT | LE | LQ | LS | LT | MINUS | MOD | NEQ | NQ | OR | PLUS | SLASH | STAR | XOR ->
                      let _v_2 =
                        let _1 = _v_0 in
                        _menhir_action_075 _1
                      in
                      _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState281 _tok
                  | _ ->
                      _eRR ())
              | NULL ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState281
              | NOT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState281
              | MINUS ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState281
              | LPAREN ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState281
              | INT _v_3 ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState281
              | IDENT _v_4 ->
                  _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState281
              | FLOAT _v_5 ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState281
              | FALSE ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState281
              | BEAD _v_6 ->
                  _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState281
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_286 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState286 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_289 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BLOCK (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WITH ->
              _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState290
          | SEMI ->
              let _v_0 = _menhir_action_041 () in
              _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState290 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_193 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_048 _1 in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_194 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_049 _1 in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_198 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_dim_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_dim_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_051 _1 _3 in
      _menhir_goto_dim_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_275 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_spec_opt (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_ident_list (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_ITEM (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_018 _2 _3 _4 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_209 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState210 in
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
  
  and _menhir_run_213 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_036 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_214 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_035 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_215 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_POS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState216 in
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
  
  and _menhir_run_219 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_038 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_220 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_033 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_222 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_030 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_224 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_034 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_226 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_037 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_227 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState227 in
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
  
  and _menhir_run_229 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_028 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_233 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_cell1_decl_attr_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attr_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_040 _1 _3 in
      _menhir_goto_decl_attr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_202 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_193 _2 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_187 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_IDENT (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_022 _2 _4 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_301 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_186 () in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_304 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          let _menhir_stack = MenhirCell1_PROC (_menhir_stack, MenhirState304) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
              | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
                  let _v_0 = _menhir_action_114 () in
                  _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState306 _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | IDENT _v ->
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState304
      | FUNCTION ->
          let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, MenhirState304) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | LPAREN ->
                  _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
              | COLON | INLINE | PARALLEL | REC | RENT | SEMI | STATIC | TYPE ->
                  let _v_1 = _menhir_action_114 () in
                  _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState327 _tok
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
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | TABLE ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | SEMI ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | REF ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | OVERLAY ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | LABEL ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | ITEM ->
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
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
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState004) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_156 () in
          _menhir_run_303 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState302 _tok
      | DEFINE ->
          _menhir_run_279 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | DEF ->
          _menhir_run_304 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | BLOCK ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | ABORT ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | _ ->
          _eRR ()
  
  and _menhir_run_298 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_044 _1 _2 in
      _menhir_goto_decl_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_list_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState293 ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState237 ->
          _menhir_run_238 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_228 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState228
      | COMMA | RPAREN ->
          let MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_029 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_217 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_POS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_POS (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_031 _3 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState217
      | _ ->
          _eRR ()
  
  and _menhir_run_211 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_REP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_REP (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_032 _3 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | _ ->
          _eRR ()
  
  and _menhir_run_163 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState163) in
          let _menhir_s = MenhirState164 in
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
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | _ ->
          _eRR ()
  
  and _menhir_run_156 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState156) in
          let _menhir_s = MenhirState157 in
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
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | STOP ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | STAR ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | SLASH ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | SEMI ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | RETURN ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | PLUS ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | OR ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | NQ ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | NEQ ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | MOD ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | MINUS ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LT ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LS ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LQ ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | LE ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | IF ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | IDENT _v_0 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState149
      | GT ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | GR ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | GQ ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | GOTO ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | GE ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | FOR ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | FALLTHRU ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | EXIT ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | EQV ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | EQUAL ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | EQ ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | CASE ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | BEGIN ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | AND ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | ABORT ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState149
      | _ ->
          _eRR ()
  
  and _menhir_run_131 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_lvalue (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_163 _1 _3 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
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
          let _v = _menhir_action_013 _1 _3 in
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
          let _v_5 = _menhir_action_177 () in
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
          let _v = _menhir_action_012 _1 in
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
          let _v = _menhir_action_004 _2 in
          _menhir_goto_by_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_by_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState097 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_147 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR _menhir_cell0_IDENT, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_by_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_s = MenhirState148 in
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
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
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
          let _v_0 = _menhir_action_003 () in
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
          let _v_5 = _menhir_action_003 () in
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState097 _tok
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
              _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
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
          let _v = _menhir_action_111 _1 in
          _menhir_goto_expr_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState124 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v
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
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
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
          let _v = _menhir_action_084 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_068 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_083 _2 in
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
          let _v = _menhir_action_082 _2 in
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
          let _v = _menhir_action_106 _1 in
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
          let _v = _menhir_action_109 _1 in
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
          let _v = _menhir_action_105 _1 _3 in
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
          let _v = _menhir_action_103 _1 _3 in
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
          let _v = _menhir_action_102 _1 _3 in
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
          let _v = _menhir_action_096 _1 _3 in
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
          let _v = _menhir_action_095 _1 _3 in
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
          let _v = _menhir_action_093 _1 _3 in
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
          let _v = _menhir_action_101 _1 _3 in
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
          let _v = _menhir_action_100 _1 _3 in
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
          let _v = _menhir_action_092 _1 _3 in
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
          let _v = _menhir_action_091 _1 _3 in
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
          let _v = _menhir_action_099 _1 _3 in
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
          let _v = _menhir_action_098 _1 _3 in
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
          let _v = _menhir_action_090 _1 _3 in
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
          let _v = _menhir_action_094 _1 _3 in
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
          let _v = _menhir_action_089 _1 _3 in
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
          let _v = _menhir_action_097 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MOD -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MOD (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_087 _1 _3 in
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
          let _v = _menhir_action_088 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_SLASH -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SLASH (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_086 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_STAR -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_STAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_085 _1 _3 in
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
          let _v = _menhir_action_104 _1 _3 in
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
          let _v = _menhir_action_107 _1 _3 in
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
                  let _v = _menhir_action_017 _1 _3 in
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
          let _v = _menhir_action_130 _1 _2 _3 _4 _5 in
          let _menhir_stack = MenhirCell0_module_header (_menhir_stack, _v) in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, MenhirState002) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_187 () in
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v_1 = _menhir_action_187 () in
              _menhir_run_338 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState002 _tok
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  let _menhir_goto_module_name_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_module_name_opt (_menhir_stack, _v) in
      let _v_0 = _menhir_action_199 () in
      _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState363 _tok
  
  let _menhir_goto_module_kind_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_module_kind_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_0 in
          let _v = _menhir_action_136 _1 in
          _menhir_goto_module_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IF | INLINE | ITEM | LABEL | OVERLAY | PARALLEL | REC | REF | RENT | RETURN | SEMI | STATIC | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_137 () in
          _menhir_goto_module_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_goto_directives_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_directives_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PROGRAM ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_131 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PROC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_133 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUNCTION ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_134 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMPOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_132 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DIRECTIVE_NAME _v ->
              let _menhir_stack = MenhirCell0_DIRECTIVE_NAME (_menhir_stack, _v) in
              let _menhir_s = MenhirState347 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING _v ->
                  _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NULL ->
                  _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FLOAT _v ->
                  _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FALSE ->
                  _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BEAD _v ->
                  _menhir_run_355 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEMI ->
                  let _v = _menhir_action_063 () in
                  _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IDENT _ | IF | INLINE | ITEM | LABEL | OVERLAY | PARALLEL | REC | REF | RENT | RETURN | SEMI | STATIC | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_135 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_348 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_058 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_arg : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState347 ->
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState358 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_360 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_061 _1 in
      _menhir_goto_directive_args _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_args : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_348 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState358
      | STRING _v_0 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState358
      | NULL ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState358
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState358
      | IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState358
      | FLOAT _v_3 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState358
      | FALSE ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState358
      | BEAD _v_4 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_355 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState358
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_064 _1 in
          _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_349 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_053 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_350 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_057 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_351 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_054 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_352 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_060 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_353 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_055 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_354 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_059 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_355 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_args_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_DIRECTIVE_NAME (_menhir_stack, _2) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_052 _2 _3 in
      let MenhirCell0_directives_opt (_menhir_stack, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_066 _1 _2 in
      _menhir_goto_directives_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_359 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_directive_args -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_directive_args (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_062 _1 _2 in
      _menhir_goto_directive_args _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | START ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_065 () in
          _menhir_goto_directives_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
end

let compilation_unit =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_compilation_unit v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
