
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
# 38 "lib/parser.mly"
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
# 34 "lib/parser.mly"
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
# 35 "lib/parser.mly"
       (int)
# 73 "lib/parser.ml"
  )
    | INSTANCE
    | INLINE
    | IF
    | IDENT of (
# 33 "lib/parser.mly"
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
# 36 "lib/parser.mly"
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
# 31 "lib/parser.mly"
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
# 37 "lib/parser.mly"
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

  | MenhirState018 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 018.
        Stack shape : ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState022 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr_list, _menhir_box_compilation_unit) _menhir_state
    (** State 022.
        Stack shape : expr_list.
        Start symbol: compilation_unit. *)

  | MenhirState023 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr_list, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 023.
        Stack shape : expr_list expr.
        Start symbol: compilation_unit. *)

  | MenhirState024 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_XOR, _menhir_box_compilation_unit) _menhir_state
    (** State 024.
        Stack shape : expr XOR.
        Start symbol: compilation_unit. *)

  | MenhirState025 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_XOR, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 025.
        Stack shape : expr XOR expr.
        Start symbol: compilation_unit. *)

  | MenhirState026 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_STAR, _menhir_box_compilation_unit) _menhir_state
    (** State 026.
        Stack shape : expr STAR.
        Start symbol: compilation_unit. *)

  | MenhirState028 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_SLASH, _menhir_box_compilation_unit) _menhir_state
    (** State 028.
        Stack shape : expr SLASH.
        Start symbol: compilation_unit. *)

  | MenhirState030 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_PLUS, _menhir_box_compilation_unit) _menhir_state
    (** State 030.
        Stack shape : expr PLUS.
        Start symbol: compilation_unit. *)

  | MenhirState031 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_PLUS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 031.
        Stack shape : expr PLUS expr.
        Start symbol: compilation_unit. *)

  | MenhirState032 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MOD, _menhir_box_compilation_unit) _menhir_state
    (** State 032.
        Stack shape : expr MOD.
        Start symbol: compilation_unit. *)

  | MenhirState034 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NQ, _menhir_box_compilation_unit) _menhir_state
    (** State 034.
        Stack shape : expr NQ.
        Start symbol: compilation_unit. *)

  | MenhirState035 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 035.
        Stack shape : expr NQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState036 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MINUS, _menhir_box_compilation_unit) _menhir_state
    (** State 036.
        Stack shape : expr MINUS.
        Start symbol: compilation_unit. *)

  | MenhirState037 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MINUS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 037.
        Stack shape : expr MINUS expr.
        Start symbol: compilation_unit. *)

  | MenhirState038 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NEQ, _menhir_box_compilation_unit) _menhir_state
    (** State 038.
        Stack shape : expr NEQ.
        Start symbol: compilation_unit. *)

  | MenhirState039 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NEQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 039.
        Stack shape : expr NEQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState040 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LT, _menhir_box_compilation_unit) _menhir_state
    (** State 040.
        Stack shape : expr LT.
        Start symbol: compilation_unit. *)

  | MenhirState041 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 041.
        Stack shape : expr LT expr.
        Start symbol: compilation_unit. *)

  | MenhirState042 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LS, _menhir_box_compilation_unit) _menhir_state
    (** State 042.
        Stack shape : expr LS.
        Start symbol: compilation_unit. *)

  | MenhirState043 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 043.
        Stack shape : expr LS expr.
        Start symbol: compilation_unit. *)

  | MenhirState044 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LQ, _menhir_box_compilation_unit) _menhir_state
    (** State 044.
        Stack shape : expr LQ.
        Start symbol: compilation_unit. *)

  | MenhirState045 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 045.
        Stack shape : expr LQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState046 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LE, _menhir_box_compilation_unit) _menhir_state
    (** State 046.
        Stack shape : expr LE.
        Start symbol: compilation_unit. *)

  | MenhirState047 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 047.
        Stack shape : expr LE expr.
        Start symbol: compilation_unit. *)

  | MenhirState048 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GT, _menhir_box_compilation_unit) _menhir_state
    (** State 048.
        Stack shape : expr GT.
        Start symbol: compilation_unit. *)

  | MenhirState049 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 049.
        Stack shape : expr GT expr.
        Start symbol: compilation_unit. *)

  | MenhirState050 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GR, _menhir_box_compilation_unit) _menhir_state
    (** State 050.
        Stack shape : expr GR.
        Start symbol: compilation_unit. *)

  | MenhirState051 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GR, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 051.
        Stack shape : expr GR expr.
        Start symbol: compilation_unit. *)

  | MenhirState052 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GQ, _menhir_box_compilation_unit) _menhir_state
    (** State 052.
        Stack shape : expr GQ.
        Start symbol: compilation_unit. *)

  | MenhirState053 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 053.
        Stack shape : expr GQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState054 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GE, _menhir_box_compilation_unit) _menhir_state
    (** State 054.
        Stack shape : expr GE.
        Start symbol: compilation_unit. *)

  | MenhirState055 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 055.
        Stack shape : expr GE expr.
        Start symbol: compilation_unit. *)

  | MenhirState056 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQUAL, _menhir_box_compilation_unit) _menhir_state
    (** State 056.
        Stack shape : expr EQUAL.
        Start symbol: compilation_unit. *)

  | MenhirState057 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQUAL, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 057.
        Stack shape : expr EQUAL expr.
        Start symbol: compilation_unit. *)

  | MenhirState058 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQ, _menhir_box_compilation_unit) _menhir_state
    (** State 058.
        Stack shape : expr EQ.
        Start symbol: compilation_unit. *)

  | MenhirState059 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQ, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 059.
        Stack shape : expr EQ expr.
        Start symbol: compilation_unit. *)

  | MenhirState060 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_AND, _menhir_box_compilation_unit) _menhir_state
    (** State 060.
        Stack shape : expr AND.
        Start symbol: compilation_unit. *)

  | MenhirState061 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_AND, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 061.
        Stack shape : expr AND expr.
        Start symbol: compilation_unit. *)

  | MenhirState062 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OR, _menhir_box_compilation_unit) _menhir_state
    (** State 062.
        Stack shape : expr OR.
        Start symbol: compilation_unit. *)

  | MenhirState063 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OR, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 063.
        Stack shape : expr OR expr.
        Start symbol: compilation_unit. *)

  | MenhirState064 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQV, _menhir_box_compilation_unit) _menhir_state
    (** State 064.
        Stack shape : expr EQV.
        Start symbol: compilation_unit. *)

  | MenhirState065 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQV, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 065.
        Stack shape : expr EQV expr.
        Start symbol: compilation_unit. *)

  | MenhirState066 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 066.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState067 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 067.
        Stack shape : LPAREN expr.
        Start symbol: compilation_unit. *)

  | MenhirState070 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_NOT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 070.
        Stack shape : NOT expr.
        Start symbol: compilation_unit. *)

  | MenhirState071 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 071.
        Stack shape : WHILE expr.
        Start symbol: compilation_unit. *)

  | MenhirState072 : (('s, _menhir_box_compilation_unit) _menhir_cell1_STOP, _menhir_box_compilation_unit) _menhir_state
    (** State 072.
        Stack shape : STOP.
        Start symbol: compilation_unit. *)

  | MenhirState075 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 075.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState077 : (('s, _menhir_box_compilation_unit) _menhir_cell1_RETURN, _menhir_box_compilation_unit) _menhir_state
    (** State 077.
        Stack shape : RETURN.
        Start symbol: compilation_unit. *)

  | MenhirState080 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_state
    (** State 080.
        Stack shape : IF.
        Start symbol: compilation_unit. *)

  | MenhirState081 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 081.
        Stack shape : IF expr.
        Start symbol: compilation_unit. *)

  | MenhirState082 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 082.
        Stack shape : IF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState083 : (('s, _menhir_box_compilation_unit) _menhir_cell1_GOTO, _menhir_box_compilation_unit) _menhir_state
    (** State 083.
        Stack shape : GOTO.
        Start symbol: compilation_unit. *)

  | MenhirState086 : (('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_state
    (** State 086.
        Stack shape : FOR.
        Start symbol: compilation_unit. *)

  | MenhirState088 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 088.
        Stack shape : FOR ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState089 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 089.
        Stack shape : FOR ident_loc expr.
        Start symbol: compilation_unit. *)

  | MenhirState090 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_state
    (** State 090.
        Stack shape : FOR ident_loc expr TO.
        Start symbol: compilation_unit. *)

  | MenhirState091 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 091.
        Stack shape : FOR ident_loc expr TO expr.
        Start symbol: compilation_unit. *)

  | MenhirState092 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_BY, _menhir_box_compilation_unit) _menhir_state
    (** State 092.
        Stack shape : expr BY.
        Start symbol: compilation_unit. *)

  | MenhirState093 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_BY, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 093.
        Stack shape : expr BY expr.
        Start symbol: compilation_unit. *)

  | MenhirState094 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 094.
        Stack shape : FOR ident_loc expr TO expr by_opt.
        Start symbol: compilation_unit. *)

  | MenhirState097 : (('s, _menhir_box_compilation_unit) _menhir_cell1_EXIT, _menhir_box_compilation_unit) _menhir_state
    (** State 097.
        Stack shape : EXIT.
        Start symbol: compilation_unit. *)

  | MenhirState101 : (('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_state
    (** State 101.
        Stack shape : CASE.
        Start symbol: compilation_unit. *)

  | MenhirState102 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 102.
        Stack shape : CASE expr.
        Start symbol: compilation_unit. *)

  | MenhirState103 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_state
    (** State 103.
        Stack shape : CASE expr OF.
        Start symbol: compilation_unit. *)

  | MenhirState104 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_state
    (** State 104.
        Stack shape : WHEN.
        Start symbol: compilation_unit. *)

  | MenhirState105 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 105.
        Stack shape : WHEN expr.
        Start symbol: compilation_unit. *)

  | MenhirState107 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_state
    (** State 107.
        Stack shape : WHEN case_labels.
        Start symbol: compilation_unit. *)

  | MenhirState108 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 108.
        Stack shape : WHEN case_labels expr.
        Start symbol: compilation_unit. *)

  | MenhirState109 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_state
    (** State 109.
        Stack shape : WHEN case_labels.
        Start symbol: compilation_unit. *)

  | MenhirState110 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 110.
        Stack shape : WHEN case_labels stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState111 : (('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_state
    (** State 111.
        Stack shape : BEGIN.
        Start symbol: compilation_unit. *)

  | MenhirState112 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 112.
        Stack shape : BEGIN stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState113 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 113.
        Stack shape : BEGIN stmt_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState116 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ABORT, _menhir_box_compilation_unit) _menhir_state
    (** State 116.
        Stack shape : ABORT.
        Start symbol: compilation_unit. *)

  | MenhirState122 : (('s, _menhir_box_compilation_unit) _menhir_cell1_lvalue, _menhir_box_compilation_unit) _menhir_state
    (** State 122.
        Stack shape : lvalue.
        Start symbol: compilation_unit. *)

  | MenhirState123 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_lvalue, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 123.
        Stack shape : lvalue expr.
        Start symbol: compilation_unit. *)

  | MenhirState128 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 128.
        Stack shape : ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState131 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 131.
        Stack shape : ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState134 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 134.
        Stack shape : ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState141 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_state
    (** State 141.
        Stack shape : CASE expr OF case_clauses.
        Start symbol: compilation_unit. *)

  | MenhirState143 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE, _menhir_box_compilation_unit) _menhir_state
    (** State 143.
        Stack shape : CASE expr OF case_clauses OTHERWISE.
        Start symbol: compilation_unit. *)

  | MenhirState144 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 144.
        Stack shape : CASE expr OF case_clauses OTHERWISE stmt_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState152 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 152.
        Stack shape : FOR ident_loc expr by_opt.
        Start symbol: compilation_unit. *)

  | MenhirState153 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 153.
        Stack shape : FOR ident_loc expr by_opt expr.
        Start symbol: compilation_unit. *)

  | MenhirState155 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_state
    (** State 155.
        Stack shape : IF expr THEN stmt.
        Start symbol: compilation_unit. *)

  | MenhirState156 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_state
    (** State 156.
        Stack shape : IF expr THEN stmt ELSIF.
        Start symbol: compilation_unit. *)

  | MenhirState157 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 157.
        Stack shape : IF expr THEN stmt ELSIF expr.
        Start symbol: compilation_unit. *)

  | MenhirState158 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 158.
        Stack shape : IF expr THEN stmt ELSIF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState160 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ELSE, _menhir_box_compilation_unit) _menhir_state
    (** State 160.
        Stack shape : ELSE.
        Start symbol: compilation_unit. *)

  | MenhirState162 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_state
    (** State 162.
        Stack shape : IF expr THEN stmt elsif_list.
        Start symbol: compilation_unit. *)

  | MenhirState163 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_state
    (** State 163.
        Stack shape : IF expr THEN stmt elsif_list ELSIF.
        Start symbol: compilation_unit. *)

  | MenhirState164 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 164.
        Stack shape : IF expr THEN stmt elsif_list ELSIF expr.
        Start symbol: compilation_unit. *)

  | MenhirState165 : ((((((((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_state
    (** State 165.
        Stack shape : IF expr THEN stmt elsif_list ELSIF expr THEN.
        Start symbol: compilation_unit. *)

  | MenhirState170 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_state
    (** State 170.
        Stack shape : TYPE.
        Start symbol: compilation_unit. *)

  | MenhirState173 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 173.
        Stack shape : TYPE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState175 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPEATOM, _menhir_box_compilation_unit) _menhir_state
    (** State 175.
        Stack shape : TYPEATOM.
        Start symbol: compilation_unit. *)

  | MenhirState181 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_status_list, _menhir_box_compilation_unit) _menhir_state
    (** State 181.
        Stack shape : TYPE ident_loc status_list.
        Start symbol: compilation_unit. *)

  | MenhirState185 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 185.
        Stack shape : TYPE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState191 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_state
    (** State 191.
        Stack shape : TABLE.
        Start symbol: compilation_unit. *)

  | MenhirState193 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 193.
        Stack shape : TABLE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState199 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_dim_list, _menhir_box_compilation_unit) _menhir_state
    (** State 199.
        Stack shape : TABLE ident_loc dim_list.
        Start symbol: compilation_unit. *)

  | MenhirState202 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 202.
        Stack shape : TABLE ident_loc table_dims_opt.
        Start symbol: compilation_unit. *)

  | MenhirState203 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_state
    (** State 203.
        Stack shape : TYPE.
        Start symbol: compilation_unit. *)

  | MenhirState205 : (('s, _menhir_box_compilation_unit) _menhir_cell1_COLON, _menhir_box_compilation_unit) _menhir_state
    (** State 205.
        Stack shape : COLON.
        Start symbol: compilation_unit. *)

  | MenhirState207 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 207.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState209 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_state
    (** State 209.
        Stack shape : WITH.
        Start symbol: compilation_unit. *)

  | MenhirState212 : (('s, _menhir_box_compilation_unit) _menhir_cell1_REP, _menhir_box_compilation_unit) _menhir_state
    (** State 212.
        Stack shape : REP.
        Start symbol: compilation_unit. *)

  | MenhirState213 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_REP, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 213.
        Stack shape : REP expr.
        Start symbol: compilation_unit. *)

  | MenhirState218 : (('s, _menhir_box_compilation_unit) _menhir_cell1_POS, _menhir_box_compilation_unit) _menhir_state
    (** State 218.
        Stack shape : POS.
        Start symbol: compilation_unit. *)

  | MenhirState219 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_POS, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 219.
        Stack shape : POS expr.
        Start symbol: compilation_unit. *)

  | MenhirState222 : (('s, _menhir_box_compilation_unit) _menhir_cell1_OVERLAY, _menhir_box_compilation_unit) _menhir_state
    (** State 222.
        Stack shape : OVERLAY.
        Start symbol: compilation_unit. *)

  | MenhirState224 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LIKE, _menhir_box_compilation_unit) _menhir_state
    (** State 224.
        Stack shape : LIKE.
        Start symbol: compilation_unit. *)

  | MenhirState226 : (('s, _menhir_box_compilation_unit) _menhir_cell1_INSTANCE, _menhir_box_compilation_unit) _menhir_state
    (** State 226.
        Stack shape : INSTANCE.
        Start symbol: compilation_unit. *)

  | MenhirState229 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT, _menhir_box_compilation_unit) _menhir_state
    (** State 229.
        Stack shape : DEFAULT.
        Start symbol: compilation_unit. *)

  | MenhirState230 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 230.
        Stack shape : DEFAULT expr.
        Start symbol: compilation_unit. *)

  | MenhirState234 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_cell1_decl_attr_list, _menhir_box_compilation_unit) _menhir_state
    (** State 234.
        Stack shape : WITH decl_attr_list.
        Start symbol: compilation_unit. *)

  | MenhirState239 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 239.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState240 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 240.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt decl_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState241 : (('s, _menhir_box_compilation_unit) _menhir_cell1_REF, _menhir_box_compilation_unit) _menhir_state
    (** State 241.
        Stack shape : REF.
        Start symbol: compilation_unit. *)

  | MenhirState242 : (('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_state
    (** State 242.
        Stack shape : PROC.
        Start symbol: compilation_unit. *)

  | MenhirState243 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 243.
        Stack shape : PROC ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState244 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_state
    (** State 244.
        Stack shape : ident_loc LPAREN.
        Start symbol: compilation_unit. *)

  | MenhirState248 : (('s, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 248.
        Stack shape : param_mode_opt.
        Start symbol: compilation_unit. *)

  | MenhirState249 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 249.
        Stack shape : param_mode_opt ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState255 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_param_list, _menhir_box_compilation_unit) _menhir_state
    (** State 255.
        Stack shape : ident_loc LPAREN param_list.
        Start symbol: compilation_unit. *)

  | MenhirState259 : (('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_state
    (** State 259.
        Stack shape : FUNCTION.
        Start symbol: compilation_unit. *)

  | MenhirState260 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 260.
        Stack shape : FUNCTION ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState261 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 261.
        Stack shape : FUNCTION ident_loc formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState266 : (('s, _menhir_box_compilation_unit) _menhir_cell1_OVERLAY, _menhir_box_compilation_unit) _menhir_state
    (** State 266.
        Stack shape : OVERLAY.
        Start symbol: compilation_unit. *)

  | MenhirState269 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LABEL, _menhir_box_compilation_unit) _menhir_state
    (** State 269.
        Stack shape : LABEL.
        Start symbol: compilation_unit. *)

  | MenhirState271 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_LABEL, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_state
    (** State 271.
        Stack shape : LABEL ident_list.
        Start symbol: compilation_unit. *)

  | MenhirState273 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_COMMA, _menhir_box_compilation_unit) _menhir_state
    (** State 273.
        Stack shape : ident_list COMMA.
        Start symbol: compilation_unit. *)

  | MenhirState275 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_state
    (** State 275.
        Stack shape : ITEM.
        Start symbol: compilation_unit. *)

  | MenhirState276 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_state
    (** State 276.
        Stack shape : ITEM ident_list.
        Start symbol: compilation_unit. *)

  | MenhirState277 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 277.
        Stack shape : ITEM ident_list type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState280 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 280.
        Stack shape : TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt decl_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState282 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_state
    (** State 282.
        Stack shape : DEFINE.
        Start symbol: compilation_unit. *)

  | MenhirState283 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 283.
        Stack shape : DEFINE ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState285 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_EQUAL, _menhir_box_compilation_unit) _menhir_state
    (** State 285.
        Stack shape : DEFINE ident_loc EQUAL.
        Start symbol: compilation_unit. *)

  | MenhirState286 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 286.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState291 : (('s, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_state
    (** State 291.
        Stack shape : DEF.
        Start symbol: compilation_unit. *)

  | MenhirState294 : (('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_state
    (** State 294.
        Stack shape : BLOCK.
        Start symbol: compilation_unit. *)

  | MenhirState295 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 295.
        Stack shape : BLOCK ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState298 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 298.
        Stack shape : BLOCK ident_loc decl_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState299 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 299.
        Stack shape : BLOCK ident_loc decl_attrs_opt decl_list_opt.
        Start symbol: compilation_unit. *)

  | MenhirState300 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 300.
        Stack shape : BLOCK ident_loc decl_attrs_opt decl_list_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState307 : (((('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 307.
        Stack shape : module_header BEGIN top_items_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState309 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_state
    (** State 309.
        Stack shape : top_items_opt DEF.
        Start symbol: compilation_unit. *)

  | MenhirState310 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_state
    (** State 310.
        Stack shape : top_items_opt DEF PROC.
        Start symbol: compilation_unit. *)

  | MenhirState311 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 311.
        Stack shape : top_items_opt DEF PROC ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState312 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 312.
        Stack shape : top_items_opt DEF PROC ident_loc formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState315 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 315.
        Stack shape : top_items_opt DEF PROC ident_loc formal_params_opt use_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState317 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN _menhir_cell0_block_items_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 317.
        Stack shape : use_attrs_opt BEGIN block_items_opt.
        Start symbol: compilation_unit. *)

  | MenhirState319 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN _menhir_cell0_block_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 319.
        Stack shape : use_attrs_opt BEGIN block_items_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState324 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 324.
        Stack shape : top_items_opt DEF PROC ident_loc formal_params_opt use_attrs_opt proc_body_opt.
        Start symbol: compilation_unit. *)

  | MenhirState325 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_state
    (** State 325.
        Stack shape : use_attrs_opt proc_body_opt END.
        Start symbol: compilation_unit. *)

  | MenhirState326 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_cell1_proc_end_name_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 326.
        Stack shape : use_attrs_opt proc_body_opt END proc_end_name_opt.
        Start symbol: compilation_unit. *)

  | MenhirState335 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_state
    (** State 335.
        Stack shape : top_items_opt DEF FUNCTION.
        Start symbol: compilation_unit. *)

  | MenhirState336 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_state
    (** State 336.
        Stack shape : top_items_opt DEF FUNCTION ident_loc.
        Start symbol: compilation_unit. *)

  | MenhirState337 : (((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 337.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt.
        Start symbol: compilation_unit. *)

  | MenhirState338 : ((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 338.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt type_spec_opt.
        Start symbol: compilation_unit. *)

  | MenhirState340 : (((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 340.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt type_spec_opt use_attrs_opt.
        Start symbol: compilation_unit. *)

  | MenhirState341 : ((((((((('s, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 341.
        Stack shape : top_items_opt DEF FUNCTION ident_loc formal_params_opt type_spec_opt use_attrs_opt proc_body_opt.
        Start symbol: compilation_unit. *)

  | MenhirState347 : (('s _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 347.
        Stack shape : module_header top_items_opt.
        Start symbol: compilation_unit. *)

  | MenhirState357 : ('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_state
    (** State 357.
        Stack shape : directives_opt DIRECTIVE_NAME.
        Start symbol: compilation_unit. *)

  | MenhirState361 : (('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_state
    (** State 361.
        Stack shape : directives_opt DIRECTIVE_NAME LPAREN.
        Start symbol: compilation_unit. *)

  | MenhirState370 : ((('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_directive_arg_list, _menhir_box_compilation_unit) _menhir_state
    (** State 370.
        Stack shape : directives_opt DIRECTIVE_NAME LPAREN directive_arg_list.
        Start symbol: compilation_unit. *)

  | MenhirState375 : (('s _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_directive_args, _menhir_box_compilation_unit) _menhir_state
    (** State 375.
        Stack shape : directives_opt DIRECTIVE_NAME directive_args.
        Start symbol: compilation_unit. *)

  | MenhirState380 : ('s _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt, _menhir_box_compilation_unit) _menhir_state
    (** State 380.
        Stack shape : directives_opt module_kind_opt module_name_opt.
        Start symbol: compilation_unit. *)


and 's _menhir_cell0_block_items_opt = 
  | MenhirCell0_block_items_opt of 's * (Ast.stmt list)

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

and ('s, 'r) _menhir_cell1_proc_end_name_opt = 
  | MenhirCell1_proc_end_name_opt of 's * ('s, 'r) _menhir_state * (Ast.ident_loc option)

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
# 31 "lib/parser.mly"
       (string)
# 1170 "lib/parser.ml"
)

and ('s, 'r) _menhir_cell1_ELSE = 
  | MenhirCell1_ELSE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ELSIF = 
  | MenhirCell1_ELSIF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_END = 
  | MenhirCell1_END of 's * ('s, 'r) _menhir_state * Lexing.position

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

and ('s, 'r) _menhir_cell1_GOTO = 
  | MenhirCell1_GOTO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GQ = 
  | MenhirCell1_GQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GR = 
  | MenhirCell1_GR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GT = 
  | MenhirCell1_GT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_INSTANCE = 
  | MenhirCell1_INSTANCE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ITEM = 
  | MenhirCell1_ITEM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LABEL = 
  | MenhirCell1_LABEL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LE = 
  | MenhirCell1_LE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LIKE = 
  | MenhirCell1_LIKE of 's * ('s, 'r) _menhir_state

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

and ('s, 'r) _menhir_cell1_TYPEATOM = 
  | MenhirCell1_TYPEATOM of 's * ('s, 'r) _menhir_state * (
# 38 "lib/parser.mly"
       (string)
# 1315 "lib/parser.ml"
)

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
# 222 "lib/parser.mly"
                ( [] )
# 1338 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_002 =
  fun _2 ->
    (
# 223 "lib/parser.mly"
                                      ( _2 )
# 1346 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_003 =
  fun () ->
    (
# 364 "lib/parser.mly"
         ( None )
# 1354 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_004 =
  fun _1 ->
    (
# 365 "lib/parser.mly"
         ( Some _1 )
# 1362 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_005 =
  fun () ->
    (
# 366 "lib/parser.mly"
         ( Some SNoop )
# 1370 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_006 =
  fun () ->
    (
# 359 "lib/parser.mly"
                ( [] )
# 1378 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_007 =
  fun _1 _2 ->
    (
# 361 "lib/parser.mly"
      ( match _2 with None -> _1 | Some s -> _1 @ [s] )
# 1386 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_008 =
  fun () ->
    (
# 440 "lib/parser.mly"
                ( None )
# 1394 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_009 =
  fun _2 ->
    (
# 441 "lib/parser.mly"
            ( Some _2 )
# 1402 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_010 =
  fun () ->
    (
# 404 "lib/parser.mly"
                ( [] )
# 1410 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_011 =
  fun _2 ->
    (
# 405 "lib/parser.mly"
                                ( _2 )
# 1418 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_012 =
  fun _3 ->
    (
# 406 "lib/parser.mly"
                                     ( _3 )
# 1426 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_013 =
  fun _1 _2 ->
    (
# 401 "lib/parser.mly"
                                 ( SCall (_1, _2) )
# 1434 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_014 =
  fun _2 _4 ->
    (
# 452 "lib/parser.mly"
                                         ( (_2, _4) )
# 1442 "lib/parser.ml"
     : (Ast.expr list * Ast.stmt list))

let _menhir_action_015 =
  fun _1 ->
    (
# 448 "lib/parser.mly"
                ( [_1] )
# 1450 "lib/parser.ml"
     : ((Ast.expr list * Ast.stmt list) list))

let _menhir_action_016 =
  fun _1 _2 ->
    (
# 449 "lib/parser.mly"
                             ( _1 @ [_2] )
# 1458 "lib/parser.ml"
     : ((Ast.expr list * Ast.stmt list) list))

let _menhir_action_017 =
  fun _1 ->
    (
# 455 "lib/parser.mly"
         ( [_1] )
# 1466 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_018 =
  fun _1 _3 ->
    (
# 456 "lib/parser.mly"
                           ( _1 @ [_3] )
# 1474 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_019 =
  fun _2 _4 _5 ->
    (
# 445 "lib/parser.mly"
      ( SCase (_2, _4, _5) )
# 1482 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_020 =
  fun _1 ->
    (
# 63 "lib/parser.mly"
              ( _1 )
# 1490 "lib/parser.ml"
     : (Ast.module_))

let _menhir_action_021 =
  fun () ->
    (
# 146 "lib/parser.mly"
                ( [] )
# 1498 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_022 =
  fun _1 _3 ->
    (
# 148 "lib/parser.mly"
      ( _1 @ [ { dir_name = "ICOMPOOL"; dir_args = [LString _3] } ] )
# 1506 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_023 =
  fun _2 _3 _4 ->
    (
# 180 "lib/parser.mly"
      ( DItem (_2, _3, _4) )
# 1514 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_024 =
  fun _2 _3 _4 _5 _7 ->
    (
# 183 "lib/parser.mly"
      ( DTable (_2, _3, _4, _5, _7) )
# 1522 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_025 =
  fun _2 _3 _5 ->
    (
# 186 "lib/parser.mly"
      ( DBlock (_2, _3, _5) )
# 1530 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_026 =
  fun _2 _5 ->
    (
# 189 "lib/parser.mly"
      ( DTypeStatus (_2, _5) )
# 1538 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_027 =
  fun _2 _4 ->
    (
# 192 "lib/parser.mly"
      ( DTypeAlias (_2, _4) )
# 1546 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_028 =
  fun _2 ->
    (
# 195 "lib/parser.mly"
      ( DOverlayDecl _2 )
# 1554 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_029 =
  fun _2 _3 ->
    (
# 198 "lib/parser.mly"
      ( DDefine (_2, _3) )
# 1562 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_030 =
  fun _2 _4 ->
    (
# 201 "lib/parser.mly"
      ( DDefine (_2, _4) )
# 1570 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_031 =
  fun _1 ->
    (
# 203 "lib/parser.mly"
                 ( _1 )
# 1578 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_032 =
  fun _2 ->
    (
# 206 "lib/parser.mly"
      ( DLabelDecl _2 )
# 1586 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_033 =
  fun () ->
    (
# 266 "lib/parser.mly"
                             ( DStatic )
# 1594 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_034 =
  fun () ->
    (
# 267 "lib/parser.mly"
                             ( DConstant )
# 1602 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_035 =
  fun _2 ->
    (
# 268 "lib/parser.mly"
                             ( DDefault _2 )
# 1610 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_036 =
  fun _2 ->
    (
# 269 "lib/parser.mly"
                             ( DLike _2 )
# 1618 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_037 =
  fun _3 ->
    (
# 270 "lib/parser.mly"
                             ( DPos _3 )
# 1626 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_038 =
  fun _3 ->
    (
# 271 "lib/parser.mly"
                             ( DRep _3 )
# 1634 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_039 =
  fun _2 ->
    (
# 272 "lib/parser.mly"
                             ( DOverlay _2 )
# 1642 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_040 =
  fun _2 ->
    (
# 273 "lib/parser.mly"
                             ( DInstance _2 )
# 1650 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_041 =
  fun () ->
    (
# 274 "lib/parser.mly"
                             ( DRec )
# 1658 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_042 =
  fun () ->
    (
# 275 "lib/parser.mly"
                             ( DRent )
# 1666 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_043 =
  fun () ->
    (
# 276 "lib/parser.mly"
                             ( DInline )
# 1674 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_044 =
  fun () ->
    (
# 277 "lib/parser.mly"
                             ( DParallel )
# 1682 "lib/parser.ml"
     : (Ast.decl_attr))

let _menhir_action_045 =
  fun _1 ->
    (
# 262 "lib/parser.mly"
            ( [_1] )
# 1690 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_046 =
  fun _1 _3 ->
    (
# 263 "lib/parser.mly"
                                 ( _1 @ [_3] )
# 1698 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_047 =
  fun () ->
    (
# 258 "lib/parser.mly"
                ( [] )
# 1706 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_048 =
  fun _3 ->
    (
# 259 "lib/parser.mly"
                                      ( _3 )
# 1714 "lib/parser.ml"
     : (Ast.decl_attr list))

let _menhir_action_049 =
  fun () ->
    (
# 226 "lib/parser.mly"
                ( [] )
# 1722 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_050 =
  fun _1 _2 ->
    (
# 227 "lib/parser.mly"
                       ( _1 @ [_2] )
# 1730 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_051 =
  fun _1 ->
    (
# 209 "lib/parser.mly"
           ( DefString _1 )
# 1738 "lib/parser.ml"
     : (Ast.define_rhs))

let _menhir_action_052 =
  fun _1 ->
    (
# 210 "lib/parser.mly"
           ( DefExpr _1 )
# 1746 "lib/parser.ml"
     : (Ast.define_rhs))

let _menhir_action_053 =
  fun () ->
    (
# 242 "lib/parser.mly"
          ( DimStar )
# 1754 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_054 =
  fun _1 ->
    (
# 243 "lib/parser.mly"
          ( DimInt _1 )
# 1762 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_055 =
  fun _1 ->
    (
# 244 "lib/parser.mly"
              ( DimId _1 )
# 1770 "lib/parser.ml"
     : (Ast.dim))

let _menhir_action_056 =
  fun _1 ->
    (
# 238 "lib/parser.mly"
      ( [_1] )
# 1778 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_057 =
  fun _1 _3 ->
    (
# 239 "lib/parser.mly"
                     ( _1 @ [_3] )
# 1786 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_058 =
  fun _2 _3 ->
    (
# 116 "lib/parser.mly"
    ( { dir_name = _2; dir_args = _3 } )
# 1794 "lib/parser.ml"
     : (Ast.directive))

let _menhir_action_059 =
  fun _1 ->
    (
# 136 "lib/parser.mly"
         ( LString _1 )
# 1802 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_060 =
  fun _1 ->
    (
# 137 "lib/parser.mly"
         ( LInt _1 )
# 1810 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_061 =
  fun _1 ->
    (
# 138 "lib/parser.mly"
         ( LFloat _1 )
# 1818 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_062 =
  fun _1 ->
    (
# 139 "lib/parser.mly"
         ( let (b,v) = _1 in LBead (b,v) )
# 1826 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_063 =
  fun () ->
    (
# 140 "lib/parser.mly"
         ( LNull )
# 1834 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_064 =
  fun () ->
    (
# 141 "lib/parser.mly"
         ( LBool true )
# 1842 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_065 =
  fun () ->
    (
# 142 "lib/parser.mly"
         ( LBool false )
# 1850 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_066 =
  fun _1 ->
    (
# 143 "lib/parser.mly"
         ( LString _1 )
# 1858 "lib/parser.ml"
     : (Ast.literal))

let _menhir_action_067 =
  fun _1 ->
    (
# 128 "lib/parser.mly"
                ( [_1] )
# 1866 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_068 =
  fun _1 _3 ->
    (
# 129 "lib/parser.mly"
                                         ( _1 @ [_3] )
# 1874 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_069 =
  fun () ->
    (
# 124 "lib/parser.mly"
                ( [] )
# 1882 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_070 =
  fun _1 ->
    (
# 125 "lib/parser.mly"
                       ( _1 )
# 1890 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_071 =
  fun _1 ->
    (
# 132 "lib/parser.mly"
                ( [_1] )
# 1898 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_072 =
  fun _1 _2 ->
    (
# 133 "lib/parser.mly"
                               ( _1 @ [_2] )
# 1906 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_073 =
  fun () ->
    (
# 119 "lib/parser.mly"
                ( [] )
# 1914 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_074 =
  fun _1 ->
    (
# 120 "lib/parser.mly"
                   ( _1 )
# 1922 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_075 =
  fun _2 ->
    (
# 121 "lib/parser.mly"
                                         ( _2 )
# 1930 "lib/parser.ml"
     : (Ast.literal list))

let _menhir_action_076 =
  fun () ->
    (
# 111 "lib/parser.mly"
                ( [] )
# 1938 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_077 =
  fun _1 _2 ->
    (
# 112 "lib/parser.mly"
                             ( _1 @ [_2] )
# 1946 "lib/parser.ml"
     : (Ast.directive list))

let _menhir_action_078 =
  fun () ->
    (
# 426 "lib/parser.mly"
                ( None )
# 1954 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_079 =
  fun _2 ->
    (
# 427 "lib/parser.mly"
              ( Some _2 )
# 1962 "lib/parser.ml"
     : (Ast.stmt option))

let _menhir_action_080 =
  fun _2 _4 ->
    (
# 422 "lib/parser.mly"
                         ( [(_2, _4)] )
# 1970 "lib/parser.ml"
     : ((Ast.expr * Ast.stmt) list))

let _menhir_action_081 =
  fun _1 _3 _5 ->
    (
# 423 "lib/parser.mly"
                                    ( _1 @ [(_3, _5)] )
# 1978 "lib/parser.ml"
     : ((Ast.expr * Ast.stmt) list))

let _menhir_action_082 =
  fun _1 ->
    (
# 393 "lib/parser.mly"
              ( Some _1 )
# 1986 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_083 =
  fun () ->
    (
# 394 "lib/parser.mly"
                ( None )
# 1994 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_084 =
  fun _1 ->
    (
# 469 "lib/parser.mly"
           ( EInt _1 )
# 2002 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_085 =
  fun _1 ->
    (
# 470 "lib/parser.mly"
           ( EFloat _1 )
# 2010 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_086 =
  fun _1 ->
    (
# 471 "lib/parser.mly"
           ( EString _1 )
# 2018 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_087 =
  fun _1 ->
    (
# 472 "lib/parser.mly"
           ( let (b,v) = _1 in EBead (b,v) )
# 2026 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_088 =
  fun () ->
    (
# 473 "lib/parser.mly"
           ( ENull )
# 2034 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_089 =
  fun () ->
    (
# 474 "lib/parser.mly"
           ( EBool true )
# 2042 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_090 =
  fun () ->
    (
# 475 "lib/parser.mly"
           ( EBool false )
# 2050 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_091 =
  fun _1 ->
    (
# 476 "lib/parser.mly"
               ( EVar _1 )
# 2058 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_092 =
  fun _1 _3 ->
    (
# 477 "lib/parser.mly"
                                          ( ECall (_1, _3) )
# 2066 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_093 =
  fun _2 ->
    (
# 478 "lib/parser.mly"
                       ( EParen _2 )
# 2074 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_094 =
  fun _2 ->
    (
# 480 "lib/parser.mly"
                            ( EUn ("-", _2) )
# 2082 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_095 =
  fun _2 ->
    (
# 481 "lib/parser.mly"
             ( EUn ("NOT", _2) )
# 2090 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_096 =
  fun _1 _3 ->
    (
# 483 "lib/parser.mly"
                    ( EBin ("*", _1, _3) )
# 2098 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_097 =
  fun _1 _3 ->
    (
# 484 "lib/parser.mly"
                    ( EBin ("/", _1, _3) )
# 2106 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_098 =
  fun _1 _3 ->
    (
# 485 "lib/parser.mly"
                    ( EBin ("MOD", _1, _3) )
# 2114 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_099 =
  fun _1 _3 ->
    (
# 486 "lib/parser.mly"
                    ( EBin ("+", _1, _3) )
# 2122 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_100 =
  fun _1 _3 ->
    (
# 487 "lib/parser.mly"
                    ( EBin ("-", _1, _3) )
# 2130 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_101 =
  fun _1 _3 ->
    (
# 489 "lib/parser.mly"
                    ( EBin ("<", _1, _3) )
# 2138 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_102 =
  fun _1 _3 ->
    (
# 490 "lib/parser.mly"
                    ( EBin ("<=", _1, _3) )
# 2146 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_103 =
  fun _1 _3 ->
    (
# 491 "lib/parser.mly"
                    ( EBin (">", _1, _3) )
# 2154 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_104 =
  fun _1 _3 ->
    (
# 492 "lib/parser.mly"
                    ( EBin (">=", _1, _3) )
# 2162 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_105 =
  fun _1 _3 ->
    (
# 493 "lib/parser.mly"
                    ( EBin ("<>", _1, _3) )
# 2170 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_106 =
  fun _1 _3 ->
    (
# 494 "lib/parser.mly"
                    ( EBin ("=", _1, _3) )
# 2178 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_107 =
  fun _1 _3 ->
    (
# 496 "lib/parser.mly"
                    ( EBin ("EQ", _1, _3) )
# 2186 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_108 =
  fun _1 _3 ->
    (
# 497 "lib/parser.mly"
                    ( EBin ("NQ", _1, _3) )
# 2194 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_109 =
  fun _1 _3 ->
    (
# 498 "lib/parser.mly"
                    ( EBin ("LS", _1, _3) )
# 2202 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_110 =
  fun _1 _3 ->
    (
# 499 "lib/parser.mly"
                    ( EBin ("LQ", _1, _3) )
# 2210 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_111 =
  fun _1 _3 ->
    (
# 500 "lib/parser.mly"
                    ( EBin ("GR", _1, _3) )
# 2218 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_112 =
  fun _1 _3 ->
    (
# 501 "lib/parser.mly"
                    ( EBin ("GQ", _1, _3) )
# 2226 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_113 =
  fun _1 _3 ->
    (
# 503 "lib/parser.mly"
                    ( EBin ("AND", _1, _3) )
# 2234 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_114 =
  fun _1 _3 ->
    (
# 504 "lib/parser.mly"
                    ( EBin ("OR", _1, _3) )
# 2242 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_115 =
  fun _1 _3 ->
    (
# 505 "lib/parser.mly"
                    ( EBin ("XOR", _1, _3) )
# 2250 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_116 =
  fun _1 _3 ->
    (
# 506 "lib/parser.mly"
                    ( EBin ("EQV", _1, _3) )
# 2258 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_117 =
  fun _1 ->
    (
# 413 "lib/parser.mly"
         ( [_1] )
# 2266 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_118 =
  fun _1 _3 ->
    (
# 414 "lib/parser.mly"
                         ( _1 @ [_3] )
# 2274 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_119 =
  fun () ->
    (
# 409 "lib/parser.mly"
                ( [] )
# 2282 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_120 =
  fun _1 ->
    (
# 410 "lib/parser.mly"
              ( _1 )
# 2290 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_121 =
  fun () ->
    (
# 397 "lib/parser.mly"
                ( None )
# 2298 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_122 =
  fun _1 ->
    (
# 398 "lib/parser.mly"
         ( Some _1 )
# 2306 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_123 =
  fun _2 _4 _6 _7 _8 ->
    (
# 434 "lib/parser.mly"
      ( SForTo (_2, _4, _6, _7, _8) )
# 2314 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_124 =
  fun _2 _4 _5 _7 _8 ->
    (
# 437 "lib/parser.mly"
      ( SForWhile (_2, _4, _5, _7, _8) )
# 2322 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_125 =
  fun () ->
    (
# 326 "lib/parser.mly"
                ( [] )
# 2330 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_126 =
  fun _2 ->
    (
# 327 "lib/parser.mly"
                                 ( _2 )
# 2338 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_127 =
  fun _1 ->
    (
# 283 "lib/parser.mly"
              ( [_1] )
# 2346 "lib/parser.ml"
     : (Ast.ident_loc list))

let _menhir_action_128 =
  fun _1 _3 ->
    (
# 284 "lib/parser.mly"
                               ( _1 @ [_3] )
# 2354 "lib/parser.ml"
     : (Ast.ident_loc list))

let _menhir_action_129 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 280 "lib/parser.mly"
          ( mk_loc _1 _startpos _endpos )
# 2364 "lib/parser.ml"
     : (Ast.ident_loc))

let _menhir_action_130 =
  fun _2 _4 _5 ->
    (
# 417 "lib/parser.mly"
                               ( SIf (_2, _4, _5) )
# 2372 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_131 =
  fun _2 _4 _5 _6 ->
    (
# 419 "lib/parser.mly"
      ( SIfElsif (((_2,_4) :: _5), _6) )
# 2380 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_132 =
  fun _2 ->
    (
# 213 "lib/parser.mly"
                            ( DLinkage (LDef, _2) )
# 2388 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_133 =
  fun _2 ->
    (
# 214 "lib/parser.mly"
                            ( DLinkage (LRef, _2) )
# 2396 "lib/parser.ml"
     : (Ast.decl))

let _menhir_action_134 =
  fun _1 ->
    (
# 217 "lib/parser.mly"
              ( LName _1 )
# 2404 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_135 =
  fun _2 _3 ->
    (
# 218 "lib/parser.mly"
                                     ( LProcSig (_2, _3) )
# 2412 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_136 =
  fun _2 _3 _4 ->
    (
# 219 "lib/parser.mly"
                                                       ( LFunSig (_2, _3, _4) )
# 2420 "lib/parser.ml"
     : (Ast.linkage_target))

let _menhir_action_137 =
  fun _1 ->
    (
# 463 "lib/parser.mly"
              ( LVar _1 )
# 2428 "lib/parser.ml"
     : (Ast.lvalue))

let _menhir_action_138 =
  fun _1 _3 ->
    (
# 464 "lib/parser.mly"
                                          ( LIndex (_1, _3) )
# 2436 "lib/parser.ml"
     : (Ast.lvalue))

let _menhir_action_139 =
  fun _2 _3 ->
    (
# 67 "lib/parser.mly"
    (
      let (kind, name, directives, attrs) = _2 in
      let (decls, stmts, procs) = _3 in
      { m_kind = kind
      ; m_name = name
      ; m_directives = directives
      ; m_attrs = attrs
      ; m_decls = decls
      ; m_stmts = stmts
      ; m_procs = procs
      }
    )
# 2455 "lib/parser.ml"
     : (Ast.module_))

let _menhir_action_140 =
  fun _2 ->
    (
# 151 "lib/parser.mly"
                                      ( _2 )
# 2463 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_141 =
  fun _1 ->
    (
# 152 "lib/parser.mly"
                  ( _1 )
# 2471 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_142 =
  fun _1 _2 _3 _4 _5 ->
    (
# 82 "lib/parser.mly"
    (
      let directives = _1 @ _5 in
      (_2, _3, directives, _4)
    )
# 2482 "lib/parser.ml"
     : (Ast.module_kind * Ast.ident_loc option * Ast.directive list *
  Ast.use_attr list))

let _menhir_action_143 =
  fun () ->
    (
# 88 "lib/parser.mly"
            ( Program )
# 2491 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_144 =
  fun () ->
    (
# 89 "lib/parser.mly"
            ( Compool )
# 2499 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_145 =
  fun () ->
    (
# 90 "lib/parser.mly"
             ( Icompool )
# 2507 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_146 =
  fun () ->
    (
# 91 "lib/parser.mly"
            ( Proc_module )
# 2515 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_147 =
  fun () ->
    (
# 92 "lib/parser.mly"
             ( Function_module )
# 2523 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_148 =
  fun () ->
    (
# 93 "lib/parser.mly"
                ( Unknown )
# 2531 "lib/parser.ml"
     : (Ast.module_kind))

let _menhir_action_149 =
  fun _1 _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 96 "lib/parser.mly"
          ( Some (mk_loc _1 _startpos _endpos) )
# 2541 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_150 =
  fun () ->
    (
# 97 "lib/parser.mly"
                ( None )
# 2549 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_151 =
  fun () ->
    (
# 459 "lib/parser.mly"
                ( None )
# 2557 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_152 =
  fun _3 ->
    (
# 460 "lib/parser.mly"
                                  ( Some _3 )
# 2565 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_153 =
  fun _1 _2 _3 ->
    (
# 339 "lib/parser.mly"
      ( { prm_mode = _1; prm_name = _2; prm_type = _3 } )
# 2573 "lib/parser.ml"
     : (Ast.param))

let _menhir_action_154 =
  fun _1 ->
    (
# 334 "lib/parser.mly"
          ( [_1] )
# 2581 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_155 =
  fun _1 _3 ->
    (
# 335 "lib/parser.mly"
                           ( _1 @ [_3] )
# 2589 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_156 =
  fun () ->
    (
# 330 "lib/parser.mly"
                ( [] )
# 2597 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_157 =
  fun _1 ->
    (
# 331 "lib/parser.mly"
               ( _1 )
# 2605 "lib/parser.ml"
     : (Ast.param list))

let _menhir_action_158 =
  fun () ->
    (
# 342 "lib/parser.mly"
                ( None )
# 2613 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_159 =
  fun () ->
    (
# 343 "lib/parser.mly"
          ( Some ByRef )
# 2621 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_160 =
  fun () ->
    (
# 344 "lib/parser.mly"
          ( Some ByVal )
# 2629 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_161 =
  fun () ->
    (
# 345 "lib/parser.mly"
          ( Some ByRes )
# 2637 "lib/parser.ml"
     : (Ast.param_mode option))

let _menhir_action_162 =
  fun () ->
    (
# 348 "lib/parser.mly"
                ( None )
# 2645 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_163 =
  fun _2 ->
    (
# 349 "lib/parser.mly"
                                        ( Some _2 )
# 2653 "lib/parser.ml"
     : (Ast.stmt list option))

let _menhir_action_164 =
  fun _3 _4 _5 _7 _8 _startpos__1_ ->
    let _startpos = _startpos__1_ in
    (
# 298 "lib/parser.mly"
      (
        let endpos = _8 in
        { pr_kind = PProc
        ; pr_name = _3
        ; pr_span = mk_span _startpos endpos
        ; pr_params = _4
        ; pr_rettype = None
        ; pr_attrs = _5
        ; pr_directives = []
        ; pr_body = _7
        }
      )
# 2673 "lib/parser.ml"
     : (Ast.proc))

let _menhir_action_165 =
  fun _3 _4 _5 _6 _8 _9 _startpos__1_ ->
    let _startpos = _startpos__1_ in
    (
# 312 "lib/parser.mly"
      (
        let endpos = _9 in
        { pr_kind = PFunction
        ; pr_name = _3
        ; pr_span = mk_span _startpos endpos
        ; pr_params = _4
        ; pr_rettype = _5
        ; pr_attrs = _6
        ; pr_directives = []
        ; pr_body = _8
        }
      )
# 2693 "lib/parser.ml"
     : (Ast.proc))

let _menhir_action_166 =
  fun _endpos_e_ e ->
    (
# 352 "lib/parser.mly"
                                      ( ignore e; _endpos_e_ )
# 2701 "lib/parser.ml"
     : (Lexing.position))

let _menhir_action_167 =
  fun _1 ->
    (
# 355 "lib/parser.mly"
              ( Some _1 )
# 2709 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_168 =
  fun () ->
    (
# 356 "lib/parser.mly"
                ( None )
# 2717 "lib/parser.ml"
     : (Ast.ident_loc option))

let _menhir_action_169 =
  fun () ->
    (
# 155 "lib/parser.mly"
                ( () )
# 2725 "lib/parser.ml"
     : (unit))

let _menhir_action_170 =
  fun () ->
    (
# 156 "lib/parser.mly"
                   ( () )
# 2733 "lib/parser.ml"
     : (unit))

let _menhir_action_171 =
  fun _1 ->
    (
# 291 "lib/parser.mly"
              ( SName _1 )
# 2741 "lib/parser.ml"
     : (Ast.status_item))

let _menhir_action_172 =
  fun _3 ->
    (
# 292 "lib/parser.mly"
                                     ( SVal _3 )
# 2749 "lib/parser.ml"
     : (Ast.status_item))

let _menhir_action_173 =
  fun _1 ->
    (
# 287 "lib/parser.mly"
              ( [_1] )
# 2757 "lib/parser.ml"
     : (Ast.status_item list))

let _menhir_action_174 =
  fun _1 _3 ->
    (
# 288 "lib/parser.mly"
                                ( _1 @ [_3] )
# 2765 "lib/parser.ml"
     : (Ast.status_item list))

let _menhir_action_175 =
  fun _1 _3 ->
    (
# 371 "lib/parser.mly"
                         ( SLabel (_1, _3) )
# 2773 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_176 =
  fun _1 _3 ->
    (
# 372 "lib/parser.mly"
                           ( SAssign (_1, _3) )
# 2781 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_177 =
  fun _1 ->
    (
# 373 "lib/parser.mly"
              ( _1 )
# 2789 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_178 =
  fun _1 ->
    (
# 374 "lib/parser.mly"
            ( _1 )
# 2797 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_179 =
  fun _1 ->
    (
# 375 "lib/parser.mly"
               ( _1 )
# 2805 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_180 =
  fun _1 ->
    (
# 376 "lib/parser.mly"
             ( _1 )
# 2813 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_181 =
  fun _1 ->
    (
# 377 "lib/parser.mly"
              ( _1 )
# 2821 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_182 =
  fun _2 ->
    (
# 378 "lib/parser.mly"
                        ( SGoto _2 )
# 2829 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_183 =
  fun _2 ->
    (
# 379 "lib/parser.mly"
                         ( SReturn _2 )
# 2837 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_184 =
  fun _2 ->
    (
# 380 "lib/parser.mly"
                            ( SExit _2 )
# 2845 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_185 =
  fun _2 ->
    (
# 381 "lib/parser.mly"
                       ( SStop _2 )
# 2853 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_186 =
  fun _2 ->
    (
# 382 "lib/parser.mly"
                        ( SAbort _2 )
# 2861 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_187 =
  fun () ->
    (
# 383 "lib/parser.mly"
                  ( SFallthru )
# 2869 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_188 =
  fun _2 ->
    (
# 384 "lib/parser.mly"
                                      ( SBlock _2 )
# 2877 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_189 =
  fun () ->
    (
# 385 "lib/parser.mly"
         ( SNoop )
# 2885 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_190 =
  fun () ->
    (
# 389 "lib/parser.mly"
                ( [] )
# 2893 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_191 =
  fun _1 _2 ->
    (
# 390 "lib/parser.mly"
                       ( _1 @ [_2] )
# 2901 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_192 =
  fun () ->
    (
# 230 "lib/parser.mly"
                ( [] )
# 2909 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_193 =
  fun _2 ->
    (
# 231 "lib/parser.mly"
                                      ( _2 )
# 2917 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_194 =
  fun () ->
    (
# 234 "lib/parser.mly"
                ( [] )
# 2925 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_195 =
  fun _2 ->
    (
# 235 "lib/parser.mly"
                           ( _2 )
# 2933 "lib/parser.ml"
     : (Ast.dim list))

let _menhir_action_196 =
  fun _1 ->
    (
# 170 "lib/parser.mly"
         ( `Decl _1 )
# 2941 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_197 =
  fun _1 ->
    (
# 171 "lib/parser.mly"
             ( `Proc _1 )
# 2949 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_198 =
  fun _1 ->
    (
# 172 "lib/parser.mly"
         ( `Stmt _1 )
# 2957 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_199 =
  fun () ->
    (
# 173 "lib/parser.mly"
         ( `Stmt SNoop )
# 2965 "lib/parser.ml"
     : ([ `Decl of Ast.decl | `Proc of Ast.proc | `Stmt of Ast.stmt ]))

let _menhir_action_200 =
  fun () ->
    (
# 159 "lib/parser.mly"
                ( (([] : Ast.decl list), ([] : Ast.stmt list), ([] : Ast.proc list)) )
# 2973 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_201 =
  fun _1 _2 ->
    (
# 161 "lib/parser.mly"
      (
        let (ds, ss, ps) = _1 in
        match _2 with
        | `Decl d -> (ds @ [d], ss, ps)
        | `Stmt s -> (ds, ss @ [s], ps)
        | `Proc p -> (ds, ss, ps @ [p])
      )
# 2987 "lib/parser.ml"
     : (Ast.decl list * Ast.stmt list * Ast.proc list))

let _menhir_action_202 =
  fun _1 _2 ->
    (
# 253 "lib/parser.mly"
                 ( TAtom (_1 ^ " " ^ string_of_int _2) )
# 2995 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_203 =
  fun _1 ->
    (
# 254 "lib/parser.mly"
                 ( TAtom _1 )
# 3003 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_204 =
  fun _1 ->
    (
# 255 "lib/parser.mly"
                 ( TNamed _1 )
# 3011 "lib/parser.ml"
     : (Ast.type_spec))

let _menhir_action_205 =
  fun () ->
    (
# 247 "lib/parser.mly"
                ( None )
# 3019 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_206 =
  fun _1 ->
    (
# 248 "lib/parser.mly"
              ( Some _1 )
# 3027 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_207 =
  fun _2 ->
    (
# 249 "lib/parser.mly"
                    ( Some _2 )
# 3035 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_208 =
  fun _2 ->
    (
# 250 "lib/parser.mly"
                   ( Some _2 )
# 3043 "lib/parser.ml"
     : (Ast.type_spec option))

let _menhir_action_209 =
  fun () ->
    (
# 104 "lib/parser.mly"
             ( ARec )
# 3051 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_210 =
  fun () ->
    (
# 105 "lib/parser.mly"
             ( ARent )
# 3059 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_211 =
  fun () ->
    (
# 106 "lib/parser.mly"
             ( AStatic )
# 3067 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_212 =
  fun () ->
    (
# 107 "lib/parser.mly"
             ( AParallel )
# 3075 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_213 =
  fun () ->
    (
# 108 "lib/parser.mly"
             ( AInline )
# 3083 "lib/parser.ml"
     : (Ast.use_attr))

let _menhir_action_214 =
  fun () ->
    (
# 100 "lib/parser.mly"
                ( [] )
# 3091 "lib/parser.ml"
     : (Ast.use_attr list))

let _menhir_action_215 =
  fun _1 _2 ->
    (
# 101 "lib/parser.mly"
                           ( _1 @ [_2] )
# 3099 "lib/parser.ml"
     : (Ast.use_attr list))

let _menhir_action_216 =
  fun _2 _3 ->
    (
# 430 "lib/parser.mly"
                    ( SWhile (_2, _3) )
# 3107 "lib/parser.ml"
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
      let _v = _menhir_action_139 _2 _3 in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_020 _1 in
          MenhirBox_compilation_unit _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_381 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt _menhir_cell0_module_name_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RENT ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_021 () in
          _menhir_goto_compool_includes_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_314 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_211 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_use_attr : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_215 _1 _2 in
      _menhir_goto_use_attrs_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_use_attrs_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState380 ->
          _menhir_run_381 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState338 ->
          _menhir_run_339 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState312 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_339 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState340
          | END ->
              let _v_0 = _menhir_action_162 () in
              _menhir_run_341 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState340
          | _ ->
              _eRR ())
      | RENT ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_316 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_006 () in
      _menhir_goto_block_items_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_block_items_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_block_items_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | TYPE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | TABLE ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          _menhir_goto_block_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | REF ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | OVERLAY ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | LABEL ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | ITEM ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | IDENT _v_1 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState317
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState317, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_169 () in
          _menhir_run_320 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState319 _tok
      | DEFINE ->
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | DEF ->
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | BLOCK ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_089 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState283 ->
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState285 ->
          _menhir_run_286 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState229 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState011 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState131 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_049 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_047 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState032 ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState030 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState026 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState024 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_286 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState286
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_052 _1 in
          _menhir_goto_define_rhs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_XOR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState024 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_086 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_088 () in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_084 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_1, _endpos__1_, _startpos__1_) = (_v, _endpos, _startpos) in
      let _v = _menhir_action_129 _1 _endpos__1_ _startpos__1_ in
      _menhir_goto_ident_loc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident_loc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState335 ->
          _menhir_run_336 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState294 ->
          _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_283 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState273 ->
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState275 ->
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState269 ->
          _menhir_run_270 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState266 ->
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState309 ->
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState241 ->
          _menhir_run_265 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState259 ->
          _menhir_run_260 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState248 ->
          _menhir_run_249 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState242 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState226 ->
          _menhir_run_227 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState224 ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState222 ->
          _menhir_run_223 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState199 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState193 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState191 ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState203 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState173 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState175 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState170 ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState347 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState004 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState165 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState160 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState283 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState285 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState229 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState218 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState156 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState152 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState131 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState128 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState009 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState058 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState056 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState052 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState050 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState048 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState046 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState040 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState032 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState028 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState026 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState024 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState011 ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_336 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState336
      | COLON | IDENT _ | INLINE | PARALLEL | REC | RENT | SEMI | STATIC | TYPE | TYPEATOM _ ->
          let _v_0 = _menhir_action_125 () in
          _menhir_run_337 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState336 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_244 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState244 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | BYVAL ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BYRES ->
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BYREF ->
          _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RPAREN ->
          let _v = _menhir_action_156 () in
          _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | IDENT _ ->
          _menhir_reduce_158 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_245 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_160 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_mode_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_param_mode_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState248
      | _ ->
          _eRR ()
  
  and _menhir_run_246 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_161 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_247 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_159 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_126 _2 in
      _menhir_goto_formal_params_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_formal_params_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState336 ->
          _menhir_run_337 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState311 ->
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState243 ->
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_337 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState337
      | TYPE ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState337
      | IDENT _v_1 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState337
      | COLON ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState337
      | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
          let _v_2 = _menhir_action_205 () in
          _menhir_run_338 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState337 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_186 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _2) = (_v, _v_0) in
          let _v = _menhir_action_202 _1 _2 in
          _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA | INLINE | PARALLEL | REC | RENT | RPAREN | SEMI | STATIC | WITH ->
          let _1 = _v in
          let _v = _menhir_action_203 _1 in
          _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_type_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState337 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState202 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_251 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_206 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState203 ->
          _menhir_run_204 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState185 ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_251 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_206 _1 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_spec_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState337 ->
          _menhir_run_338 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState276 ->
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState249 ->
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState202 ->
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_338 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_214 () in
      _menhir_run_339 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState338 _tok
  
  and _menhir_run_277 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState277
      | SEMI ->
          let _v_0 = _menhir_action_047 () in
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_208 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WITH (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState209 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATIC ->
              _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REP ->
              _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RENT ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REC ->
              _menhir_run_216 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | POS ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARALLEL ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OVERLAY ->
              _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIKE ->
              _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INSTANCE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INLINE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEFAULT ->
              _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_210 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_033 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_attr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState209 ->
          _menhir_run_236 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState234 ->
          _menhir_run_235 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_236 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_045 _1 in
      _menhir_goto_decl_attr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_attr_list : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_WITH (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_048 _3 in
          _menhir_goto_decl_attrs_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_decl_attr_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState234 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STATIC ->
              _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REP ->
              _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RENT ->
              _menhir_run_215 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | REC ->
              _menhir_run_216 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | POS ->
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PARALLEL ->
              _menhir_run_221 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | OVERLAY ->
              _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LIKE ->
              _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INSTANCE ->
              _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INLINE ->
              _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEFAULT ->
              _menhir_run_229 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONSTANT ->
              _menhir_run_231 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl_attrs_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState295 ->
          _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState277 ->
          _menhir_run_278 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState207 ->
          _menhir_run_237 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_296 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_049 () in
              _menhir_run_299 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState298 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v = _menhir_action_001 () in
              _menhir_goto_block_decl_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_299 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | TABLE ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | REF ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | OVERLAY ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | LABEL ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | ITEM ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState299, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_169 () in
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState300 _tok
      | DEFINE ->
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | DEF ->
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | BLOCK ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState299
      | _ ->
          _eRR ()
  
  and _menhir_run_170 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState170 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_191 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TABLE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState191 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_241 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState241 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_242 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PROC (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState242 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_259 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState259 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_266 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OVERLAY (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState266 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_269 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LABEL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState269 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_275 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ITEM (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState275 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_301 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_decl_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let _v = _menhir_action_002 _2 in
          _menhir_goto_block_decl_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_semis_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_170 () in
      _menhir_goto_semis_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_semis_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState326 ->
          _menhir_run_327 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState319 ->
          _menhir_run_320 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState307 ->
          _menhir_run_308 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState280 ->
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_327 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END, _menhir_box_compilation_unit) _menhir_cell1_proc_end_name_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_proc_end_name_opt (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_END (_menhir_stack, _menhir_s, _endpos_e_) = _menhir_stack in
          let e = () in
          let _v = _menhir_action_166 _endpos_e_ e in
          _menhir_goto_proc_end _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_end : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState341 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState324 ->
          _menhir_run_329 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_342 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc_body_opt (_menhir_stack, _, _8) = _menhir_stack in
      let MenhirCell1_use_attrs_opt (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_type_spec_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_DEF (_menhir_stack, _, _startpos__1_) = _menhir_stack in
      let _9 = _v in
      let _v = _menhir_action_165 _3 _4 _5 _6 _8 _9 _startpos__1_ in
      _menhir_goto_proc_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_proc_def : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_197 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_top_item : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_201 _1 _2 in
      _menhir_goto_top_items_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_top_items_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState002 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_347 : type  ttv_stack. (ttv_stack _menhir_cell0_module_header as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | TYPE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | TABLE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | STOP ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | SEMI ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_306 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RETURN ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | REF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | OVERLAY ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | LABEL ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | ITEM ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | IF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState347
      | GOTO ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | FOR ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | EXIT ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | DEFINE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | DEF ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | CASE ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | BLOCK ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | BEGIN ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | ABORT ->
          let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState347
      | TERM ->
          let _1 = _v in
          let _v = _menhir_action_141 _1 in
          _menhir_goto_module_body _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STOP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | FLOAT _v ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState072
      | SEMI ->
          let _v = _menhir_action_121 () in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_085 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_015 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_090 () in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_087 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_STOP -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_STOP (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_185 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState347 ->
          _menhir_run_344 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState004 ->
          _menhir_run_344 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState317 ->
          _menhir_run_321 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState071 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState165 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState160 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState082 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState094 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState134 ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState144 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState110 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState112 ->
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_344 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_198 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_321 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN _menhir_cell0_block_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_004 _1 in
      _menhir_goto_block_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_block_item : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN _menhir_cell0_block_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_block_items_opt (_menhir_stack, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_007 _1 _2 in
      _menhir_goto_block_items_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_169 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_216 _2 _3 in
      let _1 = _v in
      let _v = _menhir_action_179 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_166 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ELSIF (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_elsif_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_081 _1 _3 _5 in
      _menhir_goto_elsif_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_elsif_list : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_elsif_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSIF ->
          let _menhir_stack = MenhirCell1_ELSIF (_menhir_stack, MenhirState162) in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ELSE ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState162
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let _v_5 = _menhir_action_078 () in
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ELSE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState160 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEMI ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_189 () in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_077 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077
      | FLOAT _v ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077
      | SEMI ->
          let _v = _menhir_action_121 () in
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_078 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_RETURN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_183 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_080 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState080 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GOTO (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState083 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FOR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_095 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_187 () in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EXIT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SEMI ->
          let _v = _menhir_action_083 () in
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
          let _v = _menhir_action_184 _2 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState101 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_111 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_190 () in
      _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState111 _tok
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | SEMI ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | IDENT _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState112
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState112, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_169 () in
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState113 _tok
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | ELSE | ELSIF | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_stmt_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_188 _2 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ABORT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | STRING _v ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | INT _v ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116
      | FLOAT _v ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116
      | SEMI ->
          let _v = _menhir_action_121 () in
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ABORT -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ABORT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_186 _2 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_167 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_elsif_list (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_stmt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_131 _2 _4 _5 _6 in
      _menhir_goto_if_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_if_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_178 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_161 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ELSE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ELSE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_079 _2 in
      _menhir_goto_else_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_else_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState155 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState162 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_168 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_130 _2 _4 _5 in
      _menhir_goto_if_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_159 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_THEN (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_ELSIF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_080 _2 _4 in
      _menhir_goto_elsif_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_155 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSIF ->
          let _menhir_stack = MenhirCell1_ELSIF (_menhir_stack, MenhirState155) in
          let _menhir_s = MenhirState156 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ELSE ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState155
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OTHERWISE | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHEN | WHILE ->
          let _v_5 = _menhir_action_078 () in
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_154 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_by_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_124 _2 _4 _5 _7 _8 in
      _menhir_goto_for_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_for_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_180 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_150 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_by_opt (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _6) = _menhir_stack in
      let MenhirCell1_TO (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_123 _2 _4 _6 _7 _8 in
      _menhir_goto_for_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_135 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_175 _1 _3 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_120 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_stmt_list_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_191 _1 _2 in
      _menhir_goto_stmt_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt_list_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState143 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_144 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses, _menhir_box_compilation_unit) _menhir_cell1_OTHERWISE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | STOP ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | SEMI ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | RETURN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | IF ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState144
      | GOTO ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | FOR ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | EXIT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | CASE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | BEGIN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | ABORT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | END ->
          let MenhirCell1_OTHERWISE (_menhir_stack, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_152 _3 in
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
          let _v = _menhir_action_019 _2 _4 _5 in
          let _1 = _v in
          let _v = _menhir_action_181 _1 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | STOP ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | SEMI ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | RETURN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | IF ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState110
      | GOTO ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | FOR ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | FALLTHRU ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | EXIT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | CASE ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | BEGIN ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | ABORT ->
          let _menhir_stack = MenhirCell1_stmt_list_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | END | OTHERWISE | WHEN ->
          let MenhirCell1_case_labels (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_WHEN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_014 _2 _4 in
          _menhir_goto_case_clause _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_case_clause : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState103 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_149 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_015 _1 in
      _menhir_goto_case_clauses _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_case_clauses : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_case_clauses (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHEN ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
      | OTHERWISE ->
          let _menhir_stack = MenhirCell1_OTHERWISE (_menhir_stack, MenhirState141) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | COLON ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_190 () in
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState143 _tok
          | _ ->
              _eRR ())
      | END ->
          let _v = _menhir_action_151 () in
          _menhir_goto_otherwise_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_104 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHEN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState104 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_148 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OF, _menhir_box_compilation_unit) _menhir_cell1_case_clauses -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_case_clauses (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_016 _1 _2 in
      _menhir_goto_case_clauses _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_306 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_199 () in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_282 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFINE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState282 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_309 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_DEF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState309 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          let _menhir_stack = MenhirCell1_PROC (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState310 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          let _menhir_stack = MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState335 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_294 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BLOCK (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState294 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. ((ttv_stack _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_top_items_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | TYPE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | TABLE ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | SEMI ->
          _menhir_run_306 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | REF ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | OVERLAY ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | LABEL ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | ITEM ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | IDENT _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState004
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState004, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_169 () in
          _menhir_run_308 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState307 _tok
      | DEFINE ->
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | DEF ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | BLOCK ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState004
      | _ ->
          _eRR ()
  
  and _menhir_run_308 : type  ttv_stack. ((((ttv_stack _menhir_cell0_module_header, _menhir_box_compilation_unit) _menhir_cell1_BEGIN, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | TERM ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_top_items_opt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _) = _menhir_stack in
          let _v = _menhir_action_140 _2 in
          _menhir_goto_module_body _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_329 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_proc_body_opt (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_use_attrs_opt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_PROC (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_DEF (_menhir_stack, _, _startpos__1_) = _menhir_stack in
      let _8 = _v in
      let _v = _menhir_action_164 _3 _4 _5 _7 _8 _startpos__1_ in
      _menhir_goto_proc_def _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_320 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN _menhir_cell0_block_items_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | END ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell0_block_items_opt (_menhir_stack, _2) = _menhir_stack in
          let MenhirCell1_BEGIN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_163 _2 in
          _menhir_goto_proc_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_body_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState340 ->
          _menhir_run_341 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState315 ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_341 : type  ttv_stack. ((((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_proc_body_opt (_menhir_stack, _menhir_s, _v) in
      _menhir_run_325 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState341
  
  and _menhir_run_325 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_END (_menhir_stack, _menhir_s, _endpos) in
      let _menhir_s = MenhirState325 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_168 () in
          _menhir_goto_proc_end_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_proc_end_name_opt : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_proc_end_name_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_169 () in
      _menhir_run_327 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState326 _tok
  
  and _menhir_run_324 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_proc_body_opt (_menhir_stack, _menhir_s, _v) in
      _menhir_run_325 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState324
  
  and _menhir_run_281 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _menhir_stack = MenhirCell1_semis_opt (_menhir_stack, _menhir_s, _v) in
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | STOP | TABLE | TERM | TYPE | WHILE ->
          let MenhirCell1_END (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_decl_list_opt (_menhir_stack, _, _2) = _menhir_stack in
          let _v = _menhir_action_193 _2 in
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
      let _v = _menhir_action_024 _2 _3 _4 _5 _7 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState347 ->
          _menhir_run_346 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState004 ->
          _menhir_run_346 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState317 ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState240 ->
          _menhir_run_303 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState299 ->
          _menhir_run_303 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_346 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_196 _1 in
      _menhir_goto_top_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_322 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_BEGIN _menhir_cell0_block_items_opt -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _v = _menhir_action_003 () in
      _menhir_goto_block_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_303 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_list_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_050 _1 _2 in
      _menhir_goto_decl_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_list_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState298 ->
          _menhir_run_299 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState239 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_240 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_list_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPE ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | TABLE ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | REF ->
          _menhir_run_241 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | OVERLAY ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | LABEL ->
          _menhir_run_269 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | ITEM ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | END ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _menhir_stack = MenhirCell1_END (_menhir_stack, MenhirState240, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_169 () in
          _menhir_run_281 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState280 _tok
      | DEFINE ->
          _menhir_run_282 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | DEF ->
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | BLOCK ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState240
      | _ ->
          _eRR ()
  
  and _menhir_run_291 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_DEF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState291 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROC ->
          _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUNCTION ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_block_decl_body_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_decl_attrs_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attrs_opt (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_BLOCK (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_025 _2 _3 _5 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_278 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_spec_opt (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_ident_list (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_ITEM (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_023 _2 _3 _4 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_237 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt, _menhir_box_compilation_unit) _menhir_cell1_type_spec_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_049 () in
              _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState239 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | END | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v = _menhir_action_192 () in
              _menhir_goto_table_body_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_211 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_REP (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState212 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_215 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_042 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_216 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_041 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_217 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_POS (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState218 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_221 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_044 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_222 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OVERLAY (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState222 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_224 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LIKE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState224 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_226 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_INSTANCE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState226 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_228 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_043 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_229 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState229 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_231 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_034 () in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_235 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WITH, _menhir_box_compilation_unit) _menhir_cell1_decl_attr_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_attr_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_046 _1 _3 in
      _menhir_goto_decl_attr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_262 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_formal_params_opt (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_FUNCTION (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_136 _2 _3 _4 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_linkage_target : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState309 ->
          _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState291 ->
          _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState241 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_292 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEF -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_DEF (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_132 _2 in
          _menhir_goto_linkage_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_linkage_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_031 _1 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_263 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_REF -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_REF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_133 _2 in
          _menhir_goto_linkage_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_250 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_param_mode_opt (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_153 _1 _2 _3 in
      _menhir_goto_param _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState244 ->
          _menhir_run_257 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState255 ->
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_257 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_154 _1 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_param_list : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_param_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState255 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BYVAL ->
              _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BYRES ->
              _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BYREF ->
              _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _ ->
              _menhir_reduce_158 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_157 _1 in
          _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_reduce_158 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_158 () in
      _menhir_goto_param_mode_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_256 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_param_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_param_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_155 _1 _3 in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_207 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc _menhir_cell0_table_dims_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_spec_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState207
      | SEMI ->
          let _v_0 = _menhir_action_047 () in
          _menhir_run_237 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState207 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_206 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_COLON -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COLON (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_207 _2 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_204 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_208 _2 in
      _menhir_goto_type_spec_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_188 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_TYPE (_menhir_stack, _menhir_s) = _menhir_stack in
          let _4 = _v in
          let _v = _menhir_action_027 _2 _4 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_203 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState203 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_205 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COLON (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState205 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_312 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      let _v_0 = _menhir_action_214 () in
      _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState312 _tok
  
  and _menhir_run_313 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_formal_params_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_use_attrs_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATIC ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState315
          | END ->
              let _v_0 = _menhir_action_162 () in
              _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState315
          | _ ->
              _eRR ())
      | RENT ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer
      | REC ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PARALLEL ->
          _menhir_run_332 _menhir_stack _menhir_lexbuf _menhir_lexer
      | INLINE ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_330 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_210 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_331 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_209 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_332 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_212 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_333 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_213 () in
      _menhir_goto_use_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_261 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formal_params_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState261
      | TYPE ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
      | IDENT _v_1 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState261
      | COLON ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
      | SEMI ->
          let _v_2 = _menhir_action_205 () in
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_258 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_PROC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_135 _2 _3 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_328 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_use_attrs_opt, _menhir_box_compilation_unit) _menhir_cell1_proc_body_opt, _menhir_box_compilation_unit) _menhir_cell1_END as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_167 _1 in
      _menhir_goto_proc_end_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_311 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_top_items_opt, _menhir_box_compilation_unit) _menhir_cell1_DEF, _menhir_box_compilation_unit) _menhir_cell1_PROC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState311
      | INLINE | PARALLEL | REC | RENT | SEMI | STATIC ->
          let _v_0 = _menhir_action_125 () in
          _menhir_run_312 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState311 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_295 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_BLOCK as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          _menhir_run_208 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState295
      | SEMI ->
          let _v_0 = _menhir_action_047 () in
          _menhir_run_296 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState295 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_283 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | STRING _v_0 ->
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState283
      | NULL ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | NOT ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | MINUS ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | LPAREN ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | INT _v_1 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState283
      | IDENT _v_2 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState283
      | FLOAT _v_3 ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState283
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState283
      | EQUAL ->
          let _menhir_stack = MenhirCell1_EQUAL (_menhir_stack, MenhirState283) in
          let _menhir_s = MenhirState285 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | BEAD _v_9 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9 MenhirState283
      | _ ->
          _eRR ()
  
  and _menhir_run_284 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_051 _1 in
          _menhir_goto_define_rhs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AND | EQ | EQUAL | EQV | GE | GQ | GR | GT | LE | LQ | LS | LT | MINUS | MOD | NEQ | NQ | OR | PLUS | SLASH | STAR | XOR ->
          let _1 = _v in
          let _v = _menhir_action_086 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_define_rhs : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState283 ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState285 ->
          _menhir_run_287 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_289 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_DEFINE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_029 _2 _3 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_287 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFINE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_EQUAL -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_EQUAL (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident_loc (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_DEFINE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_030 _2 _4 in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_274 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_list, _menhir_box_compilation_unit) _menhir_cell1_COMMA -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_128 _1 _3 in
      _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState275 ->
          _menhir_run_276 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState269 ->
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_276 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState276
      | TYPE ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | IDENT _v_1 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState276
      | COMMA ->
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | COLON ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState276
      | SEMI | WITH ->
          let _v_2 = _menhir_action_205 () in
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState276 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_273 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_list as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState273 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_271 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LABEL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LABEL (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_032 _2 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_ident_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_273 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState271
      | _ ->
          _eRR ()
  
  and _menhir_run_270 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_127 _1 in
      _menhir_goto_ident_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_267 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_OVERLAY -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_OVERLAY (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_028 _2 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_265 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_134 _1 in
      _menhir_goto_linkage_target _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_260 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FUNCTION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | COLON | IDENT _ | SEMI | TYPE | TYPEATOM _ ->
          let _v_0 = _menhir_action_125 () in
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState260 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_249 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_param_mode_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState249
      | TYPE ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState249
      | IDENT _v_1 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState249
      | COLON ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState249
      | COMMA | RPAREN ->
          let _v_2 = _menhir_action_205 () in
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_243 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          _menhir_run_244 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState243
      | SEMI ->
          let _v_0 = _menhir_action_125 () in
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_227 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_INSTANCE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_INSTANCE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_040 _2 in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_225 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LIKE -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_LIKE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_036 _2 in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_223 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_OVERLAY -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_OVERLAY (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_039 _2 in
      _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_196 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_055 _1 in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dim : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState193 ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_200 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_201 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_goto_dim_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_dim_list : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_195 _2 in
          _menhir_goto_table_dims_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_dim_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState199 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_table_dims_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_table_dims_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | TYPEATOM _v_0 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState202
      | TYPE ->
          _menhir_run_203 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
      | IDENT _v_1 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState202
      | COLON ->
          _menhir_run_205 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState202
      | SEMI | WITH ->
          let _v_2 = _menhir_action_205 () in
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState202 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_194 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_053 () in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_195 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_054 _1 in
      _menhir_goto_dim _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_200 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_dim_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_dim_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_057 _1 _3 in
      _menhir_goto_dim_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_192 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState193 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLON | IDENT _ | SEMI | TYPE | TYPEATOM _ | WITH ->
          let _v = _menhir_action_194 () in
          _menhir_goto_table_dims_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_190 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_204 _1 in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_183 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_171 _1 in
      _menhir_goto_status_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_status_item : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState173 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_184 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_173 _1 in
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
              let _v = _menhir_action_026 _2 _5 in
              _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | COMMA ->
          let _menhir_stack = MenhirCell1_status_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState181 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEATOM _v ->
              _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_174 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_TYPEATOM (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState175 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_182 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_status_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_status_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_174 _1 _3 in
      _menhir_goto_status_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_176 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPEATOM -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_TYPEATOM (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_172 _3 in
          _menhir_goto_status_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_171 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TYPE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STATUS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState173 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEATOM _v ->
                  _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | EQUAL ->
          let _menhir_s = MenhirState185 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPEATOM _v ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
              | STRING _v_0 ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState128
              | NULL ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
              | NOT ->
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
              | MINUS ->
                  _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
              | LPAREN ->
                  _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
              | INT _v_1 ->
                  _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState128
              | IDENT _v_2 ->
                  _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState128
              | FLOAT _v_3 ->
                  _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState128
              | FALSE ->
                  _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
              | BEAD _v_4 ->
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState128
              | RPAREN ->
                  let _v_5 = _menhir_action_119 () in
                  _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | LPAREN ->
          let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
          | STRING _v_6 ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState131
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
          | INT _v_7 ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState131
          | IDENT _v_8 ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState131
          | FLOAT _v_9 ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v_9 MenhirState131
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
          | BEAD _v_10 ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_10 MenhirState131
          | RPAREN ->
              let _v_11 = _menhir_action_119 () in
              _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v_11
          | _ ->
              _eRR ())
      | COLON ->
          let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState134 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SEMI ->
          let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
          let _v = _menhir_action_010 () in
          _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQUAL ->
          let _1 = _v in
          let _v = _menhir_action_137 _1 in
          _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _3 = _v in
      let _v = _menhir_action_012 _3 in
      _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_call_args_opt : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_013 _1 _2 in
          let _1 = _v in
          let _v = _menhir_action_177 _1 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_132 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _2 = _v in
          let _v = _menhir_action_011 _2 in
          _menhir_goto_call_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQUAL ->
          let MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_138 _1 _3 in
          _menhir_goto_lvalue _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_lvalue : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_lvalue (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState122 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_EXIT -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_082 _1 in
      _menhir_goto_exit_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_087 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState088 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_084 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_GOTO -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_GOTO (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_182 _2 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | STRING _v_0 ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState018
          | NULL ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | NOT ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | MINUS ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | LPAREN ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | INT _v_1 ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState018
          | IDENT _v_2 ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState018
          | FLOAT _v_3 ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState018
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
          | BEAD _v_4 ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState018
          | RPAREN ->
              let _v_5 = _menhir_action_119 () in
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5
          | _ ->
              _eRR ())
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | MOD | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | SLASH | STAR | STOP | THEN | TO | WHILE | XOR ->
          let _1 = _v in
          let _v = _menhir_action_091 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident_loc -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_ident_loc (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_092 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState026 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SLASH (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState028 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState030 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_062 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState062 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState034 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NEQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState038 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MOD (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState032 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState036 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState040 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState042 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState044 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_046 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState046 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_048 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState048 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState050 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState052 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState054 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQV (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState064 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQUAL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState056 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState058 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_060 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AND (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState060 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_230 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_DEFAULT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState230
      | COMMA | RPAREN ->
          let MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_035 _2 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_219 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_POS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_POS (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_037 _3 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState219
      | _ ->
          _eRR ()
  
  and _menhir_run_213 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_REP as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_REP (_menhir_stack, _menhir_s) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_038 _3 in
          _menhir_goto_decl_attr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState213
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack. (((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_elsif_list, _menhir_box_compilation_unit) _menhir_cell1_ELSIF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState164) in
          let _menhir_s = MenhirState165 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_THEN, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_cell1_ELSIF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState157) in
          let _menhir_s = MenhirState158 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState157
      | _ ->
          _eRR ()
  
  and _menhir_run_153 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_by_opt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | SEMI ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | IDENT _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState153
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState153
      | _ ->
          _eRR ()
  
  and _menhir_run_123 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_lvalue as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_lvalue (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_176 _1 _3 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN, _menhir_box_compilation_unit) _menhir_cell1_case_labels as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | COLON | COMMA ->
          let MenhirCell1_case_labels (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_018 _1 _3 in
          _menhir_goto_case_labels _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_case_labels : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_case_labels (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_s = MenhirState107 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_190 () in
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState109 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_105 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHEN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState105
      | COLON | COMMA ->
          let _1 = _v in
          let _v = _menhir_action_017 _1 in
          _menhir_goto_case_labels _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_CASE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | OF ->
          let _menhir_stack = MenhirCell1_OF (_menhir_stack, MenhirState102) in
          let _menhir_s = MenhirState103 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHEN ->
              _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | _ ->
          _eRR ()
  
  and _menhir_run_093 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_BY as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | ABORT | BEGIN | CASE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | RETURN | SEMI | STOP | WHILE ->
          let MenhirCell1_BY (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_009 _2 in
          _menhir_goto_by_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_by_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState089 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_by_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_s = MenhirState152 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack. ((((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_by_opt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | SEMI ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | IDENT _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState094
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_091 : type  ttv_stack. (((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_TO as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | BY ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | ABORT | BEGIN | CASE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | RETURN | SEMI | STOP | WHILE ->
          let _v_0 = _menhir_action_008 () in
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState091 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BY (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState092 in
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
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BEAD _v ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_FOR, _menhir_box_compilation_unit) _menhir_cell1_ident_loc as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | TO ->
          let _menhir_stack = MenhirCell1_TO (_menhir_stack, MenhirState089) in
          let _menhir_s = MenhirState090 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | BY ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | WHILE ->
          let _v_5 = _menhir_action_008 () in
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState089 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_081 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | THEN ->
          let _menhir_stack = MenhirCell1_THEN (_menhir_stack, MenhirState081) in
          let _menhir_s = MenhirState082 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STOP ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEMI ->
              _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RETURN ->
              _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IDENT _v ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | GOTO ->
              _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FOR ->
              _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALLTHRU ->
              _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | EXIT ->
              _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CASE ->
              _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEGIN ->
              _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ABORT ->
              _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState075
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_122 _1 in
          _menhir_goto_expr_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_071 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | XOR ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | WHILE ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | STOP ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | STAR ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | SLASH ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | SEMI ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | RETURN ->
          _menhir_run_077 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | PLUS ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | OR ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | NQ ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | NEQ ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | MOD ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | MINUS ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LT ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LS ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LQ ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | LE ->
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | IF ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | IDENT _v_0 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState071
      | GT ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | GR ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | GQ ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | GOTO ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | GE ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | FOR ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | FALLTHRU ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | EXIT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | EQV ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | EQUAL ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | EQ ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | CASE ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | BEGIN ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | AND ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | ABORT ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_NOT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_095 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_094 _2 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_067 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_093 _2 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState066
      | COMMA | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_117 _1 in
          _menhir_goto_expr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_expr_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState022 in
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
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_120 _1 in
          _menhir_goto_expr_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_expr_list_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState131 ->
          _menhir_run_132 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState128 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState018 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_065 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQV as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState065
      | ABORT | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_EQV (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_116 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_OR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | ABORT | BEGIN | BY | CASE | COLON | COMMA | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE ->
          let MenhirCell1_OR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_114 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_AND as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_AND (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_113 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState059
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_107 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_EQUAL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState057
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState057
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState057
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState057
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState057
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_EQUAL (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_106 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState055
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState055
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState055
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState055
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState055
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_104 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState053
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_112 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_111 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_049 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_GT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState049
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_GT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_103 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState047
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LE (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_102 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState045
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_110 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState043
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_109 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_LT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_LT (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_101 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NEQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState039
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_NEQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_105 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState037
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_100 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_NQ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState035
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_NQ (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_108 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_MOD -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MOD (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_098 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_031 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_PLUS as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState031
      | ABORT | AND | BEGIN | BY | CASE | COLON | COMMA | EQ | EQUAL | EQV | EXIT | FALLTHRU | FOR | GE | GOTO | GQ | GR | GT | IDENT _ | IF | LE | LQ | LS | LT | MINUS | NEQ | NQ | OF | OR | PLUS | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_PLUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_099 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_SLASH -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SLASH (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_097 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_027 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_STAR -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_STAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_096 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_025 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_XOR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | ABORT | BEGIN | BY | CASE | COLON | COMMA | EQV | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | OF | OR | RETURN | RPAREN | SEMI | STOP | THEN | TO | WHILE | XOR ->
          let MenhirCell1_XOR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_115 _1 _3 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | STAR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | SLASH ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | OR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | NQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | NEQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | MOD ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | MINUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | LT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | LS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | LQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | LE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_046 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | GT ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | GR ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | GQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | GE ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | EQV ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | EQUAL ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | EQ ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | AND ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState023
      | COMMA | RPAREN ->
          let MenhirCell1_expr_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_118 _1 _3 in
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
                  let _v = _menhir_action_022 _1 _3 in
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
          let _v = _menhir_action_142 _1 _2 _3 _4 _5 in
          let _menhir_stack = MenhirCell0_module_header (_menhir_stack, _v) in
          (match (_tok : MenhirBasics.token) with
          | BEGIN ->
              let _menhir_stack = MenhirCell1_BEGIN (_menhir_stack, MenhirState002) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_200 () in
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003 _tok
          | ABORT | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | ITEM | LABEL | OVERLAY | REF | RETURN | SEMI | STOP | TABLE | TERM | TYPE | WHILE ->
              let _v_1 = _menhir_action_200 () in
              _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState002 _tok
          | _ ->
              _menhir_fail ())
      | _ ->
          _eRR ()
  
  let _menhir_goto_module_name_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt _menhir_cell0_module_kind_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_module_name_opt (_menhir_stack, _v) in
      let _v_0 = _menhir_action_214 () in
      _menhir_run_381 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState380 _tok
  
  let _menhir_goto_module_kind_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_module_kind_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_1, _endpos__1_, _startpos__1_) = (_v_0, _endpos, _startpos) in
          let _v = _menhir_action_149 _1 _endpos__1_ _startpos__1_ in
          _menhir_goto_module_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | ICOMPOOL | IF | INLINE | ITEM | LABEL | OVERLAY | PARALLEL | REC | REF | RENT | RETURN | SEMI | STATIC | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_150 () in
          _menhir_goto_module_name_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_goto_directives_opt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_directives_opt (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PROGRAM ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_143 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | PROC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_146 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | ICOMPOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_145 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FUNCTION ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_147 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMPOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_144 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | DIRECTIVE_NAME _v ->
              let _menhir_stack = MenhirCell0_DIRECTIVE_NAME (_menhir_stack, _v) in
              let _menhir_s = MenhirState357 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TRUE ->
                  _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | STRING _v ->
                  _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | NULL ->
                  _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
                  let _menhir_s = MenhirState361 in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | TRUE ->
                      _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | STRING _v ->
                      _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | NULL ->
                      _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | INT _v ->
                      _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | IDENT _v ->
                      _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FLOAT _v ->
                      _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | FALSE ->
                      _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
                  | BEAD _v ->
                      _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
                  | RPAREN ->
                      let _v = _menhir_action_069 () in
                      _menhir_goto_directive_arg_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
                  | _ ->
                      _eRR ())
              | INT _v ->
                  _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | IDENT _v ->
                  _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FLOAT _v ->
                  _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FALSE ->
                  _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | BEAD _v ->
                  _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | SEMI ->
                  let _v = _menhir_action_073 () in
                  _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ABORT | BEGIN | BLOCK | CASE | DEF | DEFINE | EXIT | FALLTHRU | FOR | GOTO | IDENT _ | IF | INLINE | ITEM | LABEL | OVERLAY | PARALLEL | REC | REF | RENT | RETURN | SEMI | STATIC | STOP | TABLE | TERM | TYPE | WHILE ->
          let _v = _menhir_action_148 () in
          _menhir_goto_module_kind_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_358 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_064 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_arg : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState357 ->
          _menhir_run_377 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState375 ->
          _menhir_run_376 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState361 ->
          _menhir_run_372 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState370 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_377 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_071 _1 in
      _menhir_goto_directive_args _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_args : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | STRING _v_0 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState375
      | NULL ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState375
      | IDENT _v_2 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState375
      | FLOAT _v_3 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState375
      | FALSE ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState375
      | BEAD _v_4 ->
          let _menhir_stack = MenhirCell1_directive_args (_menhir_stack, _menhir_s, _v) in
          _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState375
      | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_359 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_059 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_360 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_063 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_362 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_060 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_363 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_066 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_364 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_061 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_365 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_065 () in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_366 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_062 _1 in
      _menhir_goto_directive_arg _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_args_opt : type  ttv_stack. ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_DIRECTIVE_NAME (_menhir_stack, _2) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_058 _2 _3 in
          let MenhirCell0_directives_opt (_menhir_stack, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_077 _1 _2 in
          _menhir_goto_directives_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_376 : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_directive_args -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_directive_args (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_072 _1 _2 in
      _menhir_goto_directive_args _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_372 : type  ttv_stack. ((ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_067 _1 in
      _menhir_goto_directive_arg_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_directive_arg_list : type  ttv_stack. ((ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_directive_arg_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState370 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | NULL ->
              _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_363 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BEAD _v ->
              _menhir_run_366 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_070 _1 in
          _menhir_goto_directive_arg_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_directive_arg_list_opt : type  ttv_stack. (ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LPAREN (_menhir_stack, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_075 _2 in
      _menhir_goto_directive_args_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_371 : type  ttv_stack. ((ttv_stack _menhir_cell0_directives_opt _menhir_cell0_DIRECTIVE_NAME, _menhir_box_compilation_unit) _menhir_cell1_LPAREN, _menhir_box_compilation_unit) _menhir_cell1_directive_arg_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_directive_arg_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_068 _1 _3 in
      _menhir_goto_directive_arg_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | START ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_076 () in
          _menhir_goto_directives_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
end

let compilation_unit =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_compilation_unit v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
