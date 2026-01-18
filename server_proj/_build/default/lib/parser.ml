
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | THEN
    | TABLE
    | STRING of (
# 38 "lib/parser.mly"
       (string)
# 18 "lib/parser.ml"
  )
    | STAR
    | SLASH
    | SEMI
    | RPAR
    | RETURN
    | REAL of (
# 37 "lib/parser.mly"
       (float)
# 28 "lib/parser.ml"
  )
    | PROC
    | PLUS
    | NE
    | MINUS
    | LT
    | LPAR
    | LE
    | ITEM
    | INT of (
# 36 "lib/parser.mly"
       (int)
# 41 "lib/parser.ml"
  )
    | IF
    | IDENT of (
# 35 "lib/parser.mly"
       (string)
# 47 "lib/parser.ml"
  )
    | GT
    | GE
    | EQ
    | EOF
    | END
    | ELSE
    | DO
    | COMMA
    | COLON
    | CHAR of (
# 39 "lib/parser.mly"
       (char)
# 61 "lib/parser.ml"
  )
    | BOOL of (
# 40 "lib/parser.mly"
       (bool)
# 66 "lib/parser.ml"
  )
    | ASSIGN
  
end

include MenhirBasics

# 3 "lib/parser.mly"
  
  (* Menhir parser for a “useful subset” of JOVIAL-like syntax.
     Focus: declarations + enough statements/expr to power symbols/diagnostics. *)

  let loc (sp : Lexing.position) (ep : Lexing.position) (v : 'a) : 'a Ast.located =
    Ast.located ~startp:sp ~endp:ep v

  let loc_expr sp ep (v : Ast.expr) : Ast.expr Ast.located = loc sp ep v
  let loc_stmt sp ep (v : Ast.stmt) : Ast.stmt Ast.located = loc sp ep v
  let loc_decl sp ep (v : Ast.decl) : Ast.decl Ast.located = loc sp ep v

  let mk_ident (sp : Lexing.position) (ep : Lexing.position) (s : string) : Ast.ident =
    Ast.ident ~startp:sp ~endp:ep s

  let scalar_of_name (s : string) : Ast.scalar_type option =
    match String.uppercase_ascii s with
    | "INT"    -> Some Ast.TyInt
    | "REAL"   -> Some Ast.TyReal
    | "BOOL"   -> Some Ast.TyBool
    | "CHAR"   -> Some Ast.TyChar
    | "STRING" -> Some Ast.TyString
    | _        -> None

# 98 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_compilation_unit) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: compilation_unit. *)

  | MenhirState001 : (('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_state
    (** State 001.
        Stack shape : TABLE.
        Start symbol: compilation_unit. *)

  | MenhirState003 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_state
    (** State 003.
        Stack shape : TABLE ident.
        Start symbol: compilation_unit. *)

  | MenhirState013 : (('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_state
    (** State 013.
        Stack shape : PROC.
        Start symbol: compilation_unit. *)

  | MenhirState015 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_state
    (** State 015.
        Stack shape : PROC ident LPAR.
        Start symbol: compilation_unit. *)

  | MenhirState017 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_cell1_param_list_opt _menhir_cell0_RPAR, _menhir_box_compilation_unit) _menhir_state
    (** State 017.
        Stack shape : PROC ident LPAR param_list_opt RPAR.
        Start symbol: compilation_unit. *)

  | MenhirState018 : ((((('s, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_cell1_param_list_opt _menhir_cell0_RPAR, _menhir_box_compilation_unit) _menhir_cell1_opt_type, _menhir_box_compilation_unit) _menhir_state
    (** State 018.
        Stack shape : PROC ident LPAR param_list_opt RPAR opt_type.
        Start symbol: compilation_unit. *)

  | MenhirState019 : (('s, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_state
    (** State 019.
        Stack shape : WHILE.
        Start symbol: compilation_unit. *)

  | MenhirState022 : (('s, _menhir_box_compilation_unit) _menhir_cell1_MINUS, _menhir_box_compilation_unit) _menhir_state
    (** State 022.
        Stack shape : MINUS.
        Start symbol: compilation_unit. *)

  | MenhirState023 : (('s, _menhir_box_compilation_unit) _menhir_cell1_LPAR, _menhir_box_compilation_unit) _menhir_state
    (** State 023.
        Stack shape : LPAR.
        Start symbol: compilation_unit. *)

  | MenhirState030 : (('s, _menhir_box_compilation_unit) _menhir_cell1_mul, _menhir_box_compilation_unit) _menhir_state
    (** State 030.
        Stack shape : mul.
        Start symbol: compilation_unit. *)

  | MenhirState033 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_state
    (** State 033.
        Stack shape : ident LPAR.
        Start symbol: compilation_unit. *)

  | MenhirState035 : (('s, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 035.
        Stack shape : expr.
        Start symbol: compilation_unit. *)

  | MenhirState039 : (('s, _menhir_box_compilation_unit) _menhir_cell1_add, _menhir_box_compilation_unit) _menhir_state
    (** State 039.
        Stack shape : add.
        Start symbol: compilation_unit. *)

  | MenhirState041 : (('s, _menhir_box_compilation_unit) _menhir_cell1_mul, _menhir_box_compilation_unit) _menhir_state
    (** State 041.
        Stack shape : mul.
        Start symbol: compilation_unit. *)

  | MenhirState044 : (('s, _menhir_box_compilation_unit) _menhir_cell1_add _menhir_cell0_MINUS, _menhir_box_compilation_unit) _menhir_state
    (** State 044.
        Stack shape : add MINUS.
        Start symbol: compilation_unit. *)

  | MenhirState051 : (('s, _menhir_box_compilation_unit) _menhir_cell1_add _menhir_cell0_cmpop, _menhir_box_compilation_unit) _menhir_state
    (** State 051.
        Stack shape : add cmpop.
        Start symbol: compilation_unit. *)

  | MenhirState060 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 060.
        Stack shape : WHILE expr.
        Start symbol: compilation_unit. *)

  | MenhirState061 : (('s, _menhir_box_compilation_unit) _menhir_cell1_RETURN, _menhir_box_compilation_unit) _menhir_state
    (** State 061.
        Stack shape : RETURN.
        Start symbol: compilation_unit. *)

  | MenhirState065 : (('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_state
    (** State 065.
        Stack shape : IF.
        Start symbol: compilation_unit. *)

  | MenhirState067 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_state
    (** State 067.
        Stack shape : IF expr.
        Start symbol: compilation_unit. *)

  | MenhirState070 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_stmt_list, _menhir_box_compilation_unit) _menhir_state
    (** State 070.
        Stack shape : IF expr stmt_list.
        Start symbol: compilation_unit. *)

  | MenhirState072 : (('s, _menhir_box_compilation_unit) _menhir_cell1_stmt, _menhir_box_compilation_unit) _menhir_state
    (** State 072.
        Stack shape : stmt.
        Start symbol: compilation_unit. *)

  | MenhirState077 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_state
    (** State 077.
        Stack shape : ident LPAR.
        Start symbol: compilation_unit. *)

  | MenhirState081 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_state
    (** State 081.
        Stack shape : ident.
        Start symbol: compilation_unit. *)

  | MenhirState097 : (('s, _menhir_box_compilation_unit) _menhir_cell1_param, _menhir_box_compilation_unit) _menhir_state
    (** State 097.
        Stack shape : param.
        Start symbol: compilation_unit. *)

  | MenhirState099 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_state
    (** State 099.
        Stack shape : ident.
        Start symbol: compilation_unit. *)

  | MenhirState101 : (('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_state
    (** State 101.
        Stack shape : ITEM.
        Start symbol: compilation_unit. *)

  | MenhirState102 : ((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_state
    (** State 102.
        Stack shape : ITEM ident.
        Start symbol: compilation_unit. *)

  | MenhirState104 : (((('s, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_cell1_opt_type, _menhir_box_compilation_unit) _menhir_state
    (** State 104.
        Stack shape : ITEM ident opt_type.
        Start symbol: compilation_unit. *)

  | MenhirState113 : (('s, _menhir_box_compilation_unit) _menhir_cell1_decl, _menhir_box_compilation_unit) _menhir_state
    (** State 113.
        Stack shape : decl.
        Start symbol: compilation_unit. *)


and ('s, 'r) _menhir_cell1_add = 
  | MenhirCell1_add of 's * ('s, 'r) _menhir_state * (Ast.expr Ast.located) * Lexing.position * Lexing.position

and 's _menhir_cell0_cmpop = 
  | MenhirCell0_cmpop of 's * (Ast.binop)

and ('s, 'r) _menhir_cell1_decl = 
  | MenhirCell1_decl of 's * ('s, 'r) _menhir_state * (Ast.decl Ast.located)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr Ast.located)

and ('s, 'r) _menhir_cell1_ident = 
  | MenhirCell1_ident of 's * ('s, 'r) _menhir_state * (Ast.ident) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_mul = 
  | MenhirCell1_mul of 's * ('s, 'r) _menhir_state * (Ast.expr Ast.located) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_opt_type = 
  | MenhirCell1_opt_type of 's * ('s, 'r) _menhir_state * (Ast.type_expr Ast.located option)

and ('s, 'r) _menhir_cell1_param = 
  | MenhirCell1_param of 's * ('s, 'r) _menhir_state * (Ast.ident * Ast.type_expr Ast.located option)

and ('s, 'r) _menhir_cell1_param_list_opt = 
  | MenhirCell1_param_list_opt of 's * ('s, 'r) _menhir_state * ((Ast.ident * Ast.type_expr Ast.located option) list)

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (Ast.stmt Ast.located)

and ('s, 'r) _menhir_cell1_stmt_list = 
  | MenhirCell1_stmt_list of 's * ('s, 'r) _menhir_state * (Ast.stmt Ast.located list)

and ('s, 'r) _menhir_cell1_COLON = 
  | MenhirCell1_COLON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_ITEM = 
  | MenhirCell1_ITEM of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_LPAR = 
  | MenhirCell0_LPAR of 's * Lexing.position

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_MINUS = 
  | MenhirCell0_MINUS of 's * Lexing.position

and ('s, 'r) _menhir_cell1_PROC = 
  | MenhirCell1_PROC of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state * Lexing.position

and 's _menhir_cell0_RPAR = 
  | MenhirCell0_RPAR of 's * Lexing.position

and ('s, 'r) _menhir_cell1_TABLE = 
  | MenhirCell1_TABLE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state * Lexing.position

and _menhir_box_compilation_unit = 
  | MenhirBox_compilation_unit of (Ast.compilation_unit) [@@unboxed]

let _menhir_action_01 =
  fun _endpos_b_ _startpos_a_ a b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos_a_ in
    (
# 246 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Binary (Ast.Add, a, b))
    )
# 339 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_02 =
  fun _endpos_b_ _startpos_a_ a b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos_a_ in
    (
# 251 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Binary (Ast.Sub, a, b))
    )
# 352 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_03 =
  fun m ->
    (
# 255 "lib/parser.mly"
                                 ( m )
# 360 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_04 =
  fun e ->
    (
# 332 "lib/parser.mly"
                                 ( [e] )
# 368 "lib/parser.ml"
     : (Ast.expr Ast.located list))

let _menhir_action_05 =
  fun e es ->
    (
# 333 "lib/parser.mly"
                                 ( e :: es )
# 376 "lib/parser.ml"
     : (Ast.expr Ast.located list))

let _menhir_action_06 =
  fun () ->
    (
# 327 "lib/parser.mly"
                                 ( [] )
# 384 "lib/parser.ml"
     : (Ast.expr Ast.located list))

let _menhir_action_07 =
  fun es ->
    (
# 328 "lib/parser.mly"
                                 ( es )
# 392 "lib/parser.ml"
     : (Ast.expr Ast.located list))

let _menhir_action_08 =
  fun _endpos__4_ _startpos_id_ e id ->
    let _endpos = _endpos__4_ in
    let _startpos = _startpos_id_ in
    (
# 170 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_stmt sp ep (Ast.Assign (id, e))
    )
# 405 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_09 =
  fun _endpos__5_ _startpos_f_ args f ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos_f_ in
    (
# 178 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_stmt sp ep (Ast.CallStmt (f, args))
    )
# 418 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_10 =
  fun _endpos_b_ _startpos_a_ a b op ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos_a_ in
    (
# 228 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Binary (op, a, b))
    )
# 431 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_11 =
  fun a ->
    (
# 232 "lib/parser.mly"
                                 ( a )
# 439 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_12 =
  fun () ->
    (
# 236 "lib/parser.mly"
                                 ( Ast.Eq )
# 447 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_13 =
  fun () ->
    (
# 237 "lib/parser.mly"
                                 ( Ast.Ne )
# 455 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_14 =
  fun () ->
    (
# 238 "lib/parser.mly"
                                 ( Ast.Lt )
# 463 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_15 =
  fun () ->
    (
# 239 "lib/parser.mly"
                                 ( Ast.Le )
# 471 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_16 =
  fun () ->
    (
# 240 "lib/parser.mly"
                                 ( Ast.Gt )
# 479 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_17 =
  fun () ->
    (
# 241 "lib/parser.mly"
                                 ( Ast.Ge )
# 487 "lib/parser.ml"
     : (Ast.binop))

let _menhir_action_18 =
  fun ds ->
    (
# 63 "lib/parser.mly"
  ( { Ast.decls = ds } )
# 495 "lib/parser.ml"
     : (Ast.compilation_unit))

let _menhir_action_19 =
  fun d ->
    (
# 72 "lib/parser.mly"
                                ( d )
# 503 "lib/parser.ml"
     : (Ast.decl Ast.located))

let _menhir_action_20 =
  fun d ->
    (
# 73 "lib/parser.mly"
                                ( d )
# 511 "lib/parser.ml"
     : (Ast.decl Ast.located))

let _menhir_action_21 =
  fun d ->
    (
# 74 "lib/parser.mly"
                                ( d )
# 519 "lib/parser.ml"
     : (Ast.decl Ast.located))

let _menhir_action_22 =
  fun () ->
    (
# 67 "lib/parser.mly"
                                ( [] )
# 527 "lib/parser.ml"
     : (Ast.decl Ast.located list))

let _menhir_action_23 =
  fun d ds ->
    (
# 68 "lib/parser.mly"
                                ( d :: ds )
# 535 "lib/parser.ml"
     : (Ast.decl Ast.located list))

let _menhir_action_24 =
  fun () ->
    (
# 193 "lib/parser.mly"
                                 ( [] )
# 543 "lib/parser.ml"
     : (Ast.stmt Ast.located list))

let _menhir_action_25 =
  fun s ->
    (
# 194 "lib/parser.mly"
                                 ( s )
# 551 "lib/parser.ml"
     : (Ast.stmt Ast.located list))

let _menhir_action_26 =
  fun c ->
    (
# 223 "lib/parser.mly"
                                 ( c )
# 559 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_27 =
  fun _endpos_s_ _startpos_s_ s ->
    (
# 342 "lib/parser.mly"
    (
      let sp = _startpos_s_ and ep = _endpos_s_ in
      mk_ident sp ep s
    )
# 570 "lib/parser.ml"
     : (Ast.ident))

let _menhir_action_28 =
  fun _endpos__7_ _startpos__1_ c e t ->
    let _endpos = _endpos__7_ in
    let _startpos = _startpos__1_ in
    (
# 186 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_stmt sp ep (Ast.If (c, t, e))
    )
# 583 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_29 =
  fun _endpos__5_ _startpos__1_ init name ty ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    (
# 79 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      let it : Ast.item_decl = { Ast.name = name; ty; init } in
      loc_decl sp ep (Ast.Item it)
    )
# 597 "lib/parser.ml"
     : (Ast.decl Ast.located))

let _menhir_action_30 =
  fun _endpos_b_ _startpos_a_ a b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos_a_ in
    (
# 260 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Binary (Ast.Mul, a, b))
    )
# 610 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_31 =
  fun _endpos_b_ _startpos_a_ a b ->
    let _endpos = _endpos_b_ in
    let _startpos = _startpos_a_ in
    (
# 265 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Binary (Ast.Div, a, b))
    )
# 623 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_32 =
  fun u ->
    (
# 269 "lib/parser.mly"
                                 ( u )
# 631 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_33 =
  fun () ->
    (
# 147 "lib/parser.mly"
                                 ( None )
# 639 "lib/parser.ml"
     : (Ast.expr Ast.located option))

let _menhir_action_34 =
  fun e ->
    (
# 148 "lib/parser.mly"
                                 ( Some e )
# 647 "lib/parser.ml"
     : (Ast.expr Ast.located option))

let _menhir_action_35 =
  fun () ->
    (
# 96 "lib/parser.mly"
                                ( None )
# 655 "lib/parser.ml"
     : (int option))

let _menhir_action_36 =
  fun n ->
    (
# 97 "lib/parser.mly"
                                ( Some n )
# 663 "lib/parser.ml"
     : (int option))

let _menhir_action_37 =
  fun () ->
    (
# 130 "lib/parser.mly"
                                 ( None )
# 671 "lib/parser.ml"
     : (Ast.type_expr Ast.located option))

let _menhir_action_38 =
  fun t ->
    (
# 131 "lib/parser.mly"
                                 ( Some t )
# 679 "lib/parser.ml"
     : (Ast.type_expr Ast.located option))

let _menhir_action_39 =
  fun id ty ->
    (
# 126 "lib/parser.mly"
                                  ( (id, ty) )
# 687 "lib/parser.ml"
     : (Ast.ident * Ast.type_expr Ast.located option))

let _menhir_action_40 =
  fun p ->
    (
# 121 "lib/parser.mly"
                                 ( [p] )
# 695 "lib/parser.ml"
     : ((Ast.ident * Ast.type_expr Ast.located option) list))

let _menhir_action_41 =
  fun p ps ->
    (
# 122 "lib/parser.mly"
                                  ( p :: ps )
# 703 "lib/parser.ml"
     : ((Ast.ident * Ast.type_expr Ast.located option) list))

let _menhir_action_42 =
  fun () ->
    (
# 116 "lib/parser.mly"
                                ( [] )
# 711 "lib/parser.ml"
     : ((Ast.ident * Ast.type_expr Ast.located option) list))

let _menhir_action_43 =
  fun ps ->
    (
# 117 "lib/parser.mly"
                                ( ps )
# 719 "lib/parser.ml"
     : ((Ast.ident * Ast.type_expr Ast.located option) list))

let _menhir_action_44 =
  fun _endpos_i_ _startpos_i_ i ->
    (
# 283 "lib/parser.mly"
    (
      let sp = _startpos_i_ and ep = _endpos_i_ in
      loc_expr sp ep (Ast.IntLit i)
    )
# 730 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_45 =
  fun _endpos_r_ _startpos_r_ r ->
    (
# 288 "lib/parser.mly"
    (
      let sp = _startpos_r_ and ep = _endpos_r_ in
      loc_expr sp ep (Ast.RealLit r)
    )
# 741 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_46 =
  fun _endpos_s_ _startpos_s_ s ->
    (
# 293 "lib/parser.mly"
    (
      let sp = _startpos_s_ and ep = _endpos_s_ in
      loc_expr sp ep (Ast.StringLit s)
    )
# 752 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_47 =
  fun _endpos_c_ _startpos_c_ c ->
    (
# 298 "lib/parser.mly"
    (
      let sp = _startpos_c_ and ep = _endpos_c_ in
      loc_expr sp ep (Ast.CharLit c)
    )
# 763 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_48 =
  fun _endpos_b_ _startpos_b_ b ->
    (
# 303 "lib/parser.mly"
    (
      let sp = _startpos_b_ and ep = _endpos_b_ in
      loc_expr sp ep (Ast.BoolLit b)
    )
# 774 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_49 =
  fun _endpos__4_ _startpos_f_ args f ->
    let _endpos = _endpos__4_ in
    let _startpos = _startpos_f_ in
    (
# 309 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Call (f, args))
    )
# 787 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_50 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 314 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Var id)
    )
# 800 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_51 =
  fun _endpos__3_ _startpos__1_ e ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 320 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.loc_value e)
    )
# 813 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_52 =
  fun _endpos__9_ _startpos__1_ body name ps ret ->
    let _endpos = _endpos__9_ in
    let _startpos = _startpos__1_ in
    (
# 102 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      let pr : Ast.proc_decl =
        { Ast.name = name
        ; params = ps
        ; returns = ret
        ; body
        }
      in
      loc_decl sp ep (Ast.Proc pr)
    )
# 833 "lib/parser.ml"
     : (Ast.decl Ast.located))

let _menhir_action_53 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos__1_ in
    (
# 207 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_stmt sp ep (Ast.Return None)
    )
# 846 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_54 =
  fun _endpos__3_ _startpos__1_ e ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 212 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_stmt sp ep (Ast.Return (Some e))
    )
# 859 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_55 =
  fun s ->
    (
# 161 "lib/parser.mly"
                                 ( s )
# 867 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_56 =
  fun s ->
    (
# 162 "lib/parser.mly"
                                 ( s )
# 875 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_57 =
  fun s ->
    (
# 163 "lib/parser.mly"
                                 ( s )
# 883 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_58 =
  fun s ->
    (
# 164 "lib/parser.mly"
                                 ( s )
# 891 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_59 =
  fun s ->
    (
# 165 "lib/parser.mly"
                                 ( s )
# 899 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_action_60 =
  fun () ->
    (
# 156 "lib/parser.mly"
                                 ( [] )
# 907 "lib/parser.ml"
     : (Ast.stmt Ast.located list))

let _menhir_action_61 =
  fun s ss ->
    (
# 157 "lib/parser.mly"
                                 ( s :: ss )
# 915 "lib/parser.ml"
     : (Ast.stmt Ast.located list))

let _menhir_action_62 =
  fun _endpos__5_ _startpos__1_ elem name size ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    (
# 88 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      let tb : Ast.table_decl = { Ast.name = name; elem_ty = elem; size } in
      loc_decl sp ep (Ast.Table tb)
    )
# 929 "lib/parser.ml"
     : (Ast.decl Ast.located))

let _menhir_action_63 =
  fun _endpos_id_ _startpos_id_ id ->
    (
# 136 "lib/parser.mly"
    (
      let sp = _startpos_id_ and ep = _endpos_id_ in
      match scalar_of_name id with
      | Some k -> loc sp ep (Ast.Scalar k)
      | None ->
          let nm = mk_ident sp ep id in
          loc sp ep (Ast.Named nm)
    )
# 944 "lib/parser.ml"
     : (Ast.type_expr Ast.located))

let _menhir_action_64 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 274 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_expr sp ep (Ast.Unary (Ast.Neg, e))
    )
# 957 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_65 =
  fun p ->
    (
# 278 "lib/parser.mly"
                                 ( p )
# 965 "lib/parser.ml"
     : (Ast.expr Ast.located))

let _menhir_action_66 =
  fun _endpos__6_ _startpos__1_ body c ->
    let _endpos = _endpos__6_ in
    let _startpos = _startpos__1_ in
    (
# 199 "lib/parser.mly"
    (
      let sp = _startpos and ep = _endpos in
      loc_stmt sp ep (Ast.While (c, body))
    )
# 978 "lib/parser.ml"
     : (Ast.stmt Ast.located))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ASSIGN ->
        "ASSIGN"
    | BOOL _ ->
        "BOOL"
    | CHAR _ ->
        "CHAR"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DO ->
        "DO"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | GE ->
        "GE"
    | GT ->
        "GT"
    | IDENT _ ->
        "IDENT"
    | IF ->
        "IF"
    | INT _ ->
        "INT"
    | ITEM ->
        "ITEM"
    | LE ->
        "LE"
    | LPAR ->
        "LPAR"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | NE ->
        "NE"
    | PLUS ->
        "PLUS"
    | PROC ->
        "PROC"
    | REAL _ ->
        "REAL"
    | RETURN ->
        "RETURN"
    | RPAR ->
        "RPAR"
    | SEMI ->
        "SEMI"
    | SLASH ->
        "SLASH"
    | STAR ->
        "STAR"
    | STRING _ ->
        "STRING"
    | TABLE ->
        "TABLE"
    | THEN ->
        "THEN"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_111 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _v ->
      let ds = _v in
      let _v = _menhir_action_18 ds in
      MenhirBox_compilation_unit _v
  
  let rec _menhir_run_114 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_decl -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _v ->
      let MenhirCell1_decl (_menhir_stack, _menhir_s, d) = _menhir_stack in
      let ds = _v in
      let _v = _menhir_action_23 d ds in
      _menhir_goto_decl_list _menhir_stack _v _menhir_s
  
  and _menhir_goto_decl_list : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState113 ->
          _menhir_run_114 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_111 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TABLE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState001 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_s_, _startpos_s_, s) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_27 _endpos_s_ _startpos_s_ s in
      _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos_s_ _v _menhir_s _tok
  
  and _menhir_goto_ident : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState101 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState015 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState018 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState041 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState001 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_102 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | ASSIGN | SEMI ->
          let _v_0 = _menhir_action_37 () in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState102 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COLON (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_63 _endpos_id_ _startpos_id_ id in
          let MenhirCell1_COLON (_menhir_stack, _menhir_s) = _menhir_stack in
          let t = _v in
          let _v = _menhir_action_38 t in
          _menhir_goto_opt_type _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_opt_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState102 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_103 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_opt_type (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ASSIGN ->
          let _menhir_s = MenhirState104 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REAL _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | SEMI ->
          let _v = _menhir_action_33 () in
          _menhir_goto_opt_init _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_s_, _startpos_s_, s) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_46 _endpos_s_ _startpos_s_ s in
      _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos_s_ _v _menhir_s _tok
  
  and _menhir_goto_primary : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_p_, _startpos_p_, p) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_65 p in
      _menhir_goto_unary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_p_ _startpos_p_ _v _menhir_s _tok
  
  and _menhir_goto_unary : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState022 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState041 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState030 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState104 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState044 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_64 _endpos_e_ _startpos__1_ e in
      _menhir_goto_unary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_042 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_mul -> _ -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_mul (_menhir_stack, _menhir_s, a, _startpos_a_, _) = _menhir_stack in
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_31 _endpos_b_ _startpos_a_ a b in
      _menhir_goto_mul _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_a_ _v _menhir_s _tok
  
  and _menhir_goto_mul : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState044 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState039 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_045 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_add _menhir_cell0_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_mul (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_mul (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | DO | EQ | GE | GT | LE | LT | MINUS | NE | PLUS | RPAR | SEMI | THEN ->
          let MenhirCell0_MINUS (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_add (_menhir_stack, _menhir_s, a, _startpos_a_, _) = _menhir_stack in
          let (_endpos_b_, b) = (_endpos, _v) in
          let _v = _menhir_action_02 _endpos_b_ _startpos_a_ a b in
          _menhir_goto_add _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_a_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_mul -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState030 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_r_, _startpos_r_, r) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_45 _endpos_r_ _startpos_r_ r in
      _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_r_ _startpos_r_ _v _menhir_s _tok
  
  and _menhir_run_022 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState022 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState023 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_024 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_i_, _startpos_i_, i) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_44 _endpos_i_ _startpos_i_ i in
      _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_i_ _startpos_i_ _v _menhir_s _tok
  
  and _menhir_run_025 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_c_, _startpos_c_, c) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_47 _endpos_c_ _startpos_c_ c in
      _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_c_ _startpos_c_ _v _menhir_s _tok
  
  and _menhir_run_026 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_b_, _startpos_b_, b) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_48 _endpos_b_ _startpos_b_ b in
      _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_b_ _v _menhir_s _tok
  
  and _menhir_run_041 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_mul -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState041 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_add : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_052 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_add _menhir_cell0_cmpop as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | DO | RPAR | SEMI | THEN ->
          let MenhirCell0_cmpop (_menhir_stack, op) = _menhir_stack in
          let MenhirCell1_add (_menhir_stack, _menhir_s, a, _startpos_a_, _) = _menhir_stack in
          let (_endpos_b_, b) = (_endpos, _v) in
          let _v = _menhir_action_10 _endpos_b_ _startpos_a_ a b op in
          _menhir_goto_cmp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_add -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState039 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_add -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell0_MINUS (_menhir_stack, _startpos) in
      let _menhir_s = MenhirState044 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_cmp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let c = _v in
      let _v = _menhir_action_26 c in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState065 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState019 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState023 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState077 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState035 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_105 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_cell1_opt_type -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let e = _v in
      let _v = _menhir_action_34 e in
      _menhir_goto_opt_init _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_opt_init : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ITEM, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_cell1_opt_type -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_opt_type (_menhir_stack, _, ty) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _, name, _, _) = _menhir_stack in
          let MenhirCell1_ITEM (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__5_, init) = (_endpos, _v) in
          let _v = _menhir_action_29 _endpos__5_ _startpos__1_ init name ty in
          let d = _v in
          let _v = _menhir_action_19 d in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TABLE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | PROC ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | ITEM ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | EOF ->
          let _v_0 = _menhir_action_22 () in
          _menhir_run_114 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_PROC (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState013 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ITEM (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState101 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_ident (_menhir_stack, _menhir_s, id, _startpos_id_, _) = _menhir_stack in
          let (_endpos__4_, e) = (_endpos, _v) in
          let _v = _menhir_action_08 _endpos__4_ _startpos_id_ e id in
          let s = _v in
          let _v = _menhir_action_55 s in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | RETURN ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | IF ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState072
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState072
      | ELSE | END ->
          let _v_1 = _menhir_action_60 () in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState019 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061
      | SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos__1_, _endpos__2_) = (_startpos, _endpos) in
          let _v = _menhir_action_53 _endpos__2_ _startpos__1_ in
          _menhir_goto_return_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | REAL _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061
      | MINUS ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | LPAR ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState061
      | INT _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061
      | IDENT _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061
      | CHAR _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061
      | BOOL _v ->
          let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState061
      | _ ->
          _eRR ()
  
  and _menhir_goto_return_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let s = _v in
      let _v = _menhir_action_59 s in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_065 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState065 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING _v ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | REAL _v ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CHAR _v ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BOOL _v ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _menhir_s, s) = _menhir_stack in
      let ss = _v in
      let _v = _menhir_action_61 s ss in
      _menhir_goto_stmt_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState018 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState072 ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState070 ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState067 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_092 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_cell1_param_list_opt _menhir_cell0_RPAR, _menhir_box_compilation_unit) _menhir_cell1_opt_type -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_opt_type (_menhir_stack, _, ret) = _menhir_stack in
              let MenhirCell0_RPAR (_menhir_stack, _) = _menhir_stack in
              let MenhirCell1_param_list_opt (_menhir_stack, _, ps) = _menhir_stack in
              let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
              let MenhirCell1_ident (_menhir_stack, _, name, _, _) = _menhir_stack in
              let MenhirCell1_PROC (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (_endpos__9_, body) = (_endpos, _v) in
              let _v = _menhir_action_52 _endpos__9_ _startpos__1_ body name ps ret in
              let d = _v in
              let _v = _menhir_action_21 d in
              _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_expr (_menhir_stack, _, c) = _menhir_stack in
              let MenhirCell1_WHILE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (body, _endpos__6_) = (_v, _endpos) in
              let _v = _menhir_action_66 _endpos__6_ _startpos__1_ body c in
              let s = _v in
              let _v = _menhir_action_58 s in
              _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_stmt_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let s = _v in
      let _v = _menhir_action_25 s in
      _menhir_goto_else_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_else_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr, _menhir_box_compilation_unit) _menhir_cell1_stmt_list -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | END ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SEMI ->
              let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let MenhirCell1_stmt_list (_menhir_stack, _, t) = _menhir_stack in
              let MenhirCell1_expr (_menhir_stack, _, c) = _menhir_stack in
              let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
              let (e, _endpos__7_) = (_v, _endpos) in
              let _v = _menhir_action_28 _endpos__7_ _startpos__1_ c e t in
              let s = _v in
              let _v = _menhir_action_57 s in
              _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF, _menhir_box_compilation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
          | RETURN ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
          | IF ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState070
          | IDENT _v_0 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState070
          | END ->
              let _v_1 = _menhir_action_60 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | END ->
          let _v = _menhir_action_24 () in
          _menhir_goto_else_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_066 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
          | RETURN ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
          | IF ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState067
          | IDENT _v_0 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState067
          | ELSE | END ->
              let _v_1 = _menhir_action_60 () in
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState067 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETURN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, e) = (_endpos, _v) in
          let _v = _menhir_action_54 _endpos__3_ _startpos__1_ e in
          _menhir_goto_return_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_059 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | DO ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
          | RETURN ->
              _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
          | IF ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
          | IDENT _v_0 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState060
          | END ->
              let _v_1 = _menhir_action_60 () in
              _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_056 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_LPAR -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, e) = (_endpos, _v) in
          let _v = _menhir_action_51 _endpos__3_ _startpos__1_ e in
          _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState035 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REAL _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAR ->
          let e = _v in
          let _v = _menhir_action_04 e in
          _menhir_goto_arg_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_arg_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState077 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState033 ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState035 ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_055 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let es = _v in
      let _v = _menhir_action_07 es in
      _menhir_goto_arg_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_arg_list_opt : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState033 ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_078 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _menhir_s, f, _startpos_f_, _) = _menhir_stack in
          let (args, _endpos__5_) = (_v, _endpos_0) in
          let _v = _menhir_action_09 _endpos__5_ _startpos_f_ args f in
          let s = _v in
          let _v = _menhir_action_56 s in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_LPAR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_ident (_menhir_stack, _menhir_s, f, _startpos_f_, _) = _menhir_stack in
      let (_endpos__4_, args) = (_endpos, _v) in
      let _v = _menhir_action_49 _endpos__4_ _startpos_f_ args f in
      _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__4_ _startpos_f_ _v _menhir_s _tok
  
  and _menhir_run_037 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e) = _menhir_stack in
      let es = _v in
      let _v = _menhir_action_05 e es in
      _menhir_goto_arg_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer
      | NE ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_13 () in
          _menhir_goto_cmpop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MINUS ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LT ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_14 () in
          _menhir_goto_cmpop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | LE ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_15 () in
          _menhir_goto_cmpop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | GT ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_16 () in
          _menhir_goto_cmpop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | GE ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_17 () in
          _menhir_goto_cmpop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EQ ->
          let _menhir_stack = MenhirCell1_add (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_12 () in
          _menhir_goto_cmpop _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | COMMA | DO | RPAR | SEMI | THEN ->
          let a = _v in
          let _v = _menhir_action_11 a in
          _menhir_goto_cmp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_cmpop : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_add -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_cmpop (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | STRING _v_0 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState051
      | REAL _v_1 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState051
      | MINUS ->
          _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | LPAR ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | INT _v_2 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState051
      | IDENT _v_3 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState051
      | CHAR _v_4 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState051
      | BOOL _v_5 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState051
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_add as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_mul (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_mul (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | DO | EQ | GE | GT | LE | LT | MINUS | NE | PLUS | RPAR | SEMI | THEN ->
          let MenhirCell1_add (_menhir_stack, _menhir_s, a, _startpos_a_, _) = _menhir_stack in
          let (_endpos_b_, b) = (_endpos, _v) in
          let _v = _menhir_action_01 _endpos_b_ _startpos_a_ a b in
          _menhir_goto_add _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_a_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_mul (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SLASH ->
          let _menhir_stack = MenhirCell1_mul (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COMMA | DO | EQ | GE | GT | LE | LT | MINUS | NE | PLUS | RPAR | SEMI | THEN ->
          let (_endpos_m_, _startpos_m_, m) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_03 m in
          _menhir_goto_add _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_m_ _startpos_m_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_mul -> _ -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_mul (_menhir_stack, _menhir_s, a, _startpos_a_, _) = _menhir_stack in
      let (_endpos_b_, b) = (_endpos, _v) in
      let _v = _menhir_action_30 _endpos_b_ _startpos_a_ a b in
      _menhir_goto_mul _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_b_ _startpos_a_ _v _menhir_s _tok
  
  and _menhir_run_027 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_u_, _startpos_u_, u) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_32 u in
      _menhir_goto_mul _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_u_ _startpos_u_ _v _menhir_s _tok
  
  and _menhir_run_100 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _menhir_s, id, _, _) = _menhir_stack in
      let ty = _v in
      let _v = _menhir_action_39 id ty in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_param (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState097 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RPAR ->
          let p = _v in
          let _v = _menhir_action_40 p in
          _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_param_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState015 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_098 : type  ttv_stack. (ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_param -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_param (_menhir_stack, _menhir_s, p) = _menhir_stack in
      let ps = _v in
      let _v = _menhir_action_41 p ps in
      _menhir_goto_param_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_095 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let ps = _v in
      let _v = _menhir_action_43 ps in
      _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_param_list_opt : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_param_list_opt (_menhir_stack, _menhir_s, _v) in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_RPAR (_menhir_stack, _endpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState017
      | END | IDENT _ | IF | RETURN | WHILE ->
          let _v_0 = _menhir_action_37 () in
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState017 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_018 : type  ttv_stack. ((((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC, _menhir_box_compilation_unit) _menhir_cell1_ident _menhir_cell0_LPAR, _menhir_box_compilation_unit) _menhir_cell1_param_list_opt _menhir_cell0_RPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_opt_type (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
      | RETURN ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
      | IF ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState018
      | IDENT _v_0 ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState018
      | END ->
          let _v_1 = _menhir_action_60 () in
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_opt_type (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | RPAR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let n = _v_0 in
                  let _v = _menhir_action_36 n in
                  _menhir_goto_opt_size _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | SEMI ->
          let _v = _menhir_action_35 () in
          _menhir_goto_opt_size _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_opt_size : type  ttv_stack. (((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE, _menhir_box_compilation_unit) _menhir_cell1_ident, _menhir_box_compilation_unit) _menhir_cell1_opt_type -> _ -> _ -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_opt_type (_menhir_stack, _, elem) = _menhir_stack in
          let MenhirCell1_ident (_menhir_stack, _, name, _, _) = _menhir_stack in
          let MenhirCell1_TABLE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__5_, size) = (_endpos, _v) in
          let _v = _menhir_action_62 _endpos__5_ _startpos__1_ elem name size in
          let d = _v in
          let _v = _menhir_action_20 d in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState099
      | COMMA | RPAR ->
          let _v_0 = _menhir_action_37 () in
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_076 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v_1 ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState077
          | REAL _v_2 ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState077
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
          | LPAR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
          | INT _v_3 ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState077
          | IDENT _v_4 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState077
          | CHAR _v_5 ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState077
          | BOOL _v_6 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState077
          | RPAR ->
              let _v_7 = _menhir_action_06 () in
              _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
          | _ ->
              _eRR ())
      | ASSIGN ->
          let _menhir_s = MenhirState081 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | REAL _v ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CHAR _v ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BOOL _v ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v_1 ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState033
          | REAL _v_2 ->
              _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState033
          | MINUS ->
              _menhir_run_022 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
          | LPAR ->
              _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState033
          | INT _v_3 ->
              _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState033
          | IDENT _v_4 ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState033
          | CHAR _v_5 ->
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState033
          | BOOL _v_6 ->
              _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState033
          | RPAR ->
              let _v_7 = _menhir_action_06 () in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7
          | _ ->
              _eRR ())
      | COMMA | DO | EQ | GE | GT | LE | LT | MINUS | NE | PLUS | RPAR | SEMI | SLASH | STAR | THEN ->
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
          let _v = _menhir_action_50 _endpos_id_ _startpos_id_ id in
          _menhir_goto_primary _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_PROC as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | LPAR ->
          let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
          let _menhir_stack = MenhirCell0_LPAR (_menhir_stack, _startpos) in
          let _menhir_s = MenhirState015 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | RPAR ->
              let _v = _menhir_action_42 () in
              _menhir_goto_param_list_opt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ((ttv_stack, _menhir_box_compilation_unit) _menhir_cell1_TABLE as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_compilation_unit) _menhir_state -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState003
      | LPAR | SEMI ->
          let _v_0 = _menhir_action_37 () in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState003 _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_compilation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TABLE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | PROC ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | ITEM ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_22 () in
          _menhir_run_111 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let compilation_unit =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_compilation_unit v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
