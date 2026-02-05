/* lib/parser.mly — coverage-oriented JOVIAL J73-ish grammar */

%{
  open Ast
  let mk_span = Ast.mk_span
  let mk_loc  = Ast.mk_loc
%}

%token START TERM
%token PROGRAM COMPOOL ICOMPOOL
%token DEF PROC FUNCTION

%token BEGIN END
%token ITEM TABLE STATUS TYPE BLOCK

%token IF THEN ELSE ELSIF
%token FOR BY WHILE TO
%token CASE OF WHEN OTHERWISE
%token GOTO RETURN EXIT STOP ABORT FALLTHRU

%token DEFAULT DEFINE INLINE INSTANCE LABEL LIKE OVERLAY POS REC REF RENT REP
%token BYREF BYVAL BYRES WITH
%token STATIC CONSTANT PARALLEL

%token NULL TRUE FALSE

%token AND OR NOT XOR EQV MOD
%token EQ NQ LS LQ GR GQ

%token BANG
%token <string> DIRECTIVE_NAME

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token <int * string> BEAD
%token <string> TYPEATOM

%token LPAREN RPAREN LBRACK RBRACK
%token COMMA COLON SEMI
%token PLUS MINUS STAR SLASH
%token LT LE GT GE NEQ EQUAL
%token EOF

%left OR
%left XOR EQV
%left AND
%right NOT
%nonassoc LT LE GT GE NEQ EQUAL EQ NQ LS LQ GR GQ
%left PLUS MINUS
%left STAR SLASH MOD
%right UMINUS

%start <Ast.compilation_unit> compilation_unit
%type  <Ast.module_> module_

%type  <Lexing.position> proc_end

%%

compilation_unit:
  module_ EOF { $1 }

module_:
  START module_header module_body TERM
    {
      let (kind, name, directives, attrs) = $2 in
      let (decls, stmts, procs) = $3 in
      { m_kind = kind
      ; m_name = name
      ; m_directives = directives
      ; m_attrs = attrs
      ; m_decls = decls
      ; m_stmts = stmts
      ; m_procs = procs
      }
    }

module_header:
  directives_opt module_kind_opt module_name_opt use_attrs_opt compool_includes_opt
    {
      let directives = $1 @ $5 in
      ($2, $3, directives, $4)
    }

module_kind_opt:
  | PROGRAM { Program }
  | COMPOOL { Compool }
  | ICOMPOOL { Icompool }
  | PROC    { Proc_module }
  | FUNCTION { Function_module }
  | /* empty */ { Unknown }

module_name_opt:
  | IDENT { Some (mk_loc $1 $startpos $endpos) }
  | /* empty */ { None }

use_attrs_opt:
  | /* empty */ { [] }
  | use_attrs_opt use_attr { $1 @ [$2] }

use_attr:
  | REC      { ARec }
  | RENT     { ARent }
  | STATIC   { AStatic }
  | PARALLEL { AParallel }
  | INLINE   { AInline }

directives_opt:
  | /* empty */ { [] }
  | directives_opt directive { $1 @ [$2] }

directive:
  BANG DIRECTIVE_NAME directive_args_opt SEMI
    { { dir_name = $2; dir_args = $3 } }

directive_args_opt:
  | /* empty */ { [] }
  | directive_args { $1 }
  | LPAREN directive_arg_list_opt RPAREN { $2 }

directive_arg_list_opt:
  | /* empty */ { [] }
  | directive_arg_list { $1 }

directive_arg_list:
  directive_arg { [$1] }
| directive_arg_list COMMA directive_arg { $1 @ [$3] }

directive_args:
  directive_arg { [$1] }
| directive_args directive_arg { $1 @ [$2] }

directive_arg:
  STRING { LString $1 }
| INT    { LInt $1 }
| FLOAT  { LFloat $1 }
| BEAD   { let (b,v) = $1 in LBead (b,v) }
| NULL   { LNull }
| TRUE   { LBool true }
| FALSE  { LBool false }
| IDENT  { LString $1 }

compool_includes_opt:
  | /* empty */ { [] }
  | compool_includes_opt ICOMPOOL IDENT SEMI
      { $1 @ [ { dir_name = "ICOMPOOL"; dir_args = [LString $3] } ] }

module_body:
  | BEGIN top_items_opt END semis_opt { $2 }
  | top_items_opt { $1 }

semis_opt:
  | /* empty */ { () }
  | semis_opt SEMI { () }

top_items_opt:
  | /* empty */ { (([] : Ast.decl list), ([] : Ast.stmt list), ([] : Ast.proc list)) }
  | top_items_opt top_item
      {
        let (ds, ss, ps) = $1 in
        match $2 with
        | `Decl d -> (ds @ [d], ss, ps)
        | `Stmt s -> (ds, ss @ [s], ps)
        | `Proc p -> (ds, ss, ps @ [p])
      }

top_item:
  | decl { `Decl $1 }
  | proc_def { `Proc $1 }
  | stmt { `Stmt $1 }
  | SEMI { `Stmt SNoop }
  | error SEMI { `Stmt (SError (mk_span $startpos $endpos)) }

(* ---------- Declarations ---------- *)

decl:
  | ITEM ident_list type_spec_opt decl_attrs_opt SEMI
      { DItem ($2, $3, $4) }

  | TABLE ident_loc table_dims_opt type_spec_opt decl_attrs_opt SEMI table_body_opt
      { DTable ($2, $3, $4, $5, $7) }

  | BLOCK ident_loc decl_attrs_opt SEMI block_decl_body_opt
      { DBlock ($2, $3, $5) }

  | TYPE ident_loc STATUS LPAREN status_list RPAREN SEMI
      { DTypeStatus ($2, $5) }

  | TYPE ident_loc EQUAL type_spec SEMI
      { DTypeAlias ($2, $4) }

  | OVERLAY ident_loc SEMI
      { DOverlayDecl $2 }

  | DEFINE ident_loc define_rhs SEMI
      { DDefine ($2, $3) }

  | DEFINE ident_loc EQUAL define_rhs SEMI
      { DDefine ($2, $4) }

  | linkage_decl { $1 }

  | LABEL ident_list SEMI
      { DLabelDecl $2 }

define_rhs:
  | STRING { DefString $1 }
  | expr   { DefExpr $1 }

linkage_decl:
  | DEF linkage_target SEMI { DLinkage (LDef, $2) }
  | REF linkage_target SEMI { DLinkage (LRef, $2) }

linkage_target:
  | ident_loc { LName $1 }
  | PROC ident_loc formal_params_opt { LProcSig ($2, $3) }
  | FUNCTION ident_loc formal_params_opt type_spec_opt { LFunSig ($2, $3, $4) }

block_decl_body_opt:
  | /* empty */ { [] }
  | BEGIN decl_list_opt END semis_opt { $2 }

decl_list_opt:
  | /* empty */ { [] }
  | decl_list_opt decl { $1 @ [$2] }

table_body_opt:
  | /* empty */ { [] }
  | BEGIN decl_list_opt END semis_opt { $2 }

table_dims_opt:
  | /* empty */ { [] }
  | LPAREN dim_list RPAREN { $2 }

dim_list:
  dim { [$1] }
| dim_list COMMA dim { $1 @ [$3] }

dim:
  | STAR  { DimStar }
  | INT   { DimInt $1 }
  | ident_loc { DimId $1 }

type_spec_opt:
  | /* empty */ { None }
  | type_spec { Some $1 }
  | COLON type_spec { Some $2 }
  | TYPE type_spec { Some $2 }

type_spec:
  | TYPEATOM INT { TAtom ($1 ^ " " ^ string_of_int $2) }
  | TYPEATOM     { TAtom $1 }
  | ident_loc    { TNamed $1 }

decl_attrs_opt:
  | /* empty */ { [] }
  | WITH LPAREN decl_attr_list RPAREN { $3 }

decl_attr_list:
  decl_attr { [$1] }
| decl_attr_list COMMA decl_attr { $1 @ [$3] }

decl_attr:
  | STATIC                   { DStatic }
  | CONSTANT                 { DConstant }
  | DEFAULT expr             { DDefault $2 }
  | LIKE ident_loc           { DLike $2 }
  | POS LPAREN expr RPAREN   { DPos $3 }
  | REP LPAREN expr RPAREN   { DRep $3 }
  | OVERLAY ident_loc        { DOverlay $2 }
  | INSTANCE ident_loc       { DInstance $2 }
  | REC                      { DRec }
  | RENT                     { DRent }
  | INLINE                   { DInline }
  | PARALLEL                 { DParallel }

ident_loc:
  | IDENT { mk_loc $1 $startpos $endpos }

ident_list:
  | ident_loc { [$1] }
  | ident_list COMMA ident_loc { $1 @ [$3] }

status_list:
  status_item { [$1] }
| status_list COMMA status_item { $1 @ [$3] }

status_item:
  | ident_loc { SName $1 }
  | TYPEATOM LPAREN ident_loc RPAREN { SVal $3 }

(* ---------- Procedures ---------- *)

proc_def:
  | DEF PROC ident_loc formal_params_opt use_attrs_opt SEMI proc_body_opt proc_end
      {
        let endpos = $8 in
        { pr_kind = PProc
        ; pr_name = $3
        ; pr_span = mk_span $startpos endpos
        ; pr_params = $4
        ; pr_rettype = None
        ; pr_attrs = $5
        ; pr_directives = []
        ; pr_body = $7
        }
      }

  | DEF FUNCTION ident_loc formal_params_opt type_spec_opt use_attrs_opt SEMI proc_body_opt proc_end
      {
        let endpos = $9 in
        { pr_kind = PFunction
        ; pr_name = $3
        ; pr_span = mk_span $startpos endpos
        ; pr_params = $4
        ; pr_rettype = $5
        ; pr_attrs = $6
        ; pr_directives = []
        ; pr_body = $8
        }
      }

formal_params_opt:
  | /* empty */ { [] }
  | LPAREN param_list_opt RPAREN { $2 }

param_list_opt:
  | /* empty */ { [] }
  | param_list { $1 }

param_list:
  | param { [$1] }
  | param_list COMMA param { $1 @ [$3] }

param:
  | param_mode_opt ident_loc type_spec_opt
      { { prm_mode = $1; prm_name = $2; prm_type = $3 } }

param_mode_opt:
  | /* empty */ { None }
  | BYREF { Some ByRef }
  | BYVAL { Some ByVal }
  | BYRES { Some ByRes }

proc_body_opt:
  | /* empty */ { None }
  | BEGIN block_items_opt END semis_opt { Some $2 }

proc_end:
  | e=END proc_end_name_opt semis_opt { ignore e; $endpos(e) }

proc_end_name_opt:
  | ident_loc { Some $1 }
  | /* empty */ { None }

block_items_opt:
  | /* empty */ { [] }
  | block_items_opt block_item
      { match $2 with None -> $1 | Some s -> $1 @ [s] }

block_item:
  | decl { None }
  | stmt { Some $1 }
  | SEMI { Some SNoop }

(* ---------- Statements ---------- *)

stmt:
  | ident_loc COLON stmt { SLabel ($1, $3) }
  | lvalue EQUAL expr SEMI { SAssign ($1, $3) }
  | call_stmt { $1 }
  | if_stmt { $1 }
  | while_stmt { $1 }
  | for_stmt { $1 }
  | case_stmt { $1 }
  | GOTO ident_loc SEMI { SGoto $2 }
  | RETURN expr_opt SEMI { SReturn $2 }
  | EXIT exit_name_opt SEMI { SExit $2 }
  | STOP expr_opt SEMI { SStop $2 }
  | ABORT expr_opt SEMI { SAbort $2 }
  | FALLTHRU SEMI { SFallthru }
  | BEGIN stmt_list_opt END semis_opt { SBlock $2 }
  | SEMI { SNoop }
  | error SEMI { SError (mk_span $startpos $endpos) }

stmt_list_opt:
  | /* empty */ { [] }
  | stmt_list_opt stmt { $1 @ [$2] }

exit_name_opt:
  | ident_loc { Some $1 }
  | /* empty */ { None }

expr_opt:
  | /* empty */ { None }
  | expr { Some $1 }

call_stmt:
  | ident_loc call_args_opt SEMI { SCall ($1, $2) }

call_args_opt:
  | /* empty */ { [] }
  | LPAREN expr_list_opt RPAREN { $2 }
  | WITH LPAREN expr_list_opt RPAREN { $3 }

expr_list_opt:
  | /* empty */ { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }

if_stmt:
  | IF expr THEN stmt else_opt { SIf ($2, $4, $5) }
  | IF expr THEN stmt elsif_list else_opt
      { SIfElsif ((($2,$4) :: $5), $6) }

elsif_list:
  | ELSIF expr THEN stmt { [($2, $4)] }
  | elsif_list ELSIF expr THEN stmt { $1 @ [($3, $5)] }

else_opt:
  | /* empty */ { None }
  | ELSE stmt { Some $2 }

while_stmt:
  | WHILE expr stmt { SWhile ($2, $3) }

for_stmt:
  | FOR ident_loc COLON expr TO expr by_opt stmt
      { SForTo ($2, $4, $6, $7, $8) }

  | FOR ident_loc COLON expr by_opt WHILE expr stmt
      { SForWhile ($2, $4, $5, $7, $8) }

by_opt:
  | /* empty */ { None }
  | BY expr { Some $2 }

case_stmt:
  | CASE expr OF case_clauses otherwise_opt END SEMI
      { SCase ($2, $4, $5) }

case_clauses:
  | case_clause { [$1] }
  | case_clauses case_clause { $1 @ [$2] }

case_clause:
  | WHEN case_labels COLON stmt_list_opt { ($2, $4) }

case_labels:
  | expr { [$1] }
  | case_labels COMMA expr { $1 @ [$3] }

otherwise_opt:
  | /* empty */ { None }
  | OTHERWISE COLON stmt_list_opt { Some $3 }

lvalue:
  | ident_loc { LVar $1 }
  | ident_loc LPAREN expr_list_opt RPAREN { LIndex ($1, $3) }

(* ---------- Expressions ---------- *)

expr:
  | INT    { EInt $1 }
  | FLOAT  { EFloat $1 }
  | STRING { EString $1 }
  | BEAD   { let (b,v) = $1 in EBead (b,v) }
  | NULL   { ENull }
  | TRUE   { EBool true }
  | FALSE  { EBool false }
  | ident_loc  { EVar $1 }
  | ident_loc LPAREN expr_list_opt RPAREN { ECall ($1, $3) }
  | LPAREN expr RPAREN { EParen $2 }

  | MINUS expr %prec UMINUS { EUn ("-", $2) }
  | NOT expr { EUn ("NOT", $2) }

  | expr STAR expr  { EBin ("*", $1, $3) }
  | expr SLASH expr { EBin ("/", $1, $3) }
  | expr MOD expr   { EBin ("MOD", $1, $3) }
  | expr PLUS expr  { EBin ("+", $1, $3) }
  | expr MINUS expr { EBin ("-", $1, $3) }

  | expr LT expr    { EBin ("<", $1, $3) }
  | expr LE expr    { EBin ("<=", $1, $3) }
  | expr GT expr    { EBin (">", $1, $3) }
  | expr GE expr    { EBin (">=", $1, $3) }
  | expr NEQ expr   { EBin ("<>", $1, $3) }
  | expr EQUAL expr { EBin ("=", $1, $3) }

  | expr EQ expr    { EBin ("EQ", $1, $3) }
  | expr NQ expr    { EBin ("NQ", $1, $3) }
  | expr LS expr    { EBin ("LS", $1, $3) }
  | expr LQ expr    { EBin ("LQ", $1, $3) }
  | expr GR expr    { EBin ("GR", $1, $3) }
  | expr GQ expr    { EBin ("GQ", $1, $3) }

  | expr AND expr   { EBin ("AND", $1, $3) }
  | expr OR expr    { EBin ("OR", $1, $3) }
  | expr XOR expr   { EBin ("XOR", $1, $3) }
  | expr EQV expr   { EBin ("EQV", $1, $3) }
;
