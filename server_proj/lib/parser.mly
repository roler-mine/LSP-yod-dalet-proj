/* parser.mly — coverage-oriented JOVIAL J73-ish grammar
   Goal: consume (nearly) every keyword token so Menhir doesn't warn "unused token",
   and give each keyword a designated syntactic home (module/directive/decl/stmt/op/attr).
*/

%{
  (* Lightweight parse tree (keeps names + structure; you can map to your Ast later). *)

  type module_kind =
    | KProgram      (* PROGRAM *)
    | KCompool      (* COMPOOL *)
    | KProcModule   (* PROC as module kind, if used *)
    | KFunctionModule
    | KUnknown

  type directive = { d_name : string; d_args : literal list }

  and literal =
    | LInt of int
    | LFloat of float
    | LString of string
    | LBead of int * string
    | LNull
    | LBool of bool

  type use_attr =
    | ARec      (* REC   — module/proc use attribute *)
    | ARent     (* RENT  — module/proc use attribute *)
    | AStatic   (* STATIC *)
    | AParallel (* PARALLEL *)
    | AInline   (* INLINE *)

  type decl_attr =
    | DStatic
    | DConstant
    | DDefault of expr
    | DLike of string
    | DPos of expr
    | DRep of expr
    | DOverlay of string
    | DInstance of string
    | DRec
    | DRent
    | DInline
    | DParallel

  and type_spec =
    | TAtom of string      (* TYPEATOM: U/S/F/A/B/C/P/V ... *)
    | TNamed of string     (* IDENT *)

  and status_item =
    | SName of string
    | SVal of string       (* V(name) pattern *)

  and decl =
    | DItem of string list * type_spec option * decl_attr list
    | DTable of string * dim list * type_spec option * decl_attr list * decl list
    | DBlock of string * decl_attr list * decl list
    | DTypeStatus of string * status_item list
    | DTypeAlias of string * type_spec
    | DOverlayDecl of string
    | DDefine of string * define_rhs
    | DLinkage of linkage_kind * linkage_target
    | DLabelDecl of string list

  and define_rhs =
    | DefString of string
    | DefExpr of expr

  and linkage_kind =
    | LDef   (* DEF *)
    | LRef   (* REF *)

  and linkage_target =
    | LName of string
    | LProcSig of string * param list
    | LFunSig of string * param list * type_spec option

  and dim =
    | DimStar
    | DimInt of int
    | DimId of string

  and param_mode = ByRef | ByVal | ByRes

  and param = { pmode : param_mode option; pname : string; ptype : type_spec option }

  and lvalue =
    | LVar of string
    | LIndex of string * expr list

  and stmt =
    | SLabel of string * stmt
    | SAssign of lvalue * expr
    | SCall of string * expr list
    | SIf of expr * stmt * stmt option
    | SIfElsif of (expr * stmt) list * stmt option
    | SWhile of expr * stmt
    | SForTo of string * expr * expr * expr option * stmt          (* FOR i: a TO b [BY step] ... *)
    | SForWhile of string * expr * expr option * expr * stmt       (* FOR i: a [BY step] WHILE cond ... *)
    | SCase of expr * (expr list * stmt list) list * stmt list option
    | SGoto of string
    | SReturn of expr option
    | SExit of string option
    | SStop of expr option
    | SAbort of expr option
    | SFallthru
    | SBlock of stmt list
    | SNoop

  and expr =
    | EInt of int
    | EFloat of float
    | EString of string
    | EBead of int * string
    | ENull
    | EBool of bool
    | EVar of string
    | ECall of string * expr list
    | EUn of string * expr
    | EBin of string * expr * expr
    | EParen of expr

  type module_ =
    { kind : module_kind
    ; name : string option
    ; directives : directive list
    ; attrs : use_attr list
    ; decls : decl list
    ; stmts : stmt list
    ; procs : proc list
    }

  and proc_kind = PProc | PFunction

  and proc =
    { pkind : proc_kind
    ; pname : string
    ; params : param list
    ; rettype : type_spec option
    ; pattrs : use_attr list
    ; body : stmt list option   (* None = signature only; Some = has body *)
    }
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

%start <module_> compilation_unit

%%

compilation_unit:
  module_ EOF { $1 }

module_:
  START module_header module_body TERM
    {
      let (kind, name, directives, attrs) = $2 in
      let (decls, stmts, procs) = $3 in
      { kind; name; directives; attrs; decls; stmts; procs }
    }

module_header:
  directives_opt module_kind_opt module_name_opt use_attrs_opt compool_includes_opt
    {
      (* ICOMPOOL (or !ICOMPOOL) is treated as “directive-ish” inclusion. *)
      let directives = $1 @ $5 in
      ($2, $3, directives, $4)
    }

module_kind_opt:
  | PROGRAM { KProgram }
  | COMPOOL { KCompool }
  | PROC    { KProcModule }        /* if used as module-kind */
  | FUNCTION { KFunctionModule }   /* if used as module-kind */
  | /* empty */ { KUnknown }

module_name_opt:
  | IDENT { Some $1 }
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
  /* General directive form: !NAME args... ; */
  BANG DIRECTIVE_NAME directive_args_opt SEMI
    { { d_name = $2; d_args = $3 } }

directive_args_opt:
  | /* empty */ { [] }
  | directive_args { $1 }

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
| IDENT  { LString $1 }   /* permissive: treat bare words as string-ish */

compool_includes_opt:
  | /* empty */ { [] }
  /* Some codebases use ICOMPOOL without '!' right after START. */
  | compool_includes_opt ICOMPOOL IDENT SEMI
      { $1 @ [ { d_name = "ICOMPOOL"; d_args = [LString $3] } ] }

module_body:
  /* PROGRAM modules usually have BEGIN..END; COMPOOL sometimes is declarations-only.
     We accept both to maximize coverage. */
  | BEGIN top_items_opt END semis_opt
      { $2 }
  | top_items_opt
      { $1 }

semis_opt:
  | /* empty */ { () }
  | semis_opt SEMI { () }

top_items_opt:
  | /* empty */ { ([], [], []) }
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
  | SEMI { `Stmt SNoop }  /* tolerate empty statement separators */

(* ---------- Declarations (each keyword has a home) ---------- *)

decl:
  | ITEM ident_list type_spec_opt decl_attrs_opt SEMI
      { DItem ($2, $3, $4) }

  | TABLE IDENT table_dims_opt type_spec_opt decl_attrs_opt SEMI table_body_opt
      { DTable ($2, $3, $4, $5, $7) }

  | BLOCK IDENT decl_attrs_opt SEMI block_decl_body_opt
      { DBlock ($2, $3, $5) }

  | TYPE IDENT STATUS LPAREN status_list RPAREN SEMI
      { DTypeStatus ($2, $5) }

  | TYPE IDENT EQUAL type_spec SEMI
      { DTypeAlias ($2, $4) }

  | OVERLAY IDENT SEMI
      { DOverlayDecl $2 }

  | DEFINE IDENT EQUAL define_rhs SEMI
      { DDefine ($2, $4) }

  | linkage_decl
      { $1 }

  | LABEL ident_list SEMI
      { DLabelDecl $2 }

define_rhs:
  | STRING { DefString $1 }
  | expr   { DefExpr $1 }

linkage_decl:
  | DEF linkage_target SEMI { DLinkage (LDef, $2) }
  | REF linkage_target SEMI { DLinkage (LRef, $2) }

linkage_target:
  | IDENT { LName $1 }
  | PROC IDENT formal_params_opt { LProcSig ($2, $3) }
  | FUNCTION IDENT formal_params_opt type_spec_opt { LFunSig ($2, $3, $4) }

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
  | IDENT { DimId $1 }

type_spec_opt:
  | /* empty */ { None }
  | COLON type_spec { Some $2 }
  | TYPE type_spec { Some $2 }      /* allow TYPE <spec> in some styles */

type_spec:
  | TYPEATOM { TAtom $1 }
  | IDENT    { TNamed $1 }

decl_attrs_opt:
  | /* empty */ { [] }
  | WITH LPAREN decl_attr_list RPAREN { $3 }

decl_attr_list:
  decl_attr { [$1] }
| decl_attr_list COMMA decl_attr { $1 @ [$3] }

decl_attr:
  | STATIC            { DStatic }
  | CONSTANT          { DConstant }
  | DEFAULT expr      { DDefault $2 }
  | LIKE IDENT        { DLike $2 }
  | POS LPAREN expr RPAREN { DPos $3 }
  | REP LPAREN expr RPAREN { DRep $3 }
  | OVERLAY IDENT     { DOverlay $2 }
  | INSTANCE IDENT    { DInstance $2 }
  | REC               { DRec }
  | RENT              { DRent }
  | INLINE            { DInline }
  | PARALLEL          { DParallel }

ident_list:
  | IDENT { [$1] }
  | ident_list COMMA IDENT { $1 @ [$3] }

status_list:
  status_item { [$1] }
| status_list COMMA status_item { $1 @ [$3] }

status_item:
  | IDENT { SName $1 }
  | TYPEATOM LPAREN IDENT RPAREN { SVal $3 }  /* accepts V(NAME) style */

(* ---------- Procedure / function definitions ---------- *)

proc_def:
  | DEF PROC IDENT formal_params_opt use_attrs_opt SEMI proc_body_opt proc_end
      { { pkind = PProc; pname = $3; params = $4; rettype = None; pattrs = $5; body = $7 } }

  | DEF FUNCTION IDENT formal_params_opt type_spec_opt use_attrs_opt SEMI proc_body_opt proc_end
      { { pkind = PFunction; pname = $3; params = $4; rettype = $5; pattrs = $6; body = $8 } }

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
  | param_mode_opt IDENT type_spec_opt
      { { pmode = $1; pname = $2; ptype = $3 } }

param_mode_opt:
  | BYREF { Some ByRef }
  | BYVAL { Some ByVal }
  | BYRES { Some ByRes }
  | /* empty */ { None }

proc_body_opt:
  /* If absent, this is a signature-only DEF/REF-like declaration. */
  | /* empty */ { None }
  | BEGIN stmt_list_opt END semis_opt { Some $2 }

proc_end:
  | END proc_end_name_opt semis_opt { () }

proc_end_name_opt:
  | IDENT { Some $1 }
  | /* empty */ { None }

stmt_list_opt:
  | /* empty */ { [] }
  | stmt_list_opt stmt { $1 @ [$2] }

(* ---------- Statements ---------- *)

stmt:
  | IDENT COLON stmt { SLabel ($1, $3) }             /* label: statement */
  | lvalue EQUAL expr SEMI { SAssign ($1, $3) }      /* assignment */
  | call_stmt { $1 }
  | if_stmt { $1 }
  | while_stmt { $1 }
  | for_stmt { $1 }
  | case_stmt { $1 }
  | GOTO IDENT SEMI { SGoto $2 }
  | RETURN expr_opt SEMI { SReturn $2 }
  | EXIT exit_name_opt SEMI { SExit $2 }
  | STOP expr_opt SEMI { SStop $2 }
  | ABORT expr_opt SEMI { SAbort $2 }
  | FALLTHRU SEMI { SFallthru }
  | BEGIN stmt_list_opt END semis_opt { SBlock $2 }
  | SEMI { SNoop }

exit_name_opt:
  | IDENT { Some $1 }
  | /* empty */ { None }

expr_opt:
  | /* empty */ { None }
  | expr { Some $1 }

call_stmt:
  | IDENT call_args_opt SEMI { SCall ($1, $2) }

call_args_opt:
  | /* empty */ { [] }
  | LPAREN expr_list_opt RPAREN { $2 }
  | WITH LPAREN expr_list_opt RPAREN { $3 }   /* “WITH” call syntax placeholder */

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
  /* FOR i: a TO b [BY step] stmt */
  | FOR IDENT COLON expr TO expr by_opt stmt
      { SForTo ($2, $4, $6, $7, $8) }

  /* FOR i: a [BY step] WHILE cond stmt */
  | FOR IDENT COLON expr by_opt WHILE expr stmt
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
  | WHEN case_labels COLON stmt_list_opt
      { ($2, $4) }

case_labels:
  | expr { [$1] }
  | case_labels COMMA expr { $1 @ [$3] }

otherwise_opt:
  | /* empty */ { None }
  | OTHERWISE COLON stmt_list_opt { Some $3 }

lvalue:
  | IDENT { LVar $1 }
  | IDENT LPAREN expr_list_opt RPAREN { LIndex ($1, $3) }

(* ---------- Expressions (operators + word-relops + booleans) ---------- *)

expr:
  | INT    { EInt $1 }
  | FLOAT  { EFloat $1 }
  | STRING { EString $1 }
  | BEAD   { let (b,v) = $1 in EBead (b,v) }
  | NULL   { ENull }
  | TRUE   { EBool true }
  | FALSE  { EBool false }

  | IDENT  { EVar $1 }
  | IDENT LPAREN expr_list_opt RPAREN { ECall ($1, $3) }

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

  /* JOVIAL word-relops */
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
