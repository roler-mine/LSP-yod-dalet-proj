%{
open Ast

(* Create an LSP-friendly located node using Menhir's $startpos/$endpos. *)
let loc0 (sp:Lexing.position) (ep:Lexing.position) (node:'a) : 'a located =
  { node; span = (sp, ep) }
%}

/* ---------- Tokens (must match the lexer) ---------- */

/* module/unit */
%token START TERM PROGRAM COMPOOL

/* directives */
%token ICOMPOOL ICOPY ISKIP IBEGIN IEND ILINKAGE ITRACE IINTERFERENCE IREDUCIBLE
%token ILIST INOLIST IEJECT IINITIALIZE IORDER ILEFTRIGHT IREARRANGE IBASE IISBASE IDROP

/* define */
%token DEFINE
%token <string> DEFINE_STRING

/* decl/module keywords */
%token DEF REF RENT REC REP INLINE LABEL INSTANCE OVERLAY PARALLEL STATIC
%token TYPE ITEM TABLE BLOCK CONSTANT AS LIKE
%token BIT BITSIZE BYTESIZE WORDSIZE STATUS
%token BY POS SGNL
%token PROC FUNCTION

/* statements */
%token IF THEN ELSE CASE DEFAULT FALLTHRU FOR WHILE EXIT GOTO RETURN STOP ABORT BEGIN END

/* literals / builtins */
%token TRUE FALSE NULL
%token LOC NEXT FIRST LAST LBOUND UBOUND

/* word operators */
%token MOD NOT AND OR XOR EQV SHIFTL SHIFTR

/* punctuation/operators */
%token LPAREN RPAREN COMMA SEMI COLON DOT
%token PLUS MINUS STAR SLASH POW AT
%token LT LE GT GE EQ NE
%token LCONV RCONV

/* payload tokens */
%token <string> NAME
%token <char> LETTER
%token <int> INT
%token <string> REAL
%token <string> CHARLIT
%token <int * string> BITLIT

/* trivia / errors */
%token <string> COMMENT
%token <string> ERROR
%token EOF

/* ---------- Precedence ---------- */

%left OR
%left XOR
%left AND
%left EQV
%nonassoc EQ NE LT LE GT GE
%left SHIFTL SHIFTR
%left PLUS MINUS
%left STAR SLASH MOD
%right POW
%right NOT
%right UMINUS
%right UAT

%start <Ast.compilation_unit Ast.located> compilation_unit

%%

ident:
  | n=NAME { n }

index_letter:
  | c=LETTER { c }

status_const:
  /* V(RED) — V is usually a single-letter token. */
  | _v=LETTER LPAREN n=ident RPAREN { n }

type_class_letter:
  | c=LETTER { c }

maybe_int:
  | { None }
  | n=INT { Some n }

/* ---------- Compilation unit ---------- */

compilation_unit:
  | START pre=interlude kind=module_kind post=interlude TERM EOF
      {
        loc0 $startpos $endpos
          { pre_directives=fst pre; defines=snd pre; kind; post_directives=fst post }
      }

interlude:
  | { ([], []) }
  | xs=interlude x=interlude_item
      {
        let (ds, defs) = xs in
        match x with
        | `Dir d -> (ds @ [d], defs)
        | `Def df -> (ds, defs @ [df])
      }

interlude_item:
  | d=directive_stmt { `Dir d }
  | df=define_stmt { `Def df }
  | COMMENT { `Dir (loc0 $startpos $endpos { kind=D_ILIST; args=[] }) }
  | msg=ERROR { `Dir (loc0 $startpos $endpos { kind=D_ILIST; args=[ArgString ("LEXERR:"^msg)] }) }

/* ---------- Modules ---------- */

module_kind:
  | PROGRAM name=ident SEMI _pre=interlude ds=decls_opt body=block defs=def_section_opt _post=interlude
      { MProgram { name; decls=ds; body; defs } }

  | COMPOOL name=ident SEMI ds=decls_opt _post=interlude
      { MCompool { name; decls=ds } }

  | ds=decls_opt defs=def_section_opt _post=interlude
      { MProcModule { decls=ds; defs } }

/* ---------- Directives / DEFINE ---------- */

directive_stmt:
  | k=directive_kind args=dir_args_opt SEMI
      { loc0 $startpos $endpos { kind=k; args } }

directive_kind:
  | ICOMPOOL { D_ICOMPOOL }
  | ICOPY { D_ICOPY }
  | ISKIP { D_ISKIP }
  | IBEGIN { D_IBEGIN }
  | IEND { D_IEND }
  | ILINKAGE { D_ILINKAGE }
  | ITRACE { D_ITRACE }
  | IINTERFERENCE { D_IINTERFERENCE }
  | IREDUCIBLE { D_IREDUCIBLE }
  | ILIST { D_ILIST }
  | INOLIST { D_INOLIST }
  | IEJECT { D_IEJECT }
  | IINITIALIZE { D_IINITIALIZE }
  | IORDER { D_IORDER }
  | ILEFTRIGHT { D_ILEFTRIGHT }
  | IREARRANGE { D_IREARRANGE }
  | IBASE { D_IBASE }
  | IISBASE { D_IISBASE }
  | IDROP { D_IDROP }

dir_args_opt:
  | { [] }
  | a=dir_arg { [a] }
  | LPAREN xs=dir_arg_list_opt RPAREN { xs }

dir_arg_list_opt:
  | { [] }
  | xs=dir_arg_list { xs }

dir_arg_list:
  | a=dir_arg { [a] }
  | xs=dir_arg_list COMMA a=dir_arg { xs @ [a] }

dir_arg:
  | n=ident { ArgName n }
  | i=INT { ArgInt i }
  | s=CHARLIT { ArgString s }
  | STAR { ArgStar }
  | LPAREN xs=dir_arg_list_opt RPAREN { ArgList xs }
  | a=INT COLON b=INT { ArgRange (a,b) }

define_stmt:
  | DEFINE name=ident v=DEFINE_STRING SEMI
      { loc0 $startpos $endpos { name; value=v } }
  | DEFINE name=ident msg=ERROR SEMI
      { loc0 $startpos $endpos { name; value=("<lex-error:"^msg^">") } }

/* ---------- Declarations ---------- */

decls_opt:
  | { [] }
  | ds=decls_opt d=decl { ds @ [d] }

decl:
  | d=type_decl { d }
  | d=item_decl { d }
  | d=const_item_decl { d }
  | d=table_decl { d }
  | d=block_decl { d }
  | LABEL n=ident SEMI { loc0 $startpos $endpos (DLabel n) }
  | INLINE n=ident SEMI { loc0 $startpos $endpos (DInline n) }
 // | error SEMI { loc0 $startpos $endpos (DUnknownDecl "<parse-error-decl>") }

type_decl:
  | TYPE name=ident td=type_desc SEMI
      { loc0 $startpos $endpos (DType (name, td)) }

const_item_decl:
  | CONSTANT ITEM name=ident td=type_desc init=init_opt SEMI
      { loc0 $startpos $endpos (DItem { constant=true; name; ty=td; init }) }

item_decl:
  | ITEM name=ident td=type_desc init=init_opt SEMI
      { loc0 $startpos $endpos (DItem { constant=false; name; ty=td; init }) }

init_opt:
  | { None }
  | MINUS e=expr { Some e }  /* '-' used for initialization/assignment */

table_decl:
  | TABLE name=ident dims=table_dims td=type_desc SEMI
      { loc0 $startpos $endpos (DTable { name; dims; ty=td }) }

table_dims:
  | LPAREN ds=dim_list RPAREN { ds }
  | { [] }

dim_list:
  | d=dim { [d] }
  | ds=dim_list COMMA d=dim { ds @ [d] }

dim:
  | STAR { DimStar }
  | n=INT { DimSize n }
  | a=INT COLON b=INT { DimRange (a,b) }

block_decl:
  | BLOCK name=ident BEGIN fields=decls_opt END SEMI
      { loc0 $startpos $endpos (DBlock { name; fields }) }

/* ---------- Type descriptors ---------- */

type_desc:
  | STATUS LPAREN xs=status_list RPAREN { TyStatus xs }
  | c=type_class_letter n=maybe_int
      {
        match c with
        | 'U' | 'S' | 'F' | 'B' | 'C' -> TyClass (c, n)
        | 'P' -> TyPointer None
        | _ -> TyName (String.make 1 c)
      }
  | BIT n=maybe_int { TyBit n }
  | a=LETTER i=INT COMMA j=INT
      { if a='A' then TyAlpha (i,j) else TyName (String.make 1 a) }
  | n=ident { TyName n }

status_list:
  | s=status_entry { [s] }
  | xs=status_list COMMA s=status_entry { xs @ [s] }

status_entry:
  | sc=status_const { sc }

/* ---------- DEF section (procedures/functions) ---------- */

def_section_opt:
  | { [] }
  | xs=def_section_opt d=defn { xs @ [d] }

defn:
  | DEF s=subroutine_def { s }
 /* | error SEMI
      {
        loc0 $startpos $endpos
          { kind=Proc; name="<bad-subroutine>"; params=[]; ret=None; decls=[];
            body=[loc0 $startpos $endpos (SError (Some "bad subroutine"))] }
      }*/

subroutine_def:
  | PROC name=ident params=formals_opt SEMI decls=decls_opt body=block
      { loc0 $startpos $endpos { kind=Proc; name; params; ret=None; decls; body } }

  | FUNCTION name=ident params=formals_opt ret=ret_type_opt SEMI decls=decls_opt body=block
      { loc0 $startpos $endpos { kind=Func; name; params; ret; decls; body } }

formals_opt:
  | { [] }
  | LPAREN ps=formals RPAREN { ps }

formals:
  | { [] }
  | p=formal { [p] }
  | ps=formals COMMA p=formal { ps @ [p] }

formal:
  | n=ident td=param_type_opt { { p_name=n; p_ty=td } }
  | c=index_letter td=param_type_opt { { p_name=(String.make 1 c); p_ty=td } }

param_type_opt:
  | { None }
  | td=type_desc { Some td }

ret_type_opt:
  | { None }
  | td=type_desc { Some td }

/* ---------- Blocks / statements ---------- */

block:
  | BEGIN ss=stmt_list END opt_term=opt_stmt_term
      { ignore opt_term; ss }

opt_stmt_term:
  | { () } | SEMI { () } | COLON { () }

stmt_list:
  | { [] }
  | xs=stmt_list s=stmt_terminated { xs @ [s] }

stmt_terminated:
  | s=stmt_semi { s }
  | s=stmt_colon { s }

stmt_semi:
  | SEMI { loc0 $startpos $endpos SNull }
  | s=stmt_core SEMI { s }
  | s=if_stmt_semi { s }
  | s=for_stmt_semi { s }
  | s=while_stmt_semi { s }
  | s=case_stmt_semi { s }
 // | error SEMI { loc0 $startpos $endpos (SError None) }

stmt_colon:
  | COLON { loc0 $startpos $endpos SNull }
  | s=stmt_core COLON { s }
  | s=if_stmt_colon { s }
  | s=for_stmt_colon { s }
  | s=while_stmt_colon { s }
  | s=case_stmt_colon { s }
  //| error COLON { loc0 $startpos $endpos (SError None) }

stmt_core:
  | refs=ref_list MINUS e=expr
      { loc0 $startpos $endpos (SAssign (List.map (fun r -> r.node) refs, e)) }
  | r=ref
      { loc0 $startpos $endpos (SCallRef r.node) }
  | GOTO lab=ident { loc0 $startpos $endpos (SGoto lab) }
  | RETURN { loc0 $startpos $endpos SReturn }
  | STOP { loc0 $startpos $endpos SStop }
  | ABORT { loc0 $startpos $endpos SAbort }
  | EXIT { loc0 $startpos $endpos SExit }
  | b=block { loc0 $startpos $endpos (SBlock b) }

ref_list:
  | r=ref { [r] }
  | rs=ref_list COMMA r=ref { rs @ [r] }

ref:
  | base=ident sels=selectors
      { loc0 $startpos $endpos { base; sels } }

selectors:
  | { [] }
  | ss=selectors s=selector { ss @ [s] }

selector:
  | DOT f=ident { SelField f }
  | LPAREN es=expr_list_opt RPAREN { SelSubscript es }
  | DOT v=status_select { SelStatus v }

status_select:
  | _v=LETTER LPAREN n=ident RPAREN { n }

/* IF statement: then-branch ends with ':' before ELSE. */
if_stmt_semi:
  | IF c=expr SEMI th=stmt_semi { loc0 $startpos $endpos (SIf (c, th, None)) }
  | IF c=expr SEMI th=stmt_colon ELSE el=stmt_semi { loc0 $startpos $endpos (SIf (c, th, Some el)) }

if_stmt_colon:
  | IF c=expr SEMI th=stmt_colon { loc0 $startpos $endpos (SIf (c, th, None)) }
  | IF c=expr SEMI th=stmt_colon ELSE el=stmt_colon { loc0 $startpos $endpos (SIf (c, th, Some el)) }

/* FOR loop:
   FOR <control> : <init> [BY expr | THEN expr] [WHILE cond] <stmt>
*/
for_stmt_semi:
  | FOR lc=loop_control COLON init=expr step=loop_step_opt cond=while_opt body=stmt_semi
      { loc0 $startpos $endpos (SFor (lc, init, step, cond, body)) }

for_stmt_colon:
  | FOR lc=loop_control COLON init=expr step=loop_step_opt cond=while_opt body=stmt_colon
      { loc0 $startpos $endpos (SFor (lc, init, step, cond, body)) }

loop_control:
  | n=ident { LCName n }
  | c=index_letter { LCIndex c }

loop_step_opt:
  | { None }
  | BY e=expr { Some (StepBy e) }
  | THEN e=expr { Some (StepThen e) }

while_opt:
  | { None }
  | WHILE e=expr { Some e }

while_stmt_semi:
  | WHILE c=expr body=stmt_semi { loc0 $startpos $endpos (SWhile (c, body)) }

while_stmt_colon:
  | WHILE c=expr body=stmt_colon { loc0 $startpos $endpos (SWhile (c, body)) }

/* CASE statement */
case_stmt_semi:
  | CASE sel=expr SEMI BEGIN alts=case_alts def=case_default_opt END SEMI
      { loc0 $startpos $endpos (SCase (sel, alts, def)) }

case_stmt_colon:
  | CASE sel=expr SEMI BEGIN alts=case_alts def=case_default_opt END COLON
      { loc0 $startpos $endpos (SCase (sel, alts, def)) }

case_alts:
  | { [] }
  | xs=case_alts x=case_alt { xs @ [x] }

case_alt:
  | LPAREN labs=case_labels RPAREN s=stmt_terminated { (labs, s) }

case_default_opt:
  | { None }
  | DEFAULT RPAREN s=stmt_terminated { Some s }

case_labels:
  | l=case_label { [l] }
  | ls=case_labels COMMA l=case_label { ls @ [l] }

case_label:
  | n=INT { CLInt n }
  | a=INT COLON b=INT { CLRange (a,b) }

/* ---------- Expressions ---------- */

expr_list_opt:
  | { [] }
  | xs=expr_list { xs }

expr_list:
  | e=expr { [e] }
  | xs=expr_list COMMA e=expr { xs @ [e] }

expr:
  | e=expr OR r=expr { loc0 $startpos $endpos (EBinary (BOr, e, r)) }
  | e=expr XOR r=expr { loc0 $startpos $endpos (EBinary (BXor, e, r)) }
  | e=expr AND r=expr { loc0 $startpos $endpos (EBinary (BAnd, e, r)) }
  | e=expr EQV r=expr { loc0 $startpos $endpos (EBinary (BEqv, e, r)) }

  | e=expr EQ r=expr { loc0 $startpos $endpos (EBinary (BEq, e, r)) }
  | e=expr NE r=expr { loc0 $startpos $endpos (EBinary (BNe, e, r)) }
  | e=expr LT r=expr { loc0 $startpos $endpos (EBinary (BLt, e, r)) }
  | e=expr LE r=expr { loc0 $startpos $endpos (EBinary (BLe, e, r)) }
  | e=expr GT r=expr { loc0 $startpos $endpos (EBinary (BGt, e, r)) }
  | e=expr GE r=expr { loc0 $startpos $endpos (EBinary (BGe, e, r)) }

  | e=expr SHIFTL r=expr { loc0 $startpos $endpos (EBinary (BShl, e, r)) }
  | e=expr SHIFTR r=expr { loc0 $startpos $endpos (EBinary (BShr, e, r)) }

  | e=expr PLUS r=expr { loc0 $startpos $endpos (EBinary (BAdd, e, r)) }
  | e=expr MINUS r=expr { loc0 $startpos $endpos (EBinary (BSub, e, r)) }
  | e=expr STAR r=expr { loc0 $startpos $endpos (EBinary (BMul, e, r)) }
  | e=expr SLASH r=expr { loc0 $startpos $endpos (EBinary (BDiv, e, r)) }
  | e=expr MOD r=expr { loc0 $startpos $endpos (EBinary (BMod, e, r)) }

  | l=expr POW r=expr { loc0 $startpos $endpos (EBinary (BPow, l, r)) }

  | NOT e=expr { loc0 $startpos $endpos (EUnary (UNot, e)) }
  | MINUS e=expr %prec UMINUS { loc0 $startpos $endpos (EUnary (UNeg, e)) }
  | AT e=expr %prec UAT { loc0 $startpos $endpos (EUnary (UDeref, e)) }

  | a=atom { a }

atom:
  | n=INT { loc0 $startpos $endpos (EInt n) }
  | r=REAL { loc0 $startpos $endpos (EReal r) }
  | c=CHARLIT { loc0 $startpos $endpos (EChar c) }
  | b=BITLIT { let (sz, bits) = b in loc0 $startpos $endpos (EBit (sz, bits)) }
  | TRUE { loc0 $startpos $endpos (EBool true) }
  | FALSE { loc0 $startpos $endpos (EBool false) }
  | NULL { loc0 $startpos $endpos ENull }

  /* status constant V(RED) allowed in expressions */
  | sc=status_const { loc0 $startpos $endpos (EStatusConst sc) }

  /* conversion brackets: treat as wrapper */
  | LCONV e=expr RCONV { loc0 $startpos $endpos (EConv e) }

  /* function call: NAME(args) */
  | f=ident LPAREN args=expr_list_opt RPAREN { loc0 $startpos $endpos (ECall (f, args)) }

  /* variable reference */
  | v=ref { loc0 $startpos $endpos (EVar v.node) }

  | LPAREN e=expr RPAREN { e }
