(* server/lib/parser.mly *)

%{
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
%}

(* ---- Tokens ---- *)

%token ITEM TABLE PROC END IF THEN ELSE WHILE DO RETURN

%token LPAR RPAR COMMA SEMI COLON ASSIGN
%token PLUS MINUS STAR SLASH
%token LE GE NE EQ LT GT

%token <string> IDENT
%token <int> INT
%token <float> REAL
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL

%token EOF

(* ---- Precedence (for expressions) ---- *)

%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH
%right UMINUS

(* ---- Entry point ---- *)

%start <Ast.compilation_unit> compilation_unit

%%

(* =========================
   Top-level / declarations
   ========================= *)

compilation_unit:
  ds=decl_list EOF
  { { Ast.decls = ds } }
;

decl_list:
  | (* empty *)                 { [] }
  | d=decl ds=decl_list         { d :: ds }
;

decl:
  | d=item_decl                 { d }
  | d=table_decl                { d }
  | d=proc_decl                 { d }
;

item_decl:
  | ITEM name=ident ty=opt_type init=opt_init SEMI
    {
      let sp = $startpos and ep = $endpos in
      let it : Ast.item_decl = { Ast.name = name; ty; init } in
      loc_decl sp ep (Ast.Item it)
    }
;

table_decl:
  | TABLE name=ident elem=opt_type size=opt_size SEMI
    {
      let sp = $startpos and ep = $endpos in
      let tb : Ast.table_decl = { Ast.name = name; elem_ty = elem; size } in
      loc_decl sp ep (Ast.Table tb)
    }
;

opt_size:
  | (* empty *)                 { None }
  | LPAR n=INT RPAR             { Some n }
;

proc_decl:
  | PROC name=ident LPAR ps=param_list_opt RPAR ret=opt_type body=stmt_list END SEMI
    {
      let sp = $startpos and ep = $endpos in
      let pr : Ast.proc_decl =
        { Ast.name = name
        ; params = ps
        ; returns = ret
        ; body
        }
      in
      loc_decl sp ep (Ast.Proc pr)
    }
;

param_list_opt:
  | (* empty *)                 { [] }
  | ps=param_list               { ps }
;

param_list:
  | p=param                      { [p] }
  | p=param COMMA ps=param_list   { p :: ps }
;

param:
  | id=ident ty=opt_type          { (id, ty) }
;

opt_type:
  | (* empty *)                  { None }
  | COLON t=type_expr_l          { Some t }
;

type_expr_l:
  | id=IDENT
    {
      let sp = $startpos(id) and ep = $endpos(id) in
      match scalar_of_name id with
      | Some k -> loc sp ep (Ast.Scalar k)
      | None ->
          let nm = mk_ident sp ep id in
          loc sp ep (Ast.Named nm)
    }
;

opt_init:
  | (* empty *)                  { None }
  | ASSIGN e=expr                { Some e }
;

(* ================
   Statements
   ================ *)

stmt_list:
  | (* empty *)                  { [] }
  | s=stmt ss=stmt_list          { s :: ss }
;

stmt:
  | s=assign_stmt                { s }
  | s=call_stmt                  { s }
  | s=if_stmt                    { s }
  | s=while_stmt                 { s }
  | s=return_stmt                { s }
;

assign_stmt:
  | id=ident ASSIGN e=expr SEMI
    {
      let sp = $startpos and ep = $endpos in
      loc_stmt sp ep (Ast.Assign (id, e))
    }
;

call_stmt:
  | f=ident LPAR args=arg_list_opt RPAR SEMI
    {
      let sp = $startpos and ep = $endpos in
      loc_stmt sp ep (Ast.CallStmt (f, args))
    }
;

if_stmt:
  | IF c=expr THEN t=stmt_list e=else_opt END SEMI
    {
      let sp = $startpos and ep = $endpos in
      loc_stmt sp ep (Ast.If (c, t, e))
    }
;

else_opt:
  | (* empty *)                  { [] }
  | ELSE s=stmt_list             { s }
;

while_stmt:
  | WHILE c=expr DO body=stmt_list END SEMI
    {
      let sp = $startpos and ep = $endpos in
      loc_stmt sp ep (Ast.While (c, body))
    }
;

return_stmt:
  | RETURN SEMI
    {
      let sp = $startpos and ep = $endpos in
      loc_stmt sp ep (Ast.Return None)
    }
  | RETURN e=expr SEMI
    {
      let sp = $startpos and ep = $endpos in
      loc_stmt sp ep (Ast.Return (Some e))
    }
;

(* ================
   Expressions
   ================ *)

expr:
  | c=cmp                        { c }
;

cmp:
  | a=add op=cmpop b=add
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Binary (op, a, b))
    }
  | a=add                        { a }
;

cmpop:
  | EQ                           { Ast.Eq }
  | NE                           { Ast.Ne }
  | LT                           { Ast.Lt }
  | LE                           { Ast.Le }
  | GT                           { Ast.Gt }
  | GE                           { Ast.Ge }
;

add:
  | a=add PLUS b=mul
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Binary (Ast.Add, a, b))
    }
  | a=add MINUS b=mul
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Binary (Ast.Sub, a, b))
    }
  | m=mul                        { m }
;

mul:
  | a=mul STAR b=unary
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Binary (Ast.Mul, a, b))
    }
  | a=mul SLASH b=unary
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Binary (Ast.Div, a, b))
    }
  | u=unary                      { u }
;

unary:
  | MINUS e=unary %prec UMINUS
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Unary (Ast.Neg, e))
    }
  | p=primary                    { p }
;

primary:
  | i=INT
    {
      let sp = $startpos(i) and ep = $endpos(i) in
      loc_expr sp ep (Ast.IntLit i)
    }
  | r=REAL
    {
      let sp = $startpos(r) and ep = $endpos(r) in
      loc_expr sp ep (Ast.RealLit r)
    }
  | s=STRING
    {
      let sp = $startpos(s) and ep = $endpos(s) in
      loc_expr sp ep (Ast.StringLit s)
    }
  | c=CHAR
    {
      let sp = $startpos(c) and ep = $endpos(c) in
      loc_expr sp ep (Ast.CharLit c)
    }
  | b=BOOL
    {
      let sp = $startpos(b) and ep = $endpos(b) in
      loc_expr sp ep (Ast.BoolLit b)
    }

  | f=ident LPAR args=arg_list_opt RPAR
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Call (f, args))
    }
  | id=ident
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.Var id)
    }

  | LPAR e=expr RPAR
    {
      let sp = $startpos and ep = $endpos in
      loc_expr sp ep (Ast.loc_value e)
    }
;

arg_list_opt:
  | (* empty *)                  { [] }
  | es=arg_list                  { es }
;

arg_list:
  | e=expr                       { [e] }
  | e=expr COMMA es=arg_list     { e :: es }
;

(* ================
   Identifiers
   ================ *)

ident:
  | s=IDENT
    {
      let sp = $startpos(s) and ep = $endpos(s) in
      mk_ident sp ep s
    }
;
