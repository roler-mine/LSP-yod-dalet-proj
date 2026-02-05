(* lib/ast.mli *)

type span = {
  sp_start : Lexing.position;
  sp_end : Lexing.position;
}

type 'a located = {
  loc_value : 'a;
  loc_span : span;
}

val mk_span : Lexing.position -> Lexing.position -> span
val mk_loc : 'a -> Lexing.position -> Lexing.position -> 'a located
val unloc : 'a located -> 'a
val span_of_loc : 'a located -> span

type ident = string
type ident_loc = ident located

type module_kind =
  | Program
  | Compool
  | Icompool
  | Proc_module
  | Function_module
  | Unknown

type literal =
  | LInt of int
  | LFloat of float
  | LString of string
  | LBead of int * string
  | LNull
  | LBool of bool

type directive = {
  dir_name : string;
  dir_args : literal list;
}

type use_attr =
  | ARec
  | ARent
  | AStatic
  | AParallel
  | AInline

type type_spec =
  | TAtom of string
  | TNamed of ident_loc

type param_mode =
  | ByRef
  | ByVal
  | ByRes

type expr =
  | EInt of int
  | EFloat of float
  | EString of string
  | EBead of int * string
  | ENull
  | EBool of bool
  | EVar of ident_loc
  | ECall of ident_loc * expr list
  | EUn of string * expr
  | EBin of string * expr * expr
  | EParen of expr

type decl_attr =
  | DStatic
  | DConstant
  | DDefault of expr
  | DLike of ident_loc
  | DPos of expr
  | DRep of expr
  | DOverlay of ident_loc
  | DInstance of ident_loc
  | DRec
  | DRent
  | DInline
  | DParallel

type status_item =
  | SName of ident_loc
  | SVal of ident_loc

type linkage_kind =
  | LDef
  | LRef

type param = {
  prm_mode : param_mode option;
  prm_name : ident_loc;
  prm_type : type_spec option;
}

type dim =
  | DimStar
  | DimInt of int
  | DimId of ident_loc

type define_rhs =
  | DefString of string
  | DefExpr of expr

type linkage_target =
  | LName of ident_loc
  | LProcSig of ident_loc * param list
  | LFunSig of ident_loc * param list * type_spec option

type decl =
  | DItem of ident_loc list * type_spec option * decl_attr list
  | DTable of ident_loc * dim list * type_spec option * decl_attr list * decl list
  | DBlock of ident_loc * decl_attr list * decl list
  | DTypeStatus of ident_loc * status_item list
  | DTypeAlias of ident_loc * type_spec
  | DOverlayDecl of ident_loc
  | DDefine of ident_loc * define_rhs
  | DLinkage of linkage_kind * linkage_target
  | DLabelDecl of ident_loc list

type lvalue =
  | LVar of ident_loc
  | LIndex of ident_loc * expr list

type stmt =
  | SLabel of ident_loc * stmt
  | SAssign of lvalue * expr
  | SCall of ident_loc * expr list
  | SIf of expr * stmt * stmt option
  | SIfElsif of (expr * stmt) list * stmt option
  | SWhile of expr * stmt
  | SForTo of ident_loc * expr * expr * expr option * stmt
  | SForWhile of ident_loc * expr * expr option * expr * stmt
  | SCase of expr * (expr list * stmt list) list * stmt list option
  | SGoto of ident_loc
  | SReturn of expr option
  | SExit of ident_loc option
  | SStop of expr option
  | SAbort of expr option
  | SFallthru
  | SBlock of stmt list
  | SNoop
  | SError of span

type proc_kind =
  | PProc
  | PFunction

type proc = {
  pr_kind : proc_kind;
  pr_name : ident_loc;
  pr_span : span;
  pr_params : param list;
  pr_rettype : type_spec option;
  pr_attrs : use_attr list;
  pr_directives : directive list;
  pr_body : stmt list option;
}

type module_ = {
  m_kind : module_kind;
  m_name : ident_loc option;
  m_directives : directive list;
  m_attrs : use_attr list;
  m_decls : decl list;
  m_stmts : stmt list;
  m_procs : proc list;
}

type compilation_unit = module_
