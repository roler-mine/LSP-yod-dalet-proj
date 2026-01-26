(* lib/ast.mli *)

type span = {
  startp : Lexing.position;
  endp : Lexing.position;
}

type 'a located = {
  value : 'a;
  span : span;
}

type ident = string
type ident_loc = ident located

type module_kind =
  | Program
  | Compool
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
  d_name : string;
  d_args : literal list;
}

type use_attr =
  | ARec
  | ARent
  | AStatic
  | AParallel
  | AInline

type type_spec =
  | TAtom of string
  | TNamed of string

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
  | EVar of ident
  | ECall of ident * expr list
  | EUn of string * expr
  | EBin of string * expr * expr
  | EParen of expr

type decl_attr =
  | DStatic
  | DConstant
  | DDefault of expr
  | DLike of ident
  | DPos of expr
  | DRep of expr
  | DOverlay of ident
  | DInstance of ident
  | DRec
  | DRent
  | DInline
  | DParallel

type status_item =
  | SName of ident
  | SVal of ident

type linkage_kind =
  | LDef
  | LRef

type param = {
  pmode : param_mode option;
  pname : ident;
  ptype : type_spec option;
}

type dim =
  | DimStar
  | DimInt of int
  | DimId of ident

type define_rhs =
  | DefString of string
  | DefExpr of expr

type linkage_target =
  | LName of ident
  | LProcSig of ident * param list
  | LFunSig of ident * param list * type_spec option

type decl =
  | DItem of {
      names : ident list;
      typ : type_spec option;
      attrs : decl_attr list;
    }
  | DTable of {
      name : ident;
      dims : dim list;
      typ : type_spec option;
      attrs : decl_attr list;
      body : decl list;
    }
  | DBlock of {
      name : ident;
      attrs : decl_attr list;
      body : decl list;
    }
  | DTypeStatus of {
      name : ident;
      items : status_item list;
    }
  | DTypeAlias of {
      name : ident;
      target : type_spec;
    }
  | DOverlayDecl of ident
  | DDefine of {
      name : ident;
      rhs : define_rhs;
    }
  | DLinkage of {
      kind : linkage_kind;
      target : linkage_target;
    }
  | DLabelDecl of ident list

type lvalue =
  | LVar of ident
  | LIndex of ident * expr list

type stmt =
  | SLabel of ident * stmt
  | SAssign of lvalue * expr
  | SCall of ident * expr list
  | SIf of expr * stmt * stmt option
  | SIfElsif of (expr * stmt) list * stmt option
  | SWhile of expr * stmt
  | SForTo of {
      var : ident;
      start_ : expr;
      stop_ : expr;
      step : expr option;
      body : stmt;
    }
  | SForWhile of {
      var : ident;
      start_ : expr;
      step : expr option;
      cond : expr;
      body : stmt;
    }
  | SCase of {
      expr : expr;
      clauses : (expr list * stmt list) list;
      otherwise_ : stmt list option;
    }
  | SGoto of ident
  | SReturn of expr option
  | SExit of ident option
  | SStop of expr option
  | SAbort of expr option
  | SFallthru
  | SBlock of stmt list
  | SNoop
  | SError

type proc_kind =
  | PProc
  | PFunction

type proc = {
  pkind : proc_kind;
  pr_name : ident;
  params : param list;
  rettype : type_spec option;
  pattrs : use_attr list;
  directives : directive list;
  body : stmt list option;
}

type module_ = {
  kind : module_kind;
  name : ident option;
  directives : directive list;
  attrs : use_attr list;
  decls : decl list;
  stmts : stmt list;
  procs : proc list;
}

(* What the parser returns *)
type compilation_unit = module_
