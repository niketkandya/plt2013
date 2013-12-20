type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
Lor | Land
type resolve = Dot | Ind

type expr =
    Literal of int
  | String of string
  | Addrof of expr
  | Negof of expr
  | ConstCh of string
  | Id of string
  | MultiId of expr * resolve * expr
  | Pointer of expr
  | Array of expr * expr
  | Binop of expr * op * expr
  | Assign of expr * expr
  | Call of string * expr list
  | Null
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type cpitypes = Void | Int | Char | Ptr | Arr of expr | Struct of string | Err 

type var_decl = {
  vname: string;
  vtype: cpitypes list;
}

type struct_decl = {
  sname: string;
  smembers: var_decl list
}

type func_decl = {
  fname : string;
  formals : var_decl list;
  locals : var_decl list;
  body : stmt list;
  ret : cpitypes list
}

type program = {
  sdecls : struct_decl list;
  gdecls : var_decl list;
  fdecls : func_decl list
}

