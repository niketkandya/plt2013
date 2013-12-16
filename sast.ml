open Ast

module StringMap = Map.Make(String)

type expr_t =
    Var of expr  * cpitypes list
  | Noexpr_t

type stmt_t =
    Block_t of stmt_t list
  | Expr_t of expr_t 
  | Return_t of expr_t
  | If_t of expr_t * stmt_t * stmt_t 
  | For_t of expr_t * expr_t * expr_t * stmt_t
  | While_t of expr_t * stmt_t 

type prog_t = 
  Sast of string * expr_t list * stmt_t list

(*
type prog_t = {
  fname: string;
  formals: expr_t list;
  stmts: stmt_t list;
  prog: Ast.program
}
*)
type var_entry = { 
  offset:int;
  typ: cpitypes list
}

type func_entry = {
  param : var_entry list;
  ret_ty : cpitypes list
}

type struct_entry = {
  size: int;
  memb_index: var_entry StringMap.t
}

(* Symbol table: Information about all the names in scope *)
type envt = {
  function_index : func_entry StringMap.t; (* Index for each function *)
  struct_index   : struct_entry StringMap.t;
  global_index   : var_entry StringMap.t; (* "Address" for global variables *)
  local_index    : var_entry StringMap.t; (* FP offset for args, locals *)
}

