open Ast

type atom =
    Lit of int    (*  literal *)
  | Cchar of char
  | Sstr of string * string (* Sstr(name, label) *)
  | Lvar of int * int(* Lvar(offset,size) *)
  | Gvar of string * int (* Globacl var (name,size) *)
  | Pntr of atom * int (* Pntr(addr,size) *)
  | Addr of atom
  | Neg  of atom
  | Debug of string

type bstmt =
    Atom of atom
  | VarArr of atom * atom
  | Rval of atom
  | BinEval of atom * atom * Ast.op * atom (*Binary evaluation *)
  | BinRes of cpitypes list
  | Assgmt of atom * atom
  | Str of string * atom
  | Ldr of string * atom
  | Mov of atom * atom
  | Fcall of string * atom list * atom 
  | Branch of string
  | Predicate of atom * bool * string
  | Label of string

type prog = 
  Fstart of string * atom list * bstmt list * int (*start of a function*)
  | Global of atom list
