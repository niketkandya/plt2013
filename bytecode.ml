open Ast

type atom =
    Lit of int    (*  literal *)
  | Cchar of char
  | Lvar of int * int * int(* Lvar(index,size,cnt) *)
  | Gvar of string * int (* Globacl var (name,size) *)
  | Pntr of atom
  | Addr of atom

type bstmt =
    Atom of atom
  | Rval of atom
  | BinEval of atom * atom * Ast.op * atom (*Binary evaluation *)
  | Assgmt of atom * atom
  | Str of string * atom
  | Ldr of string * atom
  | Mov of atom * atom
  | Fcall of string * atom list * atom 
  | Branch of string
  | Predicate of atom * bool * string
  | Label of string

type prog = 
  Fstart of string * atom list * atom list * bstmt list * atom list (*start of a function*)
  | Global of atom list
