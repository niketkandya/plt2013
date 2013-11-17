open Ast

type atom =
    Lit of int    (*  literal *)
  | Cchar of char
  | Sstr of string
  | Lvar of int * int(* Lvar(offset,cnt) *)
  | Gvar of string * int (* Globacl var (name,size) *)
  | Pntr of atom * int
  | Addr of atom
  | Debug of string
  | Array of atom * atom

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
