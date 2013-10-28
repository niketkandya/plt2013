open Ast

type atom =
    Lit of int    (*  literal *)
  | Lvar of int * int (* Local Var(variable_index, variable_size) *)
  | Gvar of string * int (* Globacl var (name,size) *)

type bstmt =
    Atom of atom
  | Fstart of string * int * int (*start of a function*)
  | Fexit               (*Restore registers values at exit*)
  | Rval of atom
  | BinEval of atom * atom * Ast.op * atom (*Binary evaluation *)
  | Assgmt of atom * atom
  | Str of string * atom
  | Ldr of string * atom
  | Mov of atom * atom
  | Fcall of string * atom list * atom 
  | Branch of string
  | Predicate of atom * string
  | Label of string
