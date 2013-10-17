open Ast

type atom =
    Lit of int    (* Push a literal *)
  | Var of int           (* Discard a value *)

type bstmt =
   Atom of atom
  |Fstart of int * int (*start of a function*)
  | Fexit               (*Restore registers values at exit*)
  | BinEval of atom * atom * Ast.op * atom (*Binary evaluation *)
  | Str of string * atom
  | Ldr of string * atom
  | Mov of atom * atom
  | Fcall of atom * atom list
  | Uncond_br of string
  | Cond_br of string
