open Ast
open Bytecode

let dbg_str_of_typs typ = match typ with 
                        Void -> "Void" 
                        | Char -> "Char"
                        | Int -> "Int"
                        | Ptr -> "Ptr" 
                        | Arr(sz) -> "Arr" 
                        | Struct(sname) -> "Struct " ^ sname
                        | _ -> raise (Failure ("Requesting size of wrong"));;

let rec dbg_str_Lvar lvar = match lvar with
                        Lvar(off,sz) -> "Lvar Offset: " ^ string_of_int off ^
                                " Size: " ^ string_of_int sz
                        |  Lit (i) -> "Literal: " ^ string_of_int i
                        | Cchar (ch) -> "Const char :" ^ String.make 1 ch
                        | Sstr (str, label) -> "String: " ^ str ^ " Label: " ^
                                              label
                        | Gvar (_,_) -> "Globals: need implementation" (* Global var (name,size) *)
                        | Pntr (atm, sz) -> "Pointer: \n" ^
                                "value| " ^ (dbg_str_Lvar atm) ^
                                "Size| " ^ (string_of_int sz)
                        | Addr (atm)-> "Address: \n" ^
                                "value| " ^ (dbg_str_Lvar atm) ^"\n"
                        | Neg (atm)-> "Negative: \n" ^
                                "value| " ^ (dbg_str_Lvar atm) ^"\n"
                        | Debug(str) -> str
                        | _ -> raise (Failure ("Needs Implementation"));;

let dbg_str_print str = raise (Failure ("Debug msg: \n" ^str));;
let dbg_str_op o = match o with
                Add  -> "Add"
                | Sub -> "Sub"
                | Mult -> "Mult"
                | Div -> "Div"
                | Equal -> "Equal"
                | Neq -> "Neq"
                | Less -> "Less"
                | Leq -> "Leq"
                | Greater -> "Greater"
                | Geq -> "Geq";;

let dbg_str_bstmt bstm = match bstm with
                Atom (atm) -> "Atom: \n\t"^ dbg_str_Lvar atm
                | BinEval  (dst, var1, op, var2) -> "BinEval -> \n" ^
                                "Dst |" ^ (dbg_str_Lvar dst)^ "\n" ^
                                "Var1 |" ^ (dbg_str_Lvar var1) ^ "\n" ^
                                "Op |" ^ (dbg_str_op op)^ "\n" ^
                                "Var2 |" ^ (dbg_str_Lvar var2)^ "\n"
                | Fcall (fname, args,ret ) -> "Fcall: "  ^ "\n\t" ^
                                "fname |" ^ fname ^ "\n\t" ^
                                "args  |" ^
                                (List.fold_left 
                                (fun s t -> s ^ " " ^ (dbg_str_Lvar t)) "" args)
                                ^ "\n\t" ^
                                "ret   |" ^ (dbg_str_Lvar ret) ^ "\n"
                | Assgmt (dst, src) ->  "Assignment: " ^ "\n" ^
                                "dst |" ^ (dbg_str_Lvar dst)^ "\n" ^
                                "src |" ^ (dbg_str_Lvar src)^ "\n"
                | Label (a)-> a                
                | Predicate (pred, b,label )-> "Predicate: " ^ "\n" ^
                                "Pred |" ^ (dbg_str_Lvar pred) ^"\n" ^
                                "Label |" ^ label ^ "\n"
                | Branch(b)-> "Branch: " ^ b ^ "\n"
                | Mov (_, _)-> raise (Failure ("Unexpected: Mov"))
                | Ldr (_, _)-> raise (Failure ("Unexpected: Ldr"))
                | Str (_, _)-> raise (Failure ("Unexpected: Str"))
                | BinRes(ty) -> "BinRes: " ^ 
                        (List.fold_left (fun s t -> s ^ (dbg_str_of_typs t)) "" ty)
                |Rval (rval) -> " Rval" ^ "\n" ^
                                "Rvalue | " ^ (dbg_str_Lvar rval) ^ "\n";;

let dbg_str_bstmlist lst fname sz = fname ^ " stack size = " ^ (string_of_int
sz) ^ "\n" ^ 
      (List.fold_left (fun s bstm -> s^"\n" ^ (dbg_str_bstmt bstm)) "" lst);;


let dbg_str_program prog = 
      let rec dbg_str_proglst = 
              function 
              [] -> "" 
              | hd :: tl ->
                 (match hd with
                     Global (atmlst) -> "" (* dbg_print (List.hd atmlst) (*TODO: Global
                     functions code *)*)
                   | Fstart (fname, formals, body, stack_sz) ->
                     dbg_str_bstmlist body fname stack_sz
                 ) ^ (dbg_str_proglst tl)
      in dbg_str_proglst prog
