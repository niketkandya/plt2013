open Ast
open Sast
open Bytecode

let rec p tab_count = if (tab_count = 0 ) then "" else "\t" ^ p (tab_count-1);; 

let dbg_str_of_typs typ = match typ with 
                        Void -> "Void" 
                        | Char -> "Char"
                        | Int -> "Int"
                        | Ptr -> "Ptr" 
                        | Arr(sz) -> "Arr" 
                        | Struct(sname) -> "Struct " ^ sname
                        | _ -> raise (Failure ("Requesting size of wrong"));;

let rec dbg_str_Lvar lvar tabs = match lvar with
                 Lvar(off,sz) -> "Lvar Offset: " ^ string_of_int off ^
                                " Size: " ^ string_of_int sz 
                | Lit (i) -> "Literal: " ^ string_of_int i
                | Cchar (ch) -> "Const char :" ^ String.make 1 ch
                | Sstr (str, label) -> "String: " ^ str ^ " Label: " ^ label
                | Gvar (_,_) -> "Globals: need implementation" (* Global var (name,size) *)
                | Pntr (atm, sz) ->
                    "Pointer: " ^
                    "\n" ^ p (tabs+2) ^ "Value | " ^ (dbg_str_Lvar atm (tabs+1)) ^
                    "\n" ^ p (tabs+2) ^ "Size  | " ^ (string_of_int sz)  
                | Addr (atm)-> 
                    "Address: " ^
                    "\n" ^ p (tabs+2) ^ "Value | " ^ (dbg_str_Lvar atm (tabs+1)) 
                | Neg (atm)-> 
                    "\n" ^ p (tabs+2) ^ "Negative: \n" ^
                    "\n" ^ p (tabs+2) ^ "Value | " ^ (dbg_str_Lvar atm (tabs+1))
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

let dbg_str_bstmt bstm tabs = match bstm with
                  Atom (atm) -> p tabs ^ "Atom -> \n" 
                    ^ p (tabs+1) ^ dbg_str_Lvar atm (tabs+1)
                | BinEval  (dst, var1, op, var2) -> "BinEval -> \n" 
                    ^ p (tabs+1) ^ "Dst   |" ^ (dbg_str_Lvar dst (tabs+1))^ "\n" 
                    ^ p (tabs+1) ^ "Var1  |" ^ (dbg_str_Lvar var1 (tabs+1) ) ^ "\n" 
                    ^ p (tabs+1) ^ "Op    |" ^ (dbg_str_op op)^ "\n" 
                    ^ p (tabs+1) ^ "Var2  |" ^ (dbg_str_Lvar var2 (tabs+1)) 
                | Fcall (fname, args,ret ) -> "Fcall -> \n"   
                    ^ p (tabs+1) ^ "fname |" ^ fname ^ "\n"
                    ^ p (tabs+1) ^ "args  |" ^
                      (List.fold_left 
                      (fun s t -> s ^ " " ^ (dbg_str_Lvar t (tabs+1))) "" args) ^"\n" 
                    ^ p (tabs+1) ^ "ret   |" ^ (dbg_str_Lvar ret (tabs+1)) 
                | Assgmt (dst, src) ->  "Assignment -> \n"  
                    ^ p (tabs+1) ^ "dst   |" ^ (dbg_str_Lvar dst (tabs +1)) ^ "\n" 
                    ^ p (tabs+1) ^ "src   |" ^ (dbg_str_Lvar src (tabs +1)) 
                | Label (a)-> "Label -> \n" 
                    ^ p (tabs+1) ^ a 
                | Predicate (pred, b,label )-> "Predicate -> \n"  
                    ^ p (tabs+1) ^ "Pred  |" ^ (dbg_str_Lvar pred (tabs+1)) ^"\n" 
                    ^ p (tabs+1) ^ "Label |" ^ label 
                | Branch(b)-> "Branch -> \n"  
                    ^ p (tabs+1) ^ b 
                | Mov (_, _)-> raise (Failure ("Unexpected: Mov"))
                | Ldr (_, _)-> raise (Failure ("Unexpected: Ldr"))
                | Str (_, _)-> raise (Failure ("Unexpected: Str"))
                | BinRes(ty) -> "BinRes -> \n"  
                    ^ p (tabs+1) ^ (List.fold_left (fun s t -> s ^
                    (dbg_str_of_typs t)) "" ty) 
                |Rval (rval) -> "Rval -> \n"  
                    ^ p (tabs+1) ^ "Rvalue |" ^ (dbg_str_Lvar rval (tabs+1)) ;;

let dbg_str_bstmlist lst fname sz = fname ^ " stack size = " ^ (string_of_int
sz) ^ "\n" ^ 
      (List.fold_left (fun s bstm -> s^"\n" ^ (dbg_str_bstmt bstm 0)) "" lst);;


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
      in dbg_str_proglst prog;;

let rec p tab_count = if (tab_count = 0 ) then "" else "\t" ^ p (tab_count-1);; 

let dbg_str_sast_expr sast_expr tabs = match sast_expr with
    Var(e, typlst) ->
      p (tabs+1) ^ (List.fold_left 
      (fun s t -> s ^(dbg_str_of_typs t)) "" typlst) 
  | Noexpr_t -> 
      p (tabs+1) ^ "No Expression";; 


let rec dbg_str_sast_stmt sast_stm tabs = match sast_stm with
    Block_t(stmlst) -> "Block -> " 
    ^ (List.fold_left (fun s sast_stm -> s^"\n" ^ (dbg_str_sast_stmt sast_stm
      (tabs+1))) "" stmlst)
  | Expr_t(e) -> "Expr -> \n" 
    ^ (dbg_str_sast_expr e (tabs+1))
  | Return_t(e) -> "Return -> \n"
    ^ (dbg_str_sast_expr e (tabs+1))
  | If_t(e, t_s, f_s) -> "If -> \n"
    ^ "Predicate Expr:    " ^ (dbg_str_sast_expr e (tabs+1))
    ^ "True Stmt:         " ^ (dbg_str_sast_stmt t_s (tabs+1))
    ^ "False Stmt:        " ^ (dbg_str_sast_stmt f_s (tabs+1))
  | For_t(asn, cond, inc, s) -> "For -> \n"
    ^ "Assingment Expr:   " ^ (dbg_str_sast_expr asn (tabs+1))
    ^ "Conditional Expr:  " ^ (dbg_str_sast_expr cond (tabs+1))
    ^ "Increment Expr:    " ^ (dbg_str_sast_expr inc (tabs+1))
    ^ "For Stmt:          " ^ (dbg_str_sast_stmt s (tabs+1))
  | While_t(e, s) -> "While -> \n"
    ^ "While Expr:        " ^ (dbg_str_sast_expr e (tabs+1))
    ^ "While Stmt:        " ^ (dbg_str_sast_stmt s (tabs+1))


let dbg_str_sast_stmlist lst name tabs = name ^  
      (List.fold_left (fun s sast_stm -> s^"\n" ^ (dbg_str_sast_stmt sast_stm
      tabs)) "" lst);;

let dbg_str_sast sast =
  let get_sast_lst(prog, s) = s in
  let sast_lst = get_sast_lst(sast) in
  let rec dbg_str_sastlst = 
    function 
      [] -> "" 
    | hd :: tl ->
      (match hd with 
      Sast (fname, formals, body) ->
        dbg_str_sast_stmlist body ("Function: " ^ fname ^ "\n") 0)
      ^ (dbg_str_sastlst tl)
    in dbg_str_sastlst sast_lst
