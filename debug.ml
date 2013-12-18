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
                        | Struct(sname) -> "Struct " 
                        | Err -> "Error"

let dbg_typ ty = 
  (List.fold_left (fun s t -> s ^ (dbg_str_of_typs t)) "" ty);;
   
let dbg_typ_ll ty = 
  (List.fold_left (fun s t -> s ^ " " ^ (dbg_typ t)) "" ty);;

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

let dbg_str_print str = raise (Failure ("Debug msg: \n" ^str));;

let dbg_str_resolve r tabs = match r with
                | Dot  -> p (tabs) ^ "Dot(.)"
                | Ind -> p (tabs) ^ "Ind(->)"

let dbg_str_op o tabs = match o with
                | Add  -> p (tabs) ^ "Add"
                | Sub -> p (tabs) ^ "Sub"
                | Mult -> p (tabs) ^ "Mult"
                | Div -> p (tabs) ^ "Div"
                | Equal -> p (tabs) ^ "Equal"
                | Neq -> p (tabs) ^ "Neq"
                | Less -> p (tabs) ^ "Less"
                | Leq -> p (tabs) ^ "Leq"
                | Greater -> p (tabs) ^ "Greater"
                | Geq -> p (tabs) ^ "Geq";;

let dbg_str_bstmt bstm tabs = match bstm with
                  Atom (atm) -> p tabs ^ "Atom -> \n" 
                    ^ p (tabs+1) ^ dbg_str_Lvar atm (tabs+1)
                | BinEval  (dst, var1, op, var2) -> "BinEval -> \n" 
                    ^ p (tabs+1) ^ "Dst   |" ^ (dbg_str_Lvar dst (tabs+1))^ "\n" 
                    ^ p (tabs+1) ^ "Var1  |" ^ (dbg_str_Lvar var1 (tabs+1) ) ^ "\n" 
                    ^ p (tabs+1) ^ "Op    |" ^ (dbg_str_op op 0)^ "\n" 
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
                | BinRes(ty) -> "BinRes: -> \n"  
                    ^ p (tabs+1) ^ (List.fold_left (fun s t -> s ^
                    (dbg_str_of_typs t)) "" ty) 
                | VarArr(_,_) ->"VarArr: -> \n" (*raise (Failure ("Unexpected:
                  VarArr")) *)
                |Rval (rval) -> " Rval" ^ "\n" 
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

let rec dbg_str_sast_expr sast_expr tabs = match sast_expr with
  | Literal_t(i, t) ->
      dbg_typ t
  | String_t(s, t) ->
      dbg_typ t
  | Addrof_t(e, t) ->
      p (tabs) ^ dbg_typ t
    ^ p (tabs+1) ^ "&"
    ^ dbg_str_sast_expr e (tabs+1)   ^ "\n"
  | Negof_t(e, t) ->
      p (tabs) ^ "-("
    ^ dbg_str_sast_expr e (tabs+1)  ^ ")\n" 
  | ConstCh_t(s, t) ->
       dbg_typ t
  | Id_t(s, t) ->
       dbg_typ t
  | MultiId_t(e1, r, e2, t) -> 
      p (tabs) ^ "MultiId" ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e1 (tabs+1) ^ "\n"
    ^ p (tabs) ^ dbg_str_resolve r  (tabs +1) ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e2 (tabs+1)  ^ "\n" 
  | Pointer_t(e, t) -> 
      p (tabs) ^ dbg_typ t ^ "\n" 
    ^ p (tabs+1) ^ "*"
    ^ dbg_str_sast_expr e (tabs+1) ^ "\n" 
  | Array_t(e1, e2, t) -> 
      p (tabs) ^ dbg_typ t ^ "(" 
    ^ dbg_str_sast_expr e1 (0) ^ "[" ^ dbg_str_sast_expr e2 (0) ^ "])" 
  | Binop_t(e1, o, e2, t) -> 
      p (tabs) ^ dbg_typ t ^ "\n" 
    ^ p (tabs+1) ^ dbg_str_sast_expr e1 (0) ^ " " 
    ^ dbg_str_op o (0)  ^ " "
    ^ dbg_str_sast_expr e2 (0) ^ "\n" 
  | Assign_t(e1, e2, t) -> 
      p (tabs) ^ dbg_typ t ^ "\n" 
    ^ p (tabs+1) ^ dbg_str_sast_expr e1 (0) ^ " = " 
    ^ dbg_str_sast_expr e2 (0) ^ "\n"
  | Call_t(s, e_l, t) ->
      p (tabs) ^ dbg_typ t ^ "\n" 
    ^ p (tabs+1) ^ s ^ "( " 
    ^ (List.fold_left 
      (fun s e -> s ^(dbg_str_sast_expr e (1))) "" e_l) ^ ")\n" 
  | Noexpr_t ->
      p (tabs) ^ "No Expression" ^ "\n";; 
(*
let rec dbg_str_sast_expr sast_expr tabs = match sast_expr with
  | Literal_t(i, t) ->
      p (tabs) ^ "Literal:" ^ string_of_int i 
  | String_t(s, t) ->
      p (tabs) ^ "String: " ^ s 
  | Addrof_t(e, t) ->
      p (tabs) ^ "Addrof:" ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e (tabs+1)   ^ "\n"
  | Negof_t(e, t) ->
      p (tabs) ^ "Neg:"
    ^ p (tabs) ^ dbg_str_sast_expr e (tabs+1)  ^ "\n" 
  | ConstCh_t(s, t) ->
      p (tabs) ^ "Char: " ^ s
  | Id_t(s, t) ->
      p (tabs) ^ "Id: " ^ s
  | MultiId_t(e1, r, e2, t) -> 
      p (tabs) ^ "MultiId" ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e1 (tabs+1) ^ "\n"
    ^ p (tabs) ^ dbg_str_resolve r  (tabs +1) ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e2 (tabs+1)  ^ "\n" 
  | Pointer_t(e, t) -> 
      p (tabs) ^ "Pointer:" ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e (tabs+1) ^ "\n" 
  | Array_t(s, e, t) -> 
      p (tabs) ^ "Array: " ^ s ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e (tabs+1) ^ "\n" 
  | Binop_t(e1, o, e2, t) -> 
      p (tabs) ^ "Binop:" ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e1 (tabs+1) ^ "\n"
    ^ p (tabs) ^ dbg_str_op o (tabs+1) ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e2 (tabs+1) ^ "\n" 
  | Assign_t(e1, e2, t) -> 
      p (tabs) ^ "Assign: " ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e1 (tabs+1) ^ "\n" 
    ^ p (tabs) ^ dbg_str_sast_expr e2 (tabs+1) ^ "\n"
  | Call_t(s, e_l, t) ->
      p (tabs) ^ "Call: " ^ "\n" 
    ^ p (tabs) ^ (List.fold_left 
      (fun s e -> s ^(dbg_str_sast_expr e (tabs+1))) "" e_l) ^ "\n" 
  | Noexpr_t ->
      p (tabs) ^ "No Expression" ^ "\n";; 
*)
let rec dbg_str_sast_stmt sast_stm tabs = match sast_stm with
    Block_t(stmlst) -> "Block -> " 
    ^ (List.fold_left (fun s sast_stm -> s^"\n" ^ (dbg_str_sast_stmt sast_stm
      (tabs+1))) "" stmlst)
  | Expr_t(e) -> "Expr -> \n" 
    ^ (dbg_str_sast_expr e (tabs+1))
  | Return_t(e) -> "Return -> \n"
    ^ (dbg_str_sast_expr e (tabs+1))
  | If_t(e, t_s, f_s) -> "If -> \n"
    ^ "Predicate Expr:\n" ^ (dbg_str_sast_expr e (tabs+1))
    ^ "True Stmt:\n" ^ (dbg_str_sast_stmt t_s (tabs+1))
    ^ "False Stmt:\n" ^ (dbg_str_sast_stmt f_s (tabs+1))
  | For_t(asn, cond, inc, s) -> "For -> \n"
    ^ "Assingment Expr:\n" ^ (dbg_str_sast_expr asn (tabs+1))
    ^ "Conditional Expr:\n" ^ (dbg_str_sast_expr cond (tabs+1))
    ^ "Increment Expr:\n" ^ (dbg_str_sast_expr inc (tabs+1))
    ^ "For Stmt:\n" ^ (dbg_str_sast_stmt s (tabs+1))
  | While_t(e, s) -> "While -> \n"
    ^ "While Expr:\n" ^ (dbg_str_sast_expr e (tabs+1))
    ^ "While Stmt:\n" ^ (dbg_str_sast_stmt s (tabs+1))


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
