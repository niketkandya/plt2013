open Ast
open Bytecode

module IntMap = Map.Make(
struct type t = int
let compare = compare end
)

module StringMap = Map.Make(String)

type byc_gvar_entry = { (*TODO: add more require elements*)
        label: string;
}

type byc_env = {
        global_index: byc_gvar_entry StringMap.t;
}


let execute_prog program = 
        let p asm = "\t " ^ asm ^ "\n"
        and size_stmfd = 4 (* Total size pushed using stmfd -4 *) 
        and align_size = 4 (*Alignment of the stack *)
        in
(* lenv is of type byc_local_env *)
let tmp_idx = ref 0 and tmp_fp = ref 0 in
let f i = float_of_int i 
in
let dbg_print var = match var with
        Lvar(i,s,c) -> "Index: " ^ string_of_int i ^
                        "Size: " ^ string_of_int s ^
                        "Count: " ^ string_of_int c
        | Debug(s) -> "Debug" ^ s ^"\n"
        | _ -> "IMPLEMENT"
in
        let size_of_lvar l = match l with
                Lvar(i,s,c)-> s
                   | Gvar(n,s)-> s
                   | _ -> raise (Failure("Cannot generate size"))
in
let function_code_gen env fname formals body temps =
        let branch lb = p ("b " ^ lb) in
        let gen_label lbl = lbl ^ ":" ^ "\n" in
        let exit_label = fname ^ "_exit" in
        (* Note register r4 will be left as a temporary register 
         * so that anybody can use .eg in gen_ldr_str_code *)
        let rec gen_ldr_str_code oper sym reg atm = 
                let pre sz = if sz != 0 then(  oper ^ (if sz = 1 then "b" else "")
                        ^" "^reg ^", ") else "" in 
                match atm with
          Lit (i) -> p ( (pre 4)  ^ sym ^ string_of_int i)
        | Cchar (ch) -> p ((pre 1) ^ sym ^ string_of_int (int_of_char ch))
        | Lvar (idx, sz, cnt) -> if sz = 0 then "" else ( p ( (pre sz) ^ "[fp,#-" ^ string_of_int
                                 (idx_to_offset idx) ^"]"))
        | Gvar (vname, sz) -> "" (*TODO *)
        | Addr (vnm) -> (match vnm with
                Lvar(idx,sz,cnt) -> (match sz with
                  0 -> ""
                | _ -> 
                        p ("sub " ^reg^", fp,#" ^ 
                        string_of_int (idx_to_offset idx) ))
                |Gvar(vname,sz) -> "" (*TODO: Globals*)
                   | _ -> raise(Failure ("Lvars only should be passed")))
        | Pntr (vnm,bsz) -> (match vnm with
                Lvar(idx,sz,cnt) ->( match sz with
                        0 -> ""
                        | _ ->
                        (gen_ldr_str_code "ldr" "=" "r4" vnm) ^
                        p ((pre bsz) ^ "[r4,#0]"))
                |Gvar(vname,sz) -> "" (*TODO: Globals*)
                | _ -> raise(Failure ("Lvars only should be passed")))
        | Array(arr,ind) -> (load_code reg ind) ^
                            (load_code "r4" arr) ^
                            p ("mla "^reg^","^reg^"#"^ (size_of_lvar arr
                            )^",r4") ^


                        
                        (load_code "r0" Addr(arr))
                                ^ (load_code "r1" ind)
                                ^ p ("add r0,r0,r1")
                                ^ p ((pre 
       in
       let load_code reg var = (* load variable var to register reg *)
                gen_ldr_str_code "ldr" "=" reg var
       and store_code reg var =
                gen_ldr_str_code "str" "#" reg var in
let bin_eval dst var1 op var2 = 
        let oper = (match op with
        Add -> p "adds r3, r0, r1"
      | Sub -> p "subs r3, r0, r1" 
      | Mult ->p "muls r3, r0, r1"
      | Div -> p "bl __aeabi_idiv" ^
               p "mov r3, r0"
      | Equal ->
               p "cmp r0, r1" ^
               p "moveq r3,#1" ^
               p "movne r3,#0" ^
               p "uxtb r3,r3"(*TODO-check the need*)
      | Neq -> 
               p "cmp r0, r1" ^ 
               p "moveq r3,#0" ^ 
               p "movne r3,#1" ^ 
               p "uxtb r3,r3"
      | Less -> 
               p "cmp r0, r1" ^
               p "movlt r3,#1" ^
               p "movge r3,#0" ^
               p "uxtb r3,r3"
      | Leq -> 
               p "cmp r0, r1" ^
               p "movle r3,#1" ^
               p "movgt r3,#0"^
               p "uxtb r3,r3"
      | Greater -> 
               p "cmp r0, r1"^
               p "movgt r3,#1"^
               p "movle r3,#0"^
               p "uxtb r3,r3"
      | Geq -> 
               p "cmp r0, r1"^
               p "movge r3,#1"^
               p "movlt r3,#0"^
               p "uxtb r3,r3"
        )
in (load_code "r0" var1) ^ (load_code "r1" var2) ^ oper ^ (store_code "r3" dst)
in
let function_call fname args ret=
              let rec fcall i = function
                      []-> ""
                |hd::tl -> (load_code ("r" ^ string_of_int i) hd ) ^ (fcall (i+1) tl)
               in fcall 0 args ^ 
               ("\n\t bl  " ^ fname ^ "\n" ) ^
               (store_code "r0" ret)
               (* TODO implement properly *)
in
let predicate cond jmpontrue label = 
        let brn = if jmpontrue then "\t beq "
                    else "\t bne "
        in (load_code "r0" cond) ^
                "\t cmp r0,#1\n" ^
                brn ^ label ^ "\n"
        in
let asm_code_gen = function
   Atom (atm) -> ""
  | BinEval  (dst, var1, op, var2) -> bin_eval dst var1 op var2
  | Assgmt (dst, src) -> (load_code "r0" src) ^ (store_code "r0" dst)
  | Str (reg , atm ) ->  "Store"
  | Ldr (reg ,atm ) ->  "Load"
  | Mov (dst, src) ->  "Move"
  | Fcall (fname, args,ret) ->  function_call fname args ret  (*Whenever a function
          is called*) (*TODO do something for the ret value*)
  | Rval var -> (load_code "r0" var) ^ (branch exit_label)
  | Branch label -> branch label
  | Label label -> gen_label label
  | Predicate (cond,jmpontrue,label) -> predicate cond jmpontrue label
in
let non_atom lst = (List.filter (fun ele -> match ele with 
                Atom (atm ) -> false
                | _ -> true) lst)
in
let func_start_code =
            (* Code generation for function *)
        ".global " ^ fname ^ "\n" ^
            fname ^ ":\n" ^
                   (p "stmfd sp!, {fp, lr}") ^
                   p ("add fp, sp,#"^ string_of_int size_stmfd)  ^
                  (* List.fold_left (fun s v->s ^ "\n" ^ (dbg_print v)) "" temps
                   ^*)
                   p ("sub sp, sp,#" ^ string_of_int (env.local_data.mfp - size_stmfd)) ^ 
                   let rec formals_push_code i = if i < 0 then "" else 
                            (formals_push_code (i-1)) ^ 
                            (store_code ("r" ^ string_of_int i) (List.nth formals i))
                    in formals_push_code ((List.length formals) -1)
                    (* TODO : ifjthe variable size is 1 byte, strb should be
                     * used instead and the var_size should be updated
                     * accordingly *)
        and func_end_code = (gen_label exit_label) ^
                p "sub sp, fp, #4" ^
                       p "ldmfd sp!, {fp, pc}" ^ "\n"
        in func_start_code ^
        (List.fold_left 
                (fun str lst -> str ^ (asm_code_gen lst)) 
                "" (non_atom body))
        ^ func_end_code
in
let env = {
        global_index = StringMap.empty;
         }
in let rec print_program = function 
        [] -> "" 
        | hd :: tl ->
           (match hd with
             Global (atmlst) -> "" (*TODO: Global functions code *)
             | Fstart (fname, formals, body) ->
                 function_code_gen fname formals body)
                        ^ (print_program tl)
in print_string (print_program program)
