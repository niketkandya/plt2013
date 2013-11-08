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

type byc_lvar_entry = {
        fp_offset:int;
        size:int;
        count:int
}

type byc_local_env = {
        lmap: byc_lvar_entry IntMap.t;
        midx : int;
        mfp  : int
}

type byc_env = {
        global_index: byc_gvar_entry StringMap.t;
        local_data: byc_local_env
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
let rec build_index lenv= function
        [] -> lenv
        | hd :: tl -> let add_align = match hd with
                Lvar(idx,sz,cnt) -> tmp_idx := idx;
                                tmp_fp := (lenv.mfp + ((int_of_float (ceil 
                ( (f (sz * cnt)) /. (f align_size) )) * align_size)));
                let rec add_noalign _idx _fp _cnt _map = 
                    match _cnt with
                    0 -> _map
                   | _ -> let m = IntMap.add 
                        _idx 
                        {fp_offset = _fp; size = sz; 
                        count = (if cnt = _cnt then _cnt else 1 )} 
                        _map 
                        in add_noalign (_idx -1) (_fp - sz) (_cnt -1) m
                (* the second argument calculation is to ensure that char has the 
                 * lowest 1 byte and not the higest one byte when its aligned at 
                 * align_size bytes *)
                in add_noalign idx (if cnt = 1 then (!tmp_fp - align_size + sz)
                        else !tmp_fp) cnt lenv.lmap
                   | _ -> raise (Failure ("Unexpected for local index building"))
        in build_index {midx = !tmp_idx; mfp = !tmp_fp ; lmap = add_align} tl
in
let function_code_gen env fname formals body =
        let idx_to_offset idx = (try
                let res = IntMap.find idx env.local_data.lmap in
                res.fp_offset
                (* When not found, its assumed that its a temporary variable.
                 * For not assuming all temporaries are Int only and hence this
                 * calulation is made for the temps
                 *)
        with Not_found ->
                ((idx - env.local_data.midx) * 4 ) + env.local_data.mfp)
        in
        (* Note register r4 will be left as a temporary register 
         * so that anybody can use .eg in gen_ldr_str_code *)
        let rec gen_ldr_str_code oper sym reg atm = 
                let pre sz =  oper ^ (if sz = 1 then "b" else "")
                        ^" "^reg ^", " in 
                match atm with
          Lit (i) -> p ( (pre 0)  ^ sym ^ string_of_int i)
        | Cchar (ch) -> p ((pre 1) ^ sym ^ string_of_int (int_of_char ch))
        | Lvar (idx, sz, cnt) -> p ( (pre sz) ^ "[fp,#-" ^ string_of_int
                                 (idx_to_offset idx) ^"]")
        | Gvar (vname, sz) -> "" (*TODO *)
        | Addr (vnm) -> (match vnm with
                Lvar(idx,sz,cnt) -> 
                        p "sub " ^reg^", fp,#" ^ 
                        string_of_int (idx_to_offset idx)
                |Gvar(vname,sz) -> "" (*TODO: Globals*)
                   | _ -> raise(Failure ("Lvars only should be passed")))
        | Pntr (vnm) -> (match vnm with
                Lvar(idx,sz,cnt) -> 
                        (gen_ldr_str_code "ldr" "=" "r4" vnm) ^
                        p ((pre sz) ^ "[r4,#0]")
                |Gvar(vname,sz) -> "" (*TODO: Globals*)
                | _ -> raise(Failure ("Lvars only should be passed")))
       in
        let load_code reg var = (* load variable var to register reg *)
                gen_ldr_str_code "ldr" "=" reg var
        and store_code reg var = (*TODO handle strb case*)
                gen_ldr_str_code "str" "#" reg var in
let bin_eval dst var1 op var2 = 
        let oper = (match op with
        Add -> p "adds r3, r0, r1"
      | Sub -> p "subs r3, r0, r1" 
      | Mult -> p "muls r3, r0, r1"
      | Div -> p "Division"
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
        "cmp r0, r1
         movlt r3,#1
         movge r3,#0
         uxtb r3,r3"
      | Leq -> 
        "cmp r0, r1
         movle r3,#1
         movgt r3,#0
         uxtb r3,r3"
      | Greater -> 
        "cmp r0, r1
         movgt r3,#1
         movle r3,#0
         uxtb r3,r3"
      | Geq -> 
        "cmp r0, r1
         movge r3,#1
         movlt r3,#0
         uxtb r3,r3"
        )^ "\n"
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
                    else "\t beq "
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
  | Rval var -> load_code "r0" var
  | Branch label -> "\tbl " ^ label
  | Label label -> label ^ ":" 
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
                   p ("sub sp, sp,#" ^ string_of_int (env.local_data.mfp - size_stmfd)) ^ 
                   let rec formals_push_code i = if i < 0 then "" else 
                            (formals_push_code (i-1)) ^ 
                            (store_code ("r" ^ string_of_int i) (List.nth formals i))
                    in formals_push_code ((List.length formals) -1)
                    (* TODO : ifjthe variable size is 1 byte, strb should be
                     * used instead and the var_size should be updated
                     * accordingly *)
        and func_end_code = p "sub sp, fp, #4" ^
                       p "ldmfd sp!, {fp, pc}"
        in func_start_code ^
        (List.fold_left 
                (fun str lst -> str ^ (asm_code_gen lst)) 
                "" (non_atom body))
        ^ func_end_code
in
let env = {
        global_index = StringMap.empty;
        local_data = {lmap=IntMap.empty;
                        mfp =0;
                        midx = 0}
         }
in let rec print_program = function 
        [] -> "" 
        | hd :: tl ->
                (match hd with
                Global (atmlst) -> "" (*TODO: Global functions code *)
                | Fstart (fname, locals, formals, body) ->  let env =
                        let tmp = { env with local_data = build_index 
                        {midx =0;mfp = size_stmfd;lmap = IntMap.empty } locals } in
                        { tmp with local_data = build_index tmp.local_data formals } in
                        function_code_gen env fname formals body) ^ (print_program tl)
in print_string (print_program program)
