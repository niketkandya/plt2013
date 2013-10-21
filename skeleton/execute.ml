open Ast
open Bytecode
open Printf

let program_name = "cpi_out";;

let execute_prog program = 
let size_stmfd = 4 (* Total size pushed using stmfd -4 *) 
                and align_size = 4 (*Alignment of the stack *)
                and var_size = 4 (* right now doing only for integers *)
                in

let idx_to_offset idx = size_stmfd + ((idx-1) * align_size) + var_size
in
let get_atom_val atm = match atm with
        Lit (i) -> "#" ^ string_of_int i
      | Lvar (idx, sz) -> "[fp,#-" ^ string_of_int (idx_to_offset idx) ^"]"
      | Gvar (vname, sz) -> "" (*TODO *)
in
let load_code reg var= (* load variable var to register reg *)
                "\t ldr  " ^ reg ^ ", "^ let const = (get_atom_val var) in (if
                        const.[0] = '#' then const.[0] <- '=';const )^ "\n"
and 
    store_code reg var = (*TODO handle strb case*)
                "\t str " ^ reg ^ ", "^ (get_atom_val var)^ "\n" in

let function_start fname num_locals num_formals = 
            (* Code generation for function *)
                ".global " ^ fname ^ "\n" ^
            fname ^ ":\n" ^
                    "\t stmfd sp!, {fp, lr}\n" ^
                    "\t add fp, sp,#"^ string_of_int size_stmfd ^"\n" ^
                    "\t sub sp, sp,#" ^ string_of_int (( num_formals + num_locals) * align_size) ^ 
                    "\n" ^
                    let rec formals_push_code i = if i < 0 then "" else 
                            (formals_push_code (i-1)) ^ 
                            (store_code ("r" ^ string_of_int i)
                            (Lvar((num_locals + i + 1),var_size)))
                    in formals_push_code (num_formals -1)
                    (* TODO : if the variable size is 1 byte, strb should be
                     * used instead and the var_size should be updated
                     * accordingly *)
and function_exit = "\t sub sp, fp, #4\n" ^
                        "\t ldmfd sp!, {fp, pc}\n"
in
let bin_eval dst var1 op var2 = 
        let oper = "\t " ^  (match op with
        Add -> "add r3, r0, r1"
      | Sub -> "sub r3, r0, r1" 
      | Mult -> "mul r3, r0, r1"
      | Div -> "Division"
      | Equal -> "cmp r0, r1" 
      | Neq -> "Negative"
      | Less -> "Less"
      | Leq -> "Leq"
      | Greater -> "Greater"
      | Geq -> "Geq") ^ "\n"
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

let asm_code_gen = function
   Atom (atm) -> ""
  | Fstart (fname, num_locals, num_formals) ->  function_start fname num_locals num_formals (*start of a function*)
  | Fexit  ->  function_exit          (*Restore registers values at exit*)
  | BinEval  (dst, var1, op, var2) -> bin_eval dst var1 op var2
  | Assgmt (dst, src) -> (load_code "r0" src) ^ (store_code "r0" dst)
  | Str (reg , atm ) ->  "Store"
  | Ldr (reg ,atm ) ->  "Load"
  | Mov (dst, src) ->  "Move"
  | Fcall (fname, args,ret) ->  function_call fname args ret  (*Whenever a function
          is called*) (*TODO do something for the ret value*)
  | Rval var -> load_code "r0" var
  | Uncond_br label -> label
  | Cond_br label -> label

in 
let non_atom = (List.filter (fun ele -> match ele with Atom (atm ) -> false | _ -> true) program)
in 
    let program_str = String.concat " " (List.map asm_code_gen non_atom)
    in
      let out_file = open_out ( program_name ^ ".s") in
        fprintf out_file "%s\n" program_str;
        close_out out_file; print_endline program_str

