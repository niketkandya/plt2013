open Ast
open Bytecode





let execute_prog program = 
        
let size_stmfd = 4 (* Total size pushed using stmfd -4 *) 
                and align_size = 4 (*Alignment of the stack *)
                and var_size = 4 (* right now doing only for integers *)
                in

let function_start fname num_locals num_formals = 
            (* Code generation for function *)
            fname ^ ":\n" ^
                    "\t stmfd sp!, {fp, lr}\n" ^
                    "\t add fp, sp,#"^ string_of_int size_stmfd ^"\n" ^
                    "\t sub sp, sp,#" ^ string_of_int (( num_formals + num_locals) * align_size) ^ 
                    "\n" ^
                    let rec formals_push_code i = if i < 0 then "" else 
                            (formals_push_code (i-1)) ^ "\t str  r" ^ string_of_int i ^ ", [fp, #-" ^
                            string_of_int (((num_locals + i) * align_size) +
                            var_size) ^ "]\n"
                    in formals_push_code (num_formals -1)
                    (* TODO : if the variable size is 1 byte, strb should be
                     * used instead and the var_size should be updated
                     * accordingly *)
                    (* need a protocol to get the offset of locals given that
                     * formals are present first *)
                (*StringMap.fold (fun key va pre -> pre ^ "\n" ^key ^ ":" ^
                 * string_of_int va) env.local_index "" *)
and function_exit = "\t sub sp, fp, #4\n" ^
                        "\t ldmfd sp!, {fp, pc}\n"
in
let function_call fname args =  
              let rec fcall i = function
                      []-> ""
                |hd::tl -> (* (load_code ("r" ^ string_of_int i) hd ) ^ *) (fcall (i+1) tl)
               in fcall 0 args ^ ("\n\t bl  "^
               fname ^ "\n" )
               (* TODO implement properly *)
in

let asm_code_gen = function
   Atom (atm) ->  "Atom"
  | Fstart (fname, num_locals, num_formals) ->  function_start fname num_locals num_formals (*start of a function*)
  | Fexit  ->  function_exit          (*Restore registers values at exit*)
  | BinEval  (dst, var1, op, var2) ->  "BinEval"(*Binary evaluation *)
  | Str (reg , atm ) ->  "Store"
  | Ldr (reg ,atm ) ->  "Load"
  | Mov (dst, src) ->  "Move"
  | Fcall (fname, args ) ->  function_call fname args  (*Whenever a function is called*)
  | Uncond_br label ->  "Unconditional branch"
  | Cond_br label ->  "Conditional branch"

in
        (List.map print_endline (List.map asm_code_gen program))

                

