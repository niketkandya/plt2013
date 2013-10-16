open Ast
open Bytecode

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : int StringMap.t; (* "Address" for global variables *)
    local_index    : int StringMap.t; (* FP offset for args, locals *)
  }

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =

  (* Allocate "addresses" for each global variable *)
  let global_indexes = string_map_pairs StringMap.empty (enum 1 0 globals) in
  (* TODO Code generation for globals *)

  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in

  (* Translate a function in AST form into a list of bytecode statements *)
  let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_formals = List.length fdecl.formals
    and num_locals = List.length fdecl.locals
    and num_temp = 0
    and local_offsets = enum 1 1 fdecl.locals in
    let formal_offsets = enum 1 (num_locals+1) fdecl.formals in
    let env = { env with local_index = string_map_pairs
		  StringMap.empty (local_offsets @ formal_offsets) } in

            let size_stmfd = 4 (* Total size pushed using stmfd -4 *) 
                and align_size = 4 (*Alignment of the stack *)
                and var_size = 4 (* right now doing only for integers *)
                in
        let get_fp_offset var = 
                (try 
                        ((((StringMap.find var env.local_index)-1) * align_size
                        ) + (size_stmfd) + (var_size))
                with Not_found -> raise (Failure ("Cannot find in load_code " ^
                var))  )
                in
        let load_code reg var= (* load variable var to register reg *)
                if 
                "\t ldr  " ^ reg ^ ", [fp, #-" ^ 
                string_of_int (get_fp_offset var)
                ^ "]\n"
        and 
        store_code reg var = (*TODO Stores the content of reg to a variable*)
                "\t str  " ^ reg ^ ", [fp, #-" ^ 
                string_of_int (get_fp_offset var)
                ^ "]\n"
        and
        add_temp = (*Generate a temporary variable and updates in locals_index *)
                let num_temp = num_temp + 1 in
                env.local_index = StringMap.add ("__tmp" ^ string_of_int num_temp ) 
                (num_formals + num_locals + num_temp) env.local_index;
                "__tmp" ^ string_of_int num_temp
                in
    let function_start = 
            (* Code generation for function *)
            fdecl.fname ^ ":\n" ^
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
    in
    let function_exit = "\t sub sp, fp, #4\n" ^
                        "\t ldmfd sp!, {fp, pc}\n" in
    let rec expr = function
            Literal i -> "#" ^ string_of_int i
      | Id s ->
                      (try (StringMap.find s env.local_index);s
          with Not_found -> try (StringMap.find s
          env.global_index);s
          with Not_found -> raise (Failure ("undeclared variable " ^ s)))
      | Binop (e1, op, e2) -> (expr e1) ^ (expr e2)
      | Assign (s, e) -> let v1 = (expr e) in (load_code "r0" v1);(store_code "r0" s); v1
(*	  (try (StringMap.find s env.local_index)
  	  with Not_found -> try (StringMap.find s env.global_index)
	  with Not_found -> raise (Failure ("undeclared variable " ^ s))) *)
      | Call (fname, actuals) -> (* (try *)
              let rec fcall i = function
                      []-> ""
                |hd::tl -> ( load_code ("r" ^ string_of_int i) hd ) ^ (fcall (i+1) tl)
               in fcall 0 (List.map expr (List.rev actuals)) ^ ("\n\t bl  "^
               fname ^ "\n" )
               (* ((StringMap.find fname env.function_index))
        with Not_found -> raise (Failure ("undefined function " ^ fname))) *)
      | Noexpr -> ""

    in let rec stmt = function
	Block sl     -> (*List.map stmt sl*)
                        (List.fold_left (fun str lst -> str ^ "\n" ^ lst) ""
                        (List.map stmt sl) )
                        (*stmt sl*)
      | Expr e       -> expr e
      | Return e     -> expr e
      | If (p, t, f) -> "let t' = stmt t and f' = stmt f in"
        (*"true =" ^ t ^ " false" ^ f'*)
      | While (e, b) ->
                      "let b' = stmt b and e' = expr e in"
      | _ -> "Check something went wron with this expression"

    in  function_start ^     (* Entry: allocate space for locals *)
    (stmt (Block fdecl.body)) (* Body *)
    (* List.fold_left (fun str lst -> str ^ "\n" ^ lst) "" (stmt (Block fdecl.body)) *)
     ^ function_exit  (* Default = return 0 *)

  in let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in

  (* Code executed to start the program *)
  let entry_function = try
    string_of_int (StringMap.find "main" function_indexes)
  with Not_found -> raise (Failure ("no \"main\" function"))
  in (* Compile the functions *)
   entry_function :: List.map (translate env) functions