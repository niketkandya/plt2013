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
    and num_temp = ref 0
    and local_offsets = enum 1 1 fdecl.locals in
    let formal_offsets = enum 1 (num_locals+1) fdecl.formals in
    let env = { env with local_index = string_map_pairs
		  StringMap.empty (local_offsets @ formal_offsets) } in

            let size_stmfd = 4 (* Total size pushed using stmfd -4 *) 
                and align_size = 4 (*Alignment of the stack *)
                and var_size = 4 (* right now doing only for integers *)
                in
        let get_var var = 
                (try 
                (Lvar ( (StringMap.find var env.local_index), var_size))
                with Not_found -> try (StringMap.find var env.global_index);
                      (Gvar(var,var_size)) 
                with Not_found -> raise (Failure ("undeclared variable " ^ var)))
                in
        let add_temp size= (*Generate a temporary variable and updates in locals_index *)
                num_temp := !num_temp + 1;
                env.local_index = StringMap.add ("__tmp" ^ string_of_int !num_temp ) 
                (num_formals + num_locals + !num_temp) env.local_index;
                Lvar((num_formals + num_locals + !num_temp),size)
                in
        let function_start = [Fstart (fdecl.fname, num_locals, num_formals)]
        (* [Uncond_br (StringMap.fold (fun key va str -> str ^"\n "^ key ^ " -> "^
        string_of_int va ) env.local_index "")] *)
        and function_exit = [Fexit]
        in

let gen_atom atm = [Atom (atm)]
in

let get_atom = function
            Atom (atm) -> atm
  | BinEval  (dst, var1, op, var2) -> dst
  | Fcall (fname, args,ret ) -> ret
  | Assgmt (dst, src) -> dst 
  | _ -> raise (Failure ("Unexpected value requested for"))
in

let dbg_get_var_name var = match var with 
            Lit(i) -> "\n" ^string_of_int i
        | Lvar(num,sz) -> "\nLvar " ^ string_of_int num ^ ":" ^ string_of_int sz
        | Gvar(var,sz) -> "\nGvar " ^ var ^ ":" ^ string_of_int sz
in

let rec expr = function
        Literal i -> gen_atom (Lit i)
      | Id s -> gen_atom (get_var s)
      | Binop (e1, op, e2) -> let v1 = expr e1 and v2 = expr e2 
                                and v3 = (add_temp var_size)
        in (gen_atom v3) @  v1 @ v2 @ [BinEval (v3 ,(get_atom (List.hd
              (List.rev v1))), op, (get_atom(List.hd (List.rev v2))))] 
        (*@ [Cond_br ("BinOP:" ^ dbg_get_var_name v3 ^ dbg_get_var_name ( get_atom
        (List.hd(List.rev v1)) ) ^
              dbg_get_var_name ( get_atom (List.hd(List.rev v2))))]*)
      | Assign (s, e) ->  let v1 = (expr e) in (gen_atom (get_var s)) @ v1 @
                [Assgmt ((get_var s),get_atom (List.hd v1))]
      | Call (fname, actuals) ->  (try
               (StringMap.find fname env.function_index)
        with Not_found -> raise (Failure ("undefined function " ^ fname)));
        let param = List.map expr (List.rev actuals)
        and ret = (add_temp var_size)
        in (gen_atom ret ) @ List.concat param @
        [Fcall (fname,List.rev (List.map (fun par -> get_atom (List.hd par)) param),ret)]
      | Noexpr ->[]

    in let rec stmt = function
	Block sl     -> (*List.map stmt sl*)
                (List.fold_left (fun str lst -> str @ lst) [] 
                        (List.map stmt sl) )
                        (*stmt sl*)
      | Expr e       -> expr e
      | Return e     -> let v1 = expr e in v1 @ [Rval (get_atom (List.hd v1))]
      | If (p, t, f) -> [Cond_br "asdfasdf"]
                      (* "let t' = stmt t and f' = stmt f in" *)
        (*"true =" ^ t ^ " false" ^ f'*)
      | While (e, b) -> []
                      (*"let b' = stmt b and e' = expr e in"*)
      | _ -> []

    in  function_start @     (* Entry: allocate space for locals *)
    (stmt (Block fdecl.body)) (* Body *)
     @ function_exit  (* Default = return 0 *)

  in let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in

  (* Code executed to start the program *)
  let entry_function = try
          (StringMap.find "main" function_indexes); []
  with Not_found -> raise (Failure ("no \"main\" function"))
  in (* Compile the functions *)
   List.concat (entry_function :: List.map (translate env) functions)
