open Ast
open Bytecode

module StringMap = Map.Make(String)

type var_entry = { 
        index:int;
        count:int;
        typ: cpitypes
}

(* Symbol table: Information about all the names in scope *)
type env = {
    function_index : int StringMap.t; (* Index for each function *)
    global_index   : var_entry StringMap.t; (* "Address" for global variables *)
    local_index    : var_entry StringMap.t; (* FP offset for args, locals *)
  }


(*Careful about calling get_size_* functions *)
let get_size_type typ = match typ with
                Char
                | Chararr -> 1
                | Int
                | Intptr
                | Charptr
                | Intarr -> 4
                | _ -> raise (Failure ("Requesting size of wrong type"))

(* Size of datatypes *)
let get_size_var var = match var with 
        Var(id,typ,cnt) -> match typ with
                Int 
                | Char 
                | Intptr
                | Charptr -> get_size_type typ 
                | Struct 
                | Intarr
                | Chararr
                | Structarr ->cnt 

(* val enum : int -> 'a list -> (int * 'a) list *)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

(* val string_map_pairs StringMap 'a -> (int * 'a) list -> StringMap 'a *)
let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

let build_global_idx map pairs = map

(* Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate (globals, functions) =
  (* Allocate "addresses" for each global variable *)
  let global_indexes = build_global_idx StringMap.empty (enum 1 0 globals) in
  (* TODO Code generation for globals *)
  (* Assign indexes to function names; built-in "print" is special *)
  let built_in_functions = StringMap.add "print" (-1) StringMap.empty in
  let function_indexes = string_map_pairs built_in_functions
      (enum 1 1 (List.map (fun f -> f.fname) functions)) in


(* Translate a function in AST form into a list of bytecode statements *)
let translate env fdecl =
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_mlocal = ref 0
     and num_temp = ref 0
     and count_loop = ref 0
     and count_ifelse = ref 0
     and func_vars = fdecl.locals @ fdecl.formals in
    let var_offsets = enum 1 1 func_vars in
        (* Index is assigned based on total number of basic datatypes contained
         * in the type. e.g if there a variable of type int, it will get one
         * index more than the previous one. For an array of n elements , it
         * will get an index n more than the previous one *)
    let build_local_idx map pairs =
        List.fold_left (fun m (i, n) -> match n with 
        Var(id,tp,cnt) -> num_mlocal := i+cnt;
          (StringMap.add id {index = !num_mlocal; count = cnt; typ = tp} m)
        | _ -> raise (Failure("Unable to build_index for this type"))
          ) map pairs
          (*TODO: Struct should be handled seperately. It should have a Struct type
         * whose arguments are the list of all its constituent basic datatypes
         *)

    in
    let env = { env with local_index = 
            (build_local_idx StringMap.empty var_offsets) } in
        let get_var ?(idx = -1) var = (*idx is when using it for an array subscript*)
                (try
                (let a = (StringMap.find var env.local_index) in 
                let var_idx = if idx = -1 then a.index else a.index - idx
                and var_cnt = if idx = -1 then a.count else 1 in
                Lvar (var_idx,(get_size_type a.typ),var_cnt))
                with Not_found -> try 
                        let a = (StringMap.find var env.global_index) in
                        (Gvar(var,(get_size_type a.typ))) (*TODO- *)
                with Not_found -> raise (Failure ("undeclared variable " ^ var)))
                in
        let conv2_byt_lvar var = match var with
                Var(id,typ,cnt) -> get_var id
        in
        let add_temp tp = (*Generate a temporary variable and updates in locals_index *)
                num_temp := !num_temp + 1;
                env.local_index = StringMap.add ("__tmp" ^ string_of_int !num_temp )
                {index = (!num_mlocal + !num_temp); count = 1;typ = tp} env.local_index;
                Lvar((!num_mlocal + !num_temp),(get_size_type tp),1)
        in
        let get_loop_label num = "loop" ^ match num with
                0 -> string_of_int (count_loop := !count_loop + 1; !count_loop) ^ "_start"
                |1 -> string_of_int !count_loop ^ "_end"
                |_ -> ""
        in
        let get_ifelse_label num =  match num with 
                0 -> "else" ^ string_of_int (count_ifelse := !count_ifelse + 1;
                        !count_ifelse)
                |1 -> "end" ^ string_of_int !count_ifelse
                |_ -> ""
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
(*
let dbg_get_var_name var = match var with 
            Lit(i) -> "\n" ^string_of_int i
        | Lvar(num,sz) -> "\nLvar " ^ string_of_int num ^ ":" ^ string_of_int sz
        | Gvar(var,sz) -> "\nGvar " ^ var ^ ":" ^ string_of_int sz
in
*)
let rec expr = function
        Literal i -> gen_atom (Lit i)
      | Id s -> gen_atom (get_var s)
      | Binop (e1, op, e2) -> let v1 = expr e1 
                                and v2 = expr e2
                                and v3 = (add_temp Int)
                in (gen_atom v3) @  v1 @ v2 @ 
                [BinEval (v3 ,(get_atom (List.hd (List.rev v1))), op, 
                (get_atom(List.hd (List.rev v2))))]
      | Assign (s, e) ->
                      let v1 = (expr e)
                      in (expr s) @ v1 @
                [Assgmt ((get_atom(List.hd (expr s))),get_atom (List.hd v1))]
      | Call (fname, actuals) ->  (try
               (StringMap.find fname env.function_index)
                with Not_found -> raise (Failure ("undefined function " ^ fname)));
                let param = List.map expr (List.rev actuals)
                and ret = (add_temp Int)
                in (gen_atom ret ) @ List.concat param @
                [Fcall (fname,List.rev 
                (List.map (fun par -> get_atom (List.hd par)) param)
                ,ret)]
      | Ptr(v) ->  gen_atom (Pntr(get_var v ))
      | Arr(nm,ind) -> gen_atom (get_var ~idx:ind nm)
      | Addrof(v) -> let v1 = expr v in gen_atom (Addr(get_atom(List.hd v1)))
      | ConstCh(ch) -> gen_atom(Cchar(ch.[1]))
      | Noexpr ->[]

    in 
let rec stmt = function
	Block sl     ->
                (List.fold_left (fun str lst -> str @ lst) [] 
                        (List.map stmt sl) )
                        (*stmt sl*)
      | Expr e       -> expr e
      | Return e     -> let v1 = expr e in v1 @ [Rval (get_atom (List.hd v1))]
      | If (p, t, f) -> let v1 = expr p 
                        and v2 = stmt t 
                        and v3 = (stmt f) in
                        let v4 = (get_atom (List.hd(List.rev v1))) in
                        let l1 = (get_ifelse_label 0) 
                        and l2 = (get_ifelse_label 1) in (match v3 with
                                [] -> v1 @ [Predicate (v4,false, l2)] 
                                        @ v2  @ [Label l2]
                                | _ ->v1 @ [Predicate (v4,false, l1)] @ v2  @ 
                                        [Branch (l2)]@ [Label l1] @ v3 @ [Label l2])
      | While (e, b) -> let v1 = stmt b 
                        and v2 = expr e
                        and l0 = (get_loop_label 0) 
                        and l1 = (get_loop_label 1) in
                        let v3 = (get_atom (List.hd(List.rev v2))) in
                        [Branch l1] @ [Label l0] @ v1 @ [Label l1] 
                        @ v2 @ [Predicate (v3,true,l0)]
      | _ -> []

in [Fstart (
            fdecl.fname,
            (List.map conv2_byt_lvar fdecl.locals), 
            (List.map conv2_byt_lvar fdecl.formals), 
            (stmt (Block fdecl.body))
    )]

in let env = { function_index = function_indexes;
		 global_index = global_indexes;
		 local_index = StringMap.empty } in

  (* Code executed to start the program *)
let entry_function = try
          (StringMap.find "main" function_indexes); []
        with Not_found -> raise (Failure ("no \"main\" function"))
in 
(* Compile the functions *)
   List.concat (entry_function :: List.map (translate env) functions)
(* TODO: Globals might need to be passed before at the point where
 * entry_function is present. Globals can be passed as a list, like that of
 * Fstart *)
