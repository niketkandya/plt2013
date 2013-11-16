open Ast
open Bytecode

module StringMap = Map.Make(String)

type var_entry = { 
        offset:int;
        count:int;
        typ: cpitypes list
}

type func_entry = {
        param : var_entry list;
        ret_ty : cpitypes 
         }

type struct_entry = {
        ind_count: int;
        size: int;
        memb_index: var_entry StringMap.t
}

(* Symbol table: Information about all the names in scope *)
type envt = {
    function_index : func_entry StringMap.t; (* Index for each function *)
    struct_index   : struct_entry StringMap.t;
    global_index   : var_entry StringMap.t; (* "Address" for global variables *)
    local_index    : var_entry StringMap.t; (* FP offset for args, locals *)
  }


(*Careful about calling get_size_* functions *)
let get_size_type lenv typlst = function 
                |[] -> raise (Failure("List empty"))
                | hd::tl -> (match hd with
                        Void -> 0
                        | Char -> 1
                        | Int
                        | Ptr -> 4
                        | Arr(sz) -> sz * 
                        | Struct(sname) -> (StringMap.find sname 
                                lenv.struct_index).size
                        | _ -> raise (Failure ("Requesting size of wrong type")))

let get_idx_count lenv var = var.vcount * 
                (match (List.hd (List.rev var.vtype)) with
                Char
                | Int -> 1
                | Struct(sname) -> (StringMap.find sname
                        lenv.struct_index).ind_count
                | _ -> raise (Failure ("Requesting size of wrong type"))


(* Size of datatypes *)
let get_size_var var = var.vcount * (get_size_type var.vtype)

let build_global_idx map = map

let rec build_local_idx map env offset hole= function
       [] -> map
       | hd:: tl -> match (List.hd hd.vtype) with 
                Arr(sz) -> 
           Struct(sname) -> 
              build_local_idx map count ((gen_struct_varlst id) @ tl)
                | _ -> count := !count + cnt;
                build_local_idx (StringMap.add id
                {index = !count; count = cnt; typ = tp} map) count tl )
           | _ -> raise (Failure("Build index: Unexpected type"))


(* Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate structs globals functions =

        (* Allocate "addresses" for each global variable *)
  (* TODO Code generation for globals *)
  let global_indexes = build_global_idx StringMap.empty (enum 1 0 globals) in
  let struct_indexes = List.fold_left (fun map stct -> 
                (match stct with
                Struct(nm,lst)-> (StringMap.add nm 
                lst map)
                | _ -> map)) StringMap.empty structs
          in
  (*TODO: Add the buil-in-function printf to the below list *)
(* let built_in_functions = StringMap.add "print" (-1) StringMap.empty in *)
let function_indexes = List.fold_left 
        (fun map fdecl -> 
           let rec var_to_lst ind = function
                  [] -> []
                | hd:: tl -> (match hd with
                  Var(id,ty,cn) -> 
                      { index = ind;
                        count = cn;
                        typ = ty
                      } 
                      :: (var_to_lst (ind+1) tl)
                | _ -> raise (Failure(" Unexpected var_to_lst"))
                ) in
          StringMap.add fdecl.fname 
                {
                   param = (var_to_lst 0 fdecl.formals);
                   ret_ty = fdecl.ret
                } map
        )
        StringMap.empty functions
in
let align_size = 4
in 
(* Translate a function in AST form into a list of bytecode statements *)
let translate env fdecl=
    (* Bookkeeping: FP offsets for locals and arguments *)
    let num_mlocal = ref 0
     and count_loop = ref 0
     and count_ifelse = ref 0
     and temp_prefix = "__temp"
     and temp_list = ref []
     in

    let gen_struct_varlst dotnm = 
        let dotind = String.index dotnm '.' in
        let structnm = String.sub dotnm 0 dotind in
        let varlen = ((String.length dotnm) - (dotind + 1)) in
        let varnm = String.sub dotnm (dotind +1) varlen in
        let varlst = (try
                (StringMap.find structnm env.struct_index)
                with Not_found -> raise (Failure("Struct '"^structnm^"' not
                found"))) in 
        let rec create_lst = (function
                [] -> []
                |hd::tl ->(match hd with
                Var(nm,t,c) -> Var(varnm^"."^nm,t,c)
                        :: (create_lst tl)
                |_ -> raise (Failure("Unexpected in list")))
                ) in create_lst varlst
    in
    let env = { env with local_index = ref
            (build_local_idx StringMap.empty num_mlocal (fdecl.locals @ fdecl.formals)) }
    in
        let add_temp tp = (*Generate a temporary variable and updates in locals_index *)
                let lvar = Lvar(!num_mlocal+ 1+(List.length !temp_list),(get_size_type tp),1)
                in
                temp_list :=(lvar :: !temp_list);
                lvar in

        let get_func_entry name = (try
                        StringMap.find name env.function_index
        with Not_found -> raise (Failure("Function not found : " ^ name))) in
        let get_var ?(en = env.local_index) ?(bty = false) ?(idx = -1) var = (*idx is when using it for an array subscript*)
                (try
                (let a = (StringMap.find var !(en)) in 
                let var_idx = if idx = -1 then a.index else a.index - idx
                and var_cnt = if idx = -1 then a.count else 1 in
                let var_sz = if bty then (get_size_btype a.typ) else
                                (get_size_type a.typ) in
                        Lvar (var_idx,var_sz,var_cnt))
                with Not_found -> try 
                        let a = (StringMap.find var env.global_index) in
               (* let var_idx = if idx = -1 then a.index else a.index - idx
                and var_cnt = if idx = -1 then a.count else 1 in*)
                let var_sz = if bty then (get_size_btype a.typ) else
                                (get_size_type a.typ) in
                        (Gvar(var,var_sz)) (*TODO- *)
                with Not_found -> raise (Failure ("Undeclared variable " ^ var)))
                in
        let get_varname_size ?(bt = false) ?(ix = -1) varname = 
                match (get_var ~bty:bt ~idx:ix varname) with
                Lvar(i,s,c) -> s
                |Gvar(vn,s) -> s
        in 
        let rec conv2_byt_lvar = function
                [] -> []
                | hd::tl -> (match hd with
                  Var(id,typ,cnt) -> (
                          match typ with
                          Structtyp -> conv2_byt_lvar ( (gen_struct_varlst id)@tl)
                          | _ -> (get_var id) :: (conv2_byt_lvar tl)
                          )
                | _ -> raise (Failure("Unexpected in bytecode conversion")))
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
      | String s -> gen_atom (Sstr s)
      | Id s -> gen_atom (get_var s)
      | Binop (e1, op, e2) -> let v1 = expr e1 
                                and v2 = expr e2
                                and v3 = (add_temp Int)
                in (gen_atom v3) @  v1 @ v2  @
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
                and ret = (add_temp (get_func_entry fname).ret_ty)
                in (gen_atom ret ) @ List.concat param @
                [Fcall (fname,List.rev 
                (List.map (fun par -> get_atom (List.hd par)) param)
                ,ret)]
      | Ptr(v) ->  gen_atom (Pntr((get_var v),(get_varname_size ~bt:true v) ))
      | Arr(nm,ind) ->(try
                      gen_atom (get_var ~bty:true ~idx:(int_of_string ind) nm)
                      with _ ->gen_atom(Array(gen_atom(get_var nm),gen_atom(get_var
                        nm))))
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
      in 
      let asdf =
            (stmt (Block fdecl.body)) in
[Fstart (
            fdecl.fname,
            (conv2_byt_lvar fdecl.locals),
            (conv2_byt_lvar fdecl.formals),
            asdf,
            (!temp_list)
        (*let rec conv2_byt_tmp tmp = 
                get_var ~en:env.local_index (temp_prefix ^ string_of_int tmp) ::
                (match tmp with
                   1 -> []
                   | _ -> conv2_byt_tmp ( tmp - 1) )
        in ( conv2_byt_tmp !num_temp) *)
    )]

in let env = { function_index = function_indexes;
		 global_index = global_indexes;
                 struct_index = struct_indexes;
		 local_index = ref StringMap.empty } in

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
