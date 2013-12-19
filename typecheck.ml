open Ast
open Sast
open Debug

module StringMap = Map.Make(String)

let rec get_size_type sindex = function 
|[] ->   raise Exit
| hd::tl -> 
  (match hd with
    Void -> 0
  | Char -> 1
  | Int
  | Ptr -> 4
  | Arr(sz) ->  (match sz with
        Literal(i) -> i
        | Id(id) -> get_size_type sindex [Ptr]
        | _ -> raise(Failure("lit_to_num: unexpected"))) * (get_size_type sindex tl)
  | Struct(sname) -> (StringMap.find sname sindex).size
  | _ -> raise (Failure ("Requesting size of wrong type")));;

let rec build_local_idx map sidx offset ?(rev =0) = (function
    [] -> map
  | hd:: tl ->
      offset := 0;
      build_local_idx ~rev:rev 
    (if StringMap.mem hd.vname map then 
      raise (Failure("Double declaration of " ^ hd.vname ))
     else
      StringMap.add hd.vname
        {
          offset = 0;
          typ = hd.vtype
        } map
      )
      sidx offset tl);;

let build_global_idx map = StringMap.empty;;

(* Translate a program in AST form into a bytecode program.  Throw an
 *   exception if something is wrong, e.g., a reference to an unknown
 *   variable or function *)
let type_check_prog prog =
  let structs = prog.sdecls 
  and globals = prog.gdecls
  and functions = prog.fdecls in

(* Allocate "addresses" for each global variable *)
(* TODO Code generation for globals *)
let global_indexes = build_global_idx globals in

let struct_indexes = List.fold_left
  (fun map stct ->
    let soffset = ref 0 in
      let index = build_local_idx ~rev:1 
        StringMap.empty map soffset (List.rev stct.smembers) in
        (
          StringMap.add stct.sname 
            {
              size = !soffset;
              memb_index = index
            } map
        )
  ) 
  StringMap.empty structs
in

let f_index = List.fold_left 
  (fun map fdecl ->
    let rec var_to_lst ind = function
      [] -> []
    (*TODO Check correct values*)
    | hd :: tl -> ( {offset =0; typ = hd.vtype} :: (var_to_lst (ind+1) tl)) in
      StringMap.add fdecl.fname 
      {
        param = (var_to_lst 0 fdecl.formals);
        ret_ty = fdecl.ret
      }
      map
  )
  StringMap.empty functions
in

let f2_index = 
  StringMap.add "printf" 
  {
    param = [];
    ret_ty = [Int]
  }
  f_index
in

let f3_index =
  StringMap.add "scanf" 
  {
    param = [];
    ret_ty = [Int]
  }
  f2_index
in

let f4_index =
  StringMap.add "malloc" 
  {
    param = [];
    ret_ty = [Any]
  }
  f3_index
in

let function_indexes =
  StringMap.add "free" 
  {
    param = [];
    ret_ty = [Void]
  }
  f4_index
in

(* Translate a function in AST form into a list of bytecode statements *)
let type_check_func env fdecl=
 let curr_offset = ref 0 in
  let env = 
    {
      env with local_index = 
        (build_local_idx StringMap.empty env.struct_index curr_offset 
        (fdecl.locals @ fdecl.formals))
    }
    in
  let rec conv2_expr_t = function
      [] -> []
    | hd::tl -> Id_t(hd.vname, hd.vtype) :: (conv2_expr_t tl)
    in
  let get_func_entry name = 
    try StringMap.find name env.function_index
    with Not_found -> raise (Failure("Function not found: " ^ name)) 
    in
  let get_func_decl_typs name =
    let param = (get_func_entry name).param in
      let rec conv_param2_typ_lst = function
        [] -> []
      | hd::tl -> hd.typ :: (conv_param2_typ_lst tl) in
      conv_param2_typ_lst param
    in
  let get_type_varname table varname = 
    try (StringMap.find varname table).typ
    with Not_found -> raise (Failure("Type Checking Varname not found: " ^ varname))
    in
  let get_type_lst_expr_t = function 
    | Literal_t(i, t) -> t
    | String_t(s, t) -> t
    | Addrof_t(e, t) -> t
    | Negof_t(e, t) -> t
    | ConstCh_t(s, t) -> t
    | Id_t(s, t) -> t
    | MultiId_t(e1, r, e2, t) -> t
    | Pointer_t(e, t) -> t
    | Array_t(s, e, t) -> t
    | Binop_t(e1, o, e2, t) -> t
    | Assign_t(e1, e2, t) -> t
    | Call_t(s, e_l, t) -> t
    | Noexpr_t(t) -> t
    in
  let is_arr typ_lst = 
    match (List.hd typ_lst) with 
    | Arr(_) -> true
    | _ -> false
    in
  let get_typs_from_expr_t_lst param =
      let rec conv_el2_typ_lst = function
        [] -> []
      | hd::tl -> get_type_lst_expr_t hd :: (conv_el2_typ_lst tl) in
      conv_el2_typ_lst param
    in
  let get_struct_table2 stct =
    (try (StringMap.find stct env.struct_index).memb_index
     with Not_found -> raise(Failure(" struct " ^ stct ^ " is not a type")))
    in 
  let get_struct_table typ_lst = 
    match typ_lst with 
    | [Struct(s)] -> (get_struct_table2 s)
    | [Ptr; Struct(s)] -> (get_struct_table2 s)
    | _ -> raise (Failure
      ("Variable is " ^ (dbg_typ typ_lst) ^ " and not a Struct"))
    in
  let rec lst_match list1 list2 = 
    let typ_equal t1 t2 = 
      if t1 = t2 then true else
      match t1, t2 with
      | Ptr, Arr(_) -> true
      | Arr(_), Ptr -> true 
      | Arr(_), Arr(_) -> true 
      | _ , _  -> false in
        match list1, list2 with
        | h1::t1, h2::t2 -> typ_equal h1 h2 && lst_match t1 t2
        | [_], _ -> false
        | _, [_] -> false
        | _, _ -> true
    in
  let is_int_or_char ty =
    if lst_match ty [Int] then true 
      else if lst_match ty [Char] then true
      else false
    in
  let rec binop_result_type ?(strict=false) ty1 op ty2 =
        match ty1, ty2, op, strict with
        | [Int],  [Int],  _, _ -> [Int]
        | [Char], [Char], _, _ -> [Char]
        | _, _, _, true -> [Err]
        | [Int],  [Char], _, _ -> [Int]
        | [Char], [Int],  _, _ -> [Int]
        | Ptr::tl, [Int], Add, _ -> ty1
        | Ptr::tl, [Char], Add, _ -> ty1
        | Ptr::tl, [Int], Sub, _ -> ty1
        | Ptr::tl, [Char], Sub, _ -> ty1
        | [Int], Ptr::tl, Add, _ -> ty2
        | [Char], Ptr::tl, Add, _ -> ty2
        | [Int], Ptr::tl, Sub, _ -> ty2
        | [Char], Ptr::tl, Sub, _ -> ty2
        | Arr(s)::tl, [Int], Add, _ -> ty1
        | Arr(s)::tl, [Char], Add, _ -> ty1
        | Arr(s)::tl, [Int], Sub, _ -> ty1
        | Arr(s)::tl, [Char], Sub, _ -> ty1
        | [Int], Arr(s)::tl, Add, _ -> ty2
        | [Char], Arr(s)::tl, Add, _ -> ty2
        | [Int], Arr(s)::tl, Sub, _ -> ty2
        | [Char], Arr(s)::tl, Sub, _ -> ty2
        | Ptr::t1, [Int], Equal, _ -> ty1 
        | Ptr::t1, [Char], Equal, _ -> ty1 
        | Ptr::t1, Ptr::t2, Equal, _ -> binop_result_type ~strict:true t1 op t2
        | Arr(s1)::t1, Arr(s2)::t2, Equal, _ -> binop_result_type ~strict:true t1 op t2
        | _ , _ , _, _ -> [Err]
    in
  let assign_expr_result_type lh ty1 rh ty2 =
    let is_lh_arr t =
      (match (List.hd t) with 
      | Arr(_) -> raise(Failure("Assign Type Error: Left hand side cannot be an
        array pointer"))
      | _ -> false)  in 
    let is_lh_addr lh =
      (match lh with 
      | Addrof_t(_,_) -> raise(Failure("Assign Type Error: Left hand side cannot be an
       address expression"))
      | _ -> false)  in 
    if lst_match ty1 ty2 && not(is_lh_arr ty1) && not(is_lh_addr lh) then ty1
    else
       match ty1, ty2 with
        | [Int],  [Char] -> [Int]
        | [Char], [Int] -> [Char]
        | typ , [Any] -> typ
        | _ , _  -> [Err]
    in
  let assign_result_type ty1 ty2 =
    if lst_match ty1 ty2 then ty1
    else
       match ty1, ty2 with
        | [Int],  [Char] -> [Int]
        | [Char], [Int] -> [Char]
        | _ , _  -> [Err]
    in
  let rec cmp_param_typ list1 list2 fname = 
    (* Since printf and scanf are externally declared ignore them *) 
    if (fname = "printf" || fname = "scanf" || fname="malloc" || fname="free") then true 
    else
      match list1, list2 with
      | h1::t1, h2::t2 -> 
        if (lst_match (assign_result_type h1 h2) [Err]) then
        false else cmp_param_typ t1 t2 fname
      | [_], _ -> false
      | _, [_] -> false
      | _, _ -> true
    in
let rec tc_expr ?(table = env.local_index) ?(strict=0) = function
    Literal i -> Literal_t(i, [Int])
  | String s -> String_t(s, [Ptr; Char])
  | ConstCh(ch) -> ConstCh_t(ch, [Char])
  | Id s ->
    let typ = get_type_varname table s in
    (*if is_arr typ then
      Id_t (s, [Ptr] @ (List.tl typ))
      else*) Id_t(s, typ)
  | MultiId(fexpr,resolve,e) ->
    let v1 = tc_expr fexpr in
      let v1_type = get_type_lst_expr_t(v1) in
      (*let tab = (match v1_type with
        | [Struct(s)] -> get_struct_table s
        | [Prt;Struct(s)] -> get_struct_table s
        | _ -> raise(Failure("Variable is "^ (dbg_typ v1_type) ^" and not a
        Struct"))) in *)
      let tab = (get_struct_table v1_type) in 
      let v2 = tc_expr ~table:tab ~strict:1 e in
      let v2_type = get_type_lst_expr_t(v2) in
        (*  raise (Failure ("Struct: 
           First part is " ^ (dbg_typ v1_type) ^ " second part is " ^ (dbg_typ
           v2_type
           )) *)
      (match v1_type, resolve with
        | [Struct(s)], Dot -> MultiId_t(v1, Dot, v2, v2_type)
        | [Ptr;Struct(s)], Dot ->
          raise (Failure ("Struct Mismatch: 
           Cannot use resolve operator " ^ (dbg_str_resolve resolve 0) ^ " with
           Struct "^ s ^ " which has type " ^ (dbg_typ v1_type))) 
        | [Struct(s)], Ind ->
          raise (Failure ("Struct Mismatch: 
           Cannot use resolve operator " ^ (dbg_str_resolve resolve 0) ^ " with
           Struct "^ s ^ " which has type " ^ (dbg_typ v1_type))) 
        | [Ptr;Struct(s)], Ind -> MultiId_t(v1, Ind, v2, v2_type) 
        | _ , _ ->
          raise (Failure ("Struct Error: 
           Unknown struct with resolve operator " ^ (dbg_str_resolve resolve 0)
           ^ " and type" ^ (dbg_typ v1_type)))) 
  | Binop (e1, op, e2) -> 
    let lh = tc_expr e1 and rh = tc_expr e2 in
      let lh_type = get_type_lst_expr_t(lh)
      and rh_type = get_type_lst_expr_t(rh) in
      let ty = binop_result_type lh_type op rh_type in
        if lst_match ty [Err] then
         (* Binop_t(lh, op, rh, [Err]) *)
          raise (Failure ("Binop mismatch: 
           Left side is " ^ (dbg_typ lh_type) ^ " Right
           side is " ^ (dbg_typ rh_type) ^ 
           " op is " ^ dbg_str_op op 0)) 
        else Binop_t(lh, op, rh, ty)
  | Assign (s, e) ->
    let lh = (tc_expr s) and rh = (tc_expr e) in
      let lh_type = get_type_lst_expr_t(lh)
      and rh_type = get_type_lst_expr_t(rh) in
      let ty = assign_expr_result_type lh lh_type rh rh_type in
        if lst_match ty [Err] then 
         (* Assign_t(lh, rh, [Err])*)
          raise (Failure ("Assign mismatch: 
           Left side is " ^ (dbg_typ lh_type) ^ " Right
           side is " ^ (dbg_typ rh_type) ))
        else Assign_t(lh, rh, [Int])
  | Call (fname, actuals) ->
    let param = List.map tc_expr actuals
    and rettyp = (get_func_entry fname).ret_ty in
    let decl_typs = get_func_decl_typs fname in
    let param_typs = get_typs_from_expr_t_lst param in
    if cmp_param_typ param_typs decl_typs fname then
      Call_t(fname, param, rettyp)
    else
      raise (Failure ("Function " ^ fname ^ " is using arguments of
      type " ^ (dbg_typ_ll param_typs) ^ " but its declaration uses type " ^ 
      (dbg_typ_ll decl_typs))) 
  | Pointer(e) -> let v1 = tc_expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Pointer_t(v1, (List.tl v1_type))
  | Array(base,e) -> let v1 = tc_expr e in
    let b = tc_expr base in
    let v1_type = get_type_lst_expr_t(v1) in
      let btyp = get_type_lst_expr_t(b) in
      if is_int_or_char(v1_type) then 
        Array_t(b, v1, (List.tl btyp))
      else 
        raise (Failure ("Array index is type " ^ (dbg_typ v1_type) ^ " and not type int")) 
       (*  Array_t(base, v1, [Err] @ btyp ) *)
  | Addrof(e) -> let v1 = tc_expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Addrof_t(v1, [Ptr] @ v1_type)
  | Negof(e) -> let v1 = tc_expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      if is_int_or_char(v1_type) then
      Negof_t(v1, v1_type)
      else
        raise (Failure ("Wrong type " ^ (dbg_typ v1_type) 
            ^ " for unary minus")) 
      (* Negof_t(v1, [Err]) *)
  | Noexpr -> Noexpr_t ([Void])
    in
let rec tc_stmt = function
    Block sl ->
    (List.fold_left (fun str lst -> str @ lst) [] (List.map tc_stmt sl) )
  | Expr e -> [Expr_t (tc_expr e)]
  | Return e ->
    let v1 = tc_expr e in
    let v1_type = get_type_lst_expr_t(v1) in 
    let typ =  assign_result_type v1_type fdecl.ret in 
    if typ = [Err] then 
      raise (Failure ("Return type of function " ^ fdecl.fname ^
      " " ^ (dbg_typ fdecl.ret) ^ " does not match return type " ^
      (dbg_typ v1_type)))
    else 
      [Return_t(tc_expr e)]
  | If (p, t, f) -> 
    let v1 = tc_expr p and v2 = tc_stmt t and v3 = tc_stmt f in
    let v1_type = get_type_lst_expr_t(v1) in
      if is_int_or_char(v1_type) then
          [If_t(v1, Block_t(v2), Block_t(v3))]
      else
        raise (Failure ("If condition is type " 
      ^ (dbg_typ v1_type) ^ " and not type int")) 
  | While (e, b) ->
    let v1 = tc_expr e and v2 = tc_stmt b  in
    let v1_type = get_type_lst_expr_t(v1) in
      if is_int_or_char(v1_type) then
         [While_t(v1, Block_t(v2))]  
      else 
        raise (Failure ("While condition is type " 
      ^ (dbg_typ v1_type) ^ " and not type int")) 
  | For (asn, cmp, inc, b) -> 
    let asn_t = tc_expr asn and cmp_t = tc_expr cmp 
    and inc_t = tc_expr inc and stm_t = tc_stmt b in
    [For_t(asn_t, cmp_t, inc_t, Block_t(stm_t))]
in 

let stmtblock = (tc_stmt (Block fdecl.body)) in

let rec has_return stmt_lst = 
  match stmt_lst with
  | Return_t( _ )::tl -> true 
  | _ ::tl -> has_return tl 
  | [] -> false
in

(* Check return stmt exists if return type was declared  *)
(*if not(fdecl.ret = [Void]) && not(has_return stmtblock) then 
  raise (Failure ("Function " ^ fdecl.fname ^ ", has return type " ^ 
  (dbg_typ fdecl.ret) ^ " but no return statement found"))
else *)
  [Sast(fdecl.fname, (conv2_expr_t fdecl.formals), stmtblock) ] 
in

let env = { function_index = function_indexes;
		           global_index   = global_indexes;
               struct_index   = struct_indexes;
		           local_index    = StringMap.empty 
             }
in

(* Code executed to start the program *)
let entry_function = try
  (StringMap.find "main" function_indexes); []
  with Not_found -> raise (Failure ("no \"main\" function"))
in 

(* Compile the functions *)
(prog, List.concat (entry_function :: List.map (type_check_func env) functions));;
