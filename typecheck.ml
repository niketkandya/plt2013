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
      ( StringMap.add hd.vname
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

(* Add the built-in-function printf, scanf to the function indexes *)
let f2_index = 
  StringMap.add "printf" 
  {
    param = [];
    ret_ty = [Void]
  }
  f_index
in

let function_indexes =
  StringMap.add "scanf" 
  {
    param = [];
    ret_ty = [Void]
  }
  f2_index
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
  let get_func_entry name = 
    try StringMap.find name env.function_index
    with Not_found -> raise (Failure("Function not found : " ^ name)) 
    in
  let get_type_varname table varname = 
    try (StringMap.find varname table).typ
    with Not_found -> raise (Failure("Varname not found"))
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
    | Noexpr_t -> [Err]
    in
(*  let get_size_varname table varname =
    get_size_type env.struct_index (get_type_varname table varname)
    in
  let get_lvar_varname table strict var = 
    try Lvar((StringMap.find var table).offset, (get_size_varname table var))
    with Not_found -> 
      try
        if strict = 0 then 
          Gvar(var,(get_size_varname table var)) 
        else raise Not_found
      with Not_found -> raise (Failure(var ^": Not found"))
    in
*)  
  let get_ptrsize_type typlst =
    get_size_type env.struct_index (List.tl typlst)
    in
  let print_types ty1 ty2 =
      "left hand is: " ^ (dbg_typ ty1) ^ " right hand is: "
         ^ (dbg_typ ty2) ^ "\n" 
    in
  let rec lst_match list1 list2 = match list1, list2 with
    | h1::t1, h2::t2 -> h1 = h2 && lst_match t1 t2
    | [_], _ -> false
    | _, [_] -> false
    | _, _ -> true
    in
  let is_int_or_char ty =
    if lst_match ty [Int] then true 
      else if lst_match ty [Char] then true
      else false
    in
  let binop_result_type ty1 op ty2 =
        match ty1, ty2, op with
        | [Int],  [Int],  _ -> [Int]
        | [Char], [Char], _ -> [Char]
        | [Int],  [Char], _ -> [Int]
        | [Char], [Int],  _ -> [Int]
        | Ptr::tl, [Int], Add -> ty1
        | Ptr::tl, [Char], Add -> ty1
        | Ptr::tl, [Int], Sub -> ty1
        | Ptr::tl, [Char], Sub -> ty1
        | [Int], Ptr::tl, Add -> ty2
        | [Char], Ptr::tl, Add -> ty2
        | [Int], Ptr::tl, Sub -> ty2
        | [Char], Ptr::tl, Sub -> ty2
        | _ , _ , _ -> [Err]
    in
  let assign_result_type ty1 ty2 =
    if lst_match ty1 ty2 then ty1
    else
       match ty1, ty2 with
        | [Int],  [Char] -> [Int]
        | [Char], [Int] -> [Int]
        | _ , _  -> [Err]
    in
(*  let get_ptrsize_varname table varname =
    get_size_type env.struct_index (List.tl (get_type_varname table varname))
    in
  let get_dom_type typ1 typ2 =
    ( match List.hd typ1 with
      Ptr 
    | Arr(_) -> typ1 
    | _ -> (match List.hd typ2 with
            Ptr | Arr(_) -> typ2
            | _ -> (if (get_size_type env.struct_index typ1) <= 
                       (get_size_type env.struct_index typ2) 
                        then typ2 else typ1)
           )
    )
    in
  let raise_error_atom a = 
    match a with
      Lit (i) -> raise(Failure("Literal " ^ string_of_int i))
    | Cchar(ch) -> raise(Failure("Const Char"))
    | Sstr (s, l) -> raise(Failure("StringConst "^s))
    | Lvar (o,s) -> raise(Failure(" Lvar"))
    | Gvar (_,_) -> raise(Failure("Gvar"))
    | Pntr (_,_) -> raise(Failure("Pntr"))
    | Addr (_) -> raise(Failure("Addr"))
    | Debug (_)  -> raise(Failure("Debug"))
    | Neg (_) -> raise(Failure("Negative"))
    in
    *)
  let get_struct_table stct =
    (try (StringMap.find stct env.struct_index).memb_index
     with Not_found -> raise(Failure(" struct " ^ stct ^ " is not a type")))
    in 
let rec tc_expr ?(table = env.local_index) ?(strict=0) = function
    Literal i -> Literal_t(i, [Int])
  | String s -> String_t(s, [Ptr; Char])
  | ConstCh(ch) -> ConstCh_t(ch, [Char])
  | Id s ->
    let typ = get_type_varname table s in
        Id_t(s, typ)
  | MultiId(fexpr,resolve,e) ->
    let v1 = tc_expr fexpr in
      let v1_type = get_type_lst_expr_t(v1) in
      let tab = (match v1_type with
        | [Struct(s)] -> get_struct_table s
        | _ -> raise(Failure("Variable is not a Struct"))) in
      let v2 = tc_expr ~table:tab ~strict:1 e in
      let v2_type = get_type_lst_expr_t(v2) in
      (match resolve with
        | Dot -> MultiId_t(v1, Dot, v2, v2_type)
        | Ind -> MultiId_t(v1, Ind, v2, [Ptr] @ v2_type))
  | Binop (e1, op, e2) -> 
    let lh = tc_expr e1 and rh = tc_expr e2 in
      let lh_type = get_type_lst_expr_t(lh)
      and rh_type = get_type_lst_expr_t(rh) in
      let ty = binop_result_type lh_type op rh_type in
        if lst_match ty [Err] then
         (* Binop_t(lh, op, rh, [Err]) *)
          raise (Failure ("Binop mismatch: 
           Left side is " ^ (dbg_typ lh_type) ^ " Right
           side is " ^ (dbg_typ rh_type) )) 
        else Binop_t(lh, op, rh, ty)
  | Assign (s, e) ->
    let lh = (tc_expr s) and rh = (tc_expr e) in
      let lh_type = get_type_lst_expr_t(lh)
      and rh_type = get_type_lst_expr_t(rh) in
      let ty = assign_result_type lh_type rh_type in
        if lst_match ty [Err] then 
         (* Assign_t(lh, rh, [Err])*)
          raise (Failure ("Assign mismatch: 
           Left side is " ^ (dbg_typ lh_type) ^ " Right
           side is " ^ (dbg_typ rh_type) ))
        else Assign_t(lh, rh, [Int])
  | Call (fname, actuals) ->
    let param = List.map tc_expr (List.rev actuals)
    and rettyp = (get_func_entry fname).ret_ty in
    (* TODO check function return parameters to make sure
     * they match *)
      Call_t(fname, param, rettyp)
  | Pointer(e) -> let v1 = tc_expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Pointer_t(v1, (List.tl v1_type))
  | Array(base,e) -> let v1 = tc_expr e in
    let b = tc_expr base in
    let v1_type = get_type_lst_expr_t(v1) in
      let btyp = get_type_lst_expr_t(b) in
     (* if is_int_or_char(v1_type) then *)
        Array_t(b, v1, (List.tl btyp))
     (* else 
        raise (Failure ("Array index is type " ^ (dbg_typ v1_type) 
            ^ " and not type int")) *)
       (*  Array_t(base, v1, [Err] @ btyp ) *)
  | Addrof(e) -> let v1 = tc_expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Addrof_t(v1, [Ptr] @ v1_type)
  | Negof(e) -> let v1 = tc_expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      if is_int_or_char(v1_type) then
        raise (Failure ("Wrong type " ^ (dbg_typ v1_type) 
            ^ " for unary minus")) 
      (* Negof_t(v1, [Err]) *)
      else
      Negof_t(v1, v1_type)
  | Noexpr -> Noexpr_t 
    in
let rec tc_stmt = function
    Block sl ->
    (List.fold_left (fun str lst -> str @ lst) [] (List.map tc_stmt sl) )
  | Expr e -> [Expr_t (tc_expr e)]
  | Return e -> [Return_t(tc_expr e)]
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
  | _ -> []
in 

let stmtblock = (tc_stmt (Block fdecl.body)) in


(*[Global([Debug("Debug Message"); Debug("Yellow")])] @*)
(*[Sast(fdecl.fname, [Var(Noexpr, [Void])], [Expr(Var(Noexpr, [Void]))]) ] *)
[Sast(fdecl.fname, [Noexpr_t], stmtblock) ] 
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
