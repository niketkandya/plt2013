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
  | Arr(sz) -> sz * (get_size_type sindex tl)
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
      Var(expr, typ) -> typ 
    in
  let get_type_first_expr_t = function 
      Var(expr, typ) -> List.hd (typ) 
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
  let print_type ty = 
      (List.fold_left (fun s t -> s ^ (dbg_str_of_typs t)) "" ty)
    in
  let print_types ty1 ty2 =
      "left hand is: " ^ (print_type ty1) ^ " right hand is: "
         ^ (print_type ty2) ^ "\n" 
    in
  let rec lst_match list1 list2 = match list1, list2 with
    | h1::t1, h2::t2 -> h1 = h2 && lst_match t1 t2
    | [_], _ -> false
    | _, [_] -> false
    | _, _ -> true
    in
  let predicate_correct ty =
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
  let get_struct_table stct =
    (try (StringMap.find stct env.struct_index).memb_index
     with Not_found -> raise(Failure(" struct " ^ stct ^ " is not a type")))
    in *)
let rec expr ?(table = env.local_index) ?(strict=0) = function
    Literal i -> Var(Literal(i), [Int])
  | String s -> Var(String(s), [Ptr; Char])
  | ConstCh(ch) -> Var(ConstCh(ch), [Char])
  | Id s ->
      let typ = get_type_varname table s in
        Var(Id(s), typ)
(*      | MultiId(fexpr,resolve,e) ->
                let v1 = expr fexpr in
                let tab = (match List.hd (get_binres_type v1) with
                  Struct(s) -> get_struct_table s
                  | _ -> raise(Failure("Must be a struct"))) in
                let v2 = expr ~table:tab ~strict:1 e in
                let offset = (match get_atom(List.hd (List.rev v2)) with
                   Lvar(o,s) -> List.rev(List.tl(List.rev v2)) @
                   gen_atom (Lit o)
                   | Pntr(b,s) -> (*This will an array *)
                      (match (List.nth (List.rev v2) 1) with
                        BinEval(dst,op1,op,op2) -> 
                          (List.rev(List.tl(List.tl(List.rev v2)))) @
                          [BinEval(dst,(get_off_lvar op1),Add,op2)]
                          @ gen_atom dst
                        | _ -> raise(Failure("Array was expected: MultiId"))
                        )
                   | _ -> raise(Failure("Unexpected type in MultiId"))) in
                let baddr = (match get_atom( List.hd (List.rev v1)) with
                        Lvar(o,s) as l -> Addr(l)
                        | Pntr(b,s) -> b
                        | _ -> raise(Failure("Unexpected type in MultiId"))) in
                        List.rev(List.tl(List.rev offset))
                        @ (add_base_offset ( List.hd (get_binres_type offset) 
                        ::(get_binres_type offset)) 
                        baddr (get_atom (List.hd (List.rev offset))))
*)
  | Binop (e1, op, e2) -> 
    let lh = expr e1 and rh = expr e2 in
      let lh_type = get_type_lst_expr_t(lh)
      and rh_type = get_type_lst_expr_t(rh) in
      let ty = binop_result_type lh_type op rh_type in
        if lst_match ty [Err] then raise 
          (Failure ("Binop mismatch: 
           Left side is " ^ (print_type lh_type) ^ " Right
           side is " ^ (print_type rh_type) ))
        else Var(Binop(e1, op, e2), ty)
  | Assign (s, e) ->
    let lh = (expr s) and rh = (expr e) in
      let lh_type = get_type_lst_expr_t(lh)
      and rh_type = get_type_lst_expr_t(rh) in
      let ty = assign_result_type lh_type rh_type in
        if lst_match ty [Err] then raise 
          (Failure ("Assign mismatch: 
           Left side is " ^ (print_type lh_type) ^ " Right
           side is " ^ (print_type rh_type) ))
        else Var(Assign(s, e), [Int])
  | Call (fname, actuals) ->
    let param = List.map expr (List.rev actuals)
    and rettyp = (get_func_entry fname).ret_ty in
    (* TODO check function return parameters to make sure
     * they match *)
      Var(Call(fname, actuals), rettyp)
  | Pointer(e) -> let v1 = expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Var(Pointer(e), v1_type)
  | Array(base,e) -> let v1 = expr e in
    let v1_type = get_type_lst_expr_t(v1) in
      let btyp = (get_type_varname table base) in
      if predicate_correct(v1_type) then
          Var(Array(base, e), (List.tl btyp))
      else raise (Failure ("Array index is type " ^ (print_type v1_type) 
            ^ " and not type int"))
  | Addrof(e) -> let v1 = expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Var(Addrof(e), v1_type)
  | Negof(e) -> let v1 = expr e in 
    let v1_type = get_type_lst_expr_t(v1) in
      Var(Negof(e), v1_type)
  | Noexpr -> Noexpr_t 
  | _ -> Noexpr_t 
    in
let rec stmt = function
    Block sl ->
    (List.fold_left (fun str lst -> str @ lst) [] (List.map stmt sl) )
  | Expr e -> [Expr_t (expr e)]
  | Return e -> [Return_t(expr e)]
(*  | If (p, t, f) -> 
    let v1 = expr p and v2 = stmt t and v3 = stmt f in
      [If_t(v1, v2, v3)]
  | While (e, b) ->
    let v1 = expr e and v2 = stmt b  in
       [While_t(v1, v2)]  *)
  | _ -> []
in 

let stmtblock = (stmt (Block fdecl.body)) in


(*[Global([Debug("Debug Message"); Debug("Yellow")])] @*)
(*[Sast(fdecl.fname, [Var(Noexpr, [Void])], [Expr(Var(Noexpr, [Void]))]) ] *)
[Sast(fdecl.fname, [Var(Noexpr, [Void])], stmtblock) ] 
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
