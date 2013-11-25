open Ast
open Bytecode
open Debug
open Printexc

module StringMap = Map.Make(String)

type var_entry = { 
        offset:int;
        typ: cpitypes list
}

type func_entry = {
        param : var_entry list;
        ret_ty : cpitypes list
         }

type struct_entry = {
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

let rec get_size_type sindex = function 
        |[] ->   raise Exit
                | hd::tl -> (match hd with
                        Void -> 0
                        | Char -> 1
                        | Int
                        | Ptr -> 4
                        | Arr(sz) -> sz * (get_size_type sindex tl)
                        | Struct(sname) -> (StringMap.find sname 
                                sindex).size
                        | _ -> raise (Failure ("Requesting size of wrong
type")));;



let build_global_idx map = StringMap.empty;;

let calc_offset sidx offset typlst = let align_size = 4 in
                let offset = offset + get_size_type sidx typlst
                in match (List.hd typlst) with
                Char -> offset
                | _ ->  align_size *
                        int_of_float(ceil ((float_of_int offset ) /.
                        (float_of_int align_size)));;


let rec build_local_idx map sidx offset = (function
       [] -> map
       | hd:: tl -> offset := (calc_offset sidx !offset hd.vtype);
                                build_local_idx (StringMap.add hd.vname
                {offset = !offset; typ=hd.vtype} map) sidx offset tl);;


(* Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate prog =
        let structs = prog.sdecls 
        and globals = prog.gdecls
        and functions = prog.fdecls in

        (* Allocate "addresses" for each global variable *)
  (* TODO Code generation for globals *)
  let global_indexes = build_global_idx globals in
  let struct_indexes = List.fold_left (fun map stct ->
                let soffset = ref 0 in
                let index = build_local_idx StringMap.empty 
                map soffset stct.smembers in
                (StringMap.add stct.sname
                {size = !soffset; memb_index = index} map))
                StringMap.empty structs
          in
  (*TODO: Add the buil-in-function printf to the below list *)
(* let built_in_functions = StringMap.add "print" (-1) StringMap.empty in *)
let function_indexes = List.fold_left 
        (fun map fdecl ->
           let rec var_to_lst ind = function
                  [] -> []
                | hd:: tl -> (
                        {offset =0 ;(*TODO Check correct values*)
                        typ = hd.vtype
                        } :: (var_to_lst (ind+1) tl)
                ) in
          StringMap.add fdecl.fname 
                {
                   param = (var_to_lst 0 fdecl.formals);
                   ret_ty = fdecl.ret
                } map
        )
        StringMap.empty functions
in

(* Translate a function in AST form into a list of bytecode statements *)
let translate env fdecl=
    let curr_offset = ref 0
        and count_loop = ref 0
        and count_ifelse = ref 0
                in
        let env = { env with local_index =
                (build_local_idx StringMap.empty env.struct_index
                curr_offset (fdecl.locals @ fdecl.formals)) }
                in
        let add_temp typlst =
                curr_offset := (calc_offset env.struct_index !curr_offset
                typlst);
                Lvar(!curr_offset,(get_size_type env.struct_index typlst))
                in
        let get_func_entry name = (try
                        StringMap.find name env.function_index
                with Not_found -> raise (Failure("Function not found : " ^ name))) 
                in
        let get_type_varname table varname =
                (try
                (StringMap.find varname table).typ
                with Not_found -> raise (Failure("Varname not found")
                ))
                in
        let get_size_varname table varname =
                get_size_type env.struct_index (get_type_varname table varname)
                in
        let get_lvar_varname table strict var = 
                (try
                        Lvar( (StringMap.find var table).offset,
                        (get_size_varname table var))
                 with Not_found -> (try
                         if (strict = 0) then (
                         Gvar(var,
                        (get_size_varname table var))
                         )
                         else
                                 raise Not_found
                 with Not_found -> raise (Failure(var ^": Not found"))))
                in
        (*let get_varname_size ?(bt = false) ?(ix = -1) varname = 
                match (get_var ~bty:bt ~idx:ix varname) with
                Lvar(i,s,c) -> s
                |Gvar(vn,s) -> s
                in *)
        let get_ptrsize_type typlst =
                get_size_type env.struct_index (List.tl typlst)
                in
        let get_ptrsize_varname table varname =
                get_size_type env.struct_index (List.tl (get_type_varname table varname))
                in
        let get_binres_type e = match List.hd e with
                BinRes(typ) -> typ
                | _ -> raise (Failure("Unexpted type: Expected BinRes"))
                in
        let gen_binres_type typ = 
                [BinRes(typ)]
                in
        let get_dom_type typ1 typ2 =
                ( match List.hd typ1 with
                Ptr | Arr(_) -> typ1 
                | _ -> (match List.hd typ2 with
                        Ptr | Arr(_) -> typ2
                        | _ -> (if (get_size_type env.struct_index typ1) <= 
                                   (get_size_type env.struct_index typ2) 
                           then typ2 else typ1)
                        )
                )
        in
        let rec conv2_byt_lvar = function
                [] -> []
                | hd::tl -> let entry = StringMap.find hd.vname
                env.local_index 
                in Lvar(entry.offset, 
                (get_size_type env.struct_index entry.typ))
                :: (conv2_byt_lvar tl) 
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
        let get_off_lvar lvar = match lvar with
                Lvar(o,s) -> o
                | _ -> raise(Failure("Needs implementation"))
                in
        let get_atom = function
                Atom (atm) -> atm
                | BinEval  (dst, var1, op, var2) -> dst
                | Fcall (fname, args,ret ) -> ret
                | Assgmt (dst, src) -> dst 
                | Label (a)-> raise (Failure ("Unexpected: Label-> " ^ a))
                | Predicate (_, _, _)-> raise (Failure ("Unexpected: Predicate"))
                | Branch _-> raise (Failure ("Unexpected: Branch"))
                | Mov (_, _)-> raise (Failure ("Unexpected: Mov"))
                | Ldr (_, _)-> raise (Failure ("Unexpected: Ldr"))
                | Str (_, _)-> raise (Failure ("Unexpected: Str"))
                | BinRes(ty) -> raise (Failure ("Unexpected: BinRes " ^
                  dbg_str_of_typs (List.hd ty)))
                |Rval _ -> raise (Failure ("Unexpected: Rval"))
                in
        let incr_by_ptrsz exp incrsz tmp = [BinEval (tmp, (Lit incrsz),
                         Mult,(get_atom(List.hd (List.rev exp))))]
                in
        let get_smemb_table stct =(try
                let stable = StringMap.find stct env.struct_index
                with Not_found -> 
                raise(Failure(" struct " ^ stct ^ " is not a type")))
                in
        let incr_by_offset atm off = match atm with
                Lvar(o,s) ->
                | Pntr(lvar,s) ->
                | _ -> raise(Failure("Needs implementation"))
                in
        let gen_addr_lst v1 = gen_binres_type( (get_binres_type v1))
                @ v1 @ gen_atom (Addr(get_atom (List.hd(List.rev v1))))
                in
        let add_base_offset btyp baddr off =
                let v3 = add_temp btyp in
                let v4 = get_ptrsize_type btyp
                [BinEval (v3,baddr,Add,off)] @ (gen_atom (Pntr(v3,v4)))
                in
let rec expr ?(table = env.local_index) ?(strict=0) = function
        Literal i -> (gen_binres_type [Int]) @ gen_atom (Lit i)
      | String s -> gen_atom (Sstr s) (*TODO return (gen_binres_type [Arr()
      char]*)
      | ConstCh(ch) -> (gen_binres_type [Char]) @ gen_atom(Cchar(ch.[1]))
      | Id s ->
                let retyp = get_type_varname table s in
                let v1 = (gen_binres_type(retyp)) @
                                gen_atom(get_lvar_varname table strict s)
                in (match List.hd retyp with
                        Arr(_) -> gen_addr_lst v1
                        | _ -> v1)
      | MultiId(fexpr,resolve,e) ->
                let v1 = expr fexpr in
                let tab = (match List.hd (get_binres_type v1) with
                  Struct(s) -> get_smemb_table s
                  | _ -> raise(Failure("Must be a struct"))) in
                let v2 = expr ~table:tab ~strict:1 e in
                let offset = (match get_atom(List.hd (List.rev v2)) with
                   Lvar(o,s) -> List.rev(List.tl(List.rev v2)) @ [Lit o]
                   | Pntr(b,s) -> (*This will an array *)
                      (match (List.nth (List.rev v2) 2) with
                        BinEval(dst,op1,op,op2) -> 
                          (List.rev(List.tl(List.tl(List.rev v2)))) @
                          [BinEval(dst,(Lit (get_off_lvar op1)),Add,op2)]
                          @ dst
                        | _ -> raise(Failure("Array was expected: MultiId"))
                        )
                   | _ -> raise(Failure("Unexpected type in MultiId"))) in
                let baddr = (match get_atom( List.hd (List.rev v1)) with
                        Lvar(o,s) as l -> Addr(l)
                        | Pntr(b,s) -> b
                        | _ -> raise(Failure("Unexpected type in MultiId"))) in
                 offset @ (add_base_offset (get_binres_type offset) baddr
                           (List.hd (List.rev offset)))
      | Binop (e1, op, e2) -> let v1 = expr e1
                                and v2 = expr e2 in
                let v1binres = get_binres_type v1
                and v2binres = get_binres_type v2 in
                let binres = get_dom_type v1binres v2binres in 
                let v3 = (add_temp binres) in
                (gen_binres_type binres) @ (gen_atom v3) @ (List.tl v1) @
                (List.tl v2) @
                (match List.hd binres with
                Ptr | Arr(_) -> 
                    (match List.hd v1binres with 
                      Ptr | Arr(_) -> (let tmp = (add_temp v2binres) in 
                      (incr_by_ptrsz v2 (get_size_type env.struct_index 
                      (List.tl v1binres)) tmp) @ 
                      [BinEval (v3 ,(get_atom (List.hd (List.rev v1))), op, tmp)])
                      | _ -> 
                        (match List.hd v2binres with
                         Ptr | Arr(_) ->
                          let tmp = ((add_temp v1binres)) in
                          (incr_by_ptrsz v1 (get_size_type env.struct_index 
                          (List.tl v2binres)) tmp) @
                          [BinEval (v3 ,tmp, op,(get_atom 
                          (List.hd (List.rev v2))))]
                         | _ -> raise(Failure("Cannot reach here"))
                             )
                        )
                | _ -> [BinEval (v3 ,(get_atom (List.hd (List.rev v1))), op,
                (get_atom(List.hd (List.rev v2))))])
      | Assign (s, e) ->
                      let v1 = (expr e)
                      and v2 = (expr s)
                      in (gen_binres_type (get_binres_type v2)) 
                      @ v1 @ v2 @
                [Assgmt ((get_atom(List.hd (List.rev v2))),get_atom (List.hd
                (List.rev v1)))]
      | Call (fname, actuals) ->
                let param = List.map expr (List.rev actuals)
                and rettyp = (get_func_entry fname).ret_ty in
                let ret = (add_temp rettyp ) in 
                (gen_binres_type rettyp)@
                (gen_atom ret) @ List.concat param @ 
                [Fcall (fname,List.rev
                (List.map (fun par -> get_atom (List.hd (List.rev
                par))) param)
                ,ret)]
      | Pointer(e) -> let v1 = expr e in 
                 let binresv1 = (get_binres_type v1) in
                v1 @ gen_atom (Pntr( (get_atom (List.hd (List.rev v1))),
                (get_ptrsize_type binresv1)))
      | Array(base,e) -> let v1 = expr e in
                         let off = add_temp (get_binres_type v1) in
                         let btyp = (get_type_varname table base) in
                         let v4 = get_ptrsize_type btyp in 
                         let baddr = Addr(get_lvar_varname table strict base) in
                         gen_binres_type(List.tl (get_type_varname table base)) @
                         (incr_by_ptrsz v1 v4 off) @ 
                         (add_base_offset btyp baddr off)
      | Addrof(v) -> let v1 = expr v in gen_addr_lst v1
      | Noexpr ->[]
    in
let rec stmt = function
	Block sl     ->
                (List.fold_left (fun str lst -> str @ lst) [] 
                        (List.map stmt sl) )
                        (*stmt sl*)
      | Expr e       -> expr e
      | Return e     -> let v1 = expr e in 
                        v1 @ 
                        [Rval (get_atom (List.hd (List.rev v1)))]
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
      let stmtblock = (stmt (Block fdecl.body))
      in
[Fstart (
            fdecl.fname,
            (conv2_byt_lvar fdecl.formals),
            stmtblock,
            !curr_offset
    )]

in let env = { function_index = function_indexes;
		 global_index = global_indexes;
                 struct_index = struct_indexes;
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
