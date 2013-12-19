open Sast
open Ast
open Bytecode
open Debug
open Printexc

module StringMap = Map.Make(String)

let err str = raise(Failure("Compile: "^ str));;

let rec get_size_type sindex = function 
|[] ->   raise Exit
| hd::tl -> 
  (match hd with
    Void -> 0
  | Char -> 1
  | Int
  | Ptr -> 4
  | Arr(sz) -> (match sz with
        Literal(i) -> i
        | Id(id) -> get_size_type sindex [Ptr]
        | _ -> err "lit_to_num: unexpected") * (get_size_type sindex tl)
  | Struct(sname) -> (StringMap.find sname sindex).size
  | _ -> err "Requesting size of wrong type");;

let get_atom = function
      Atom (atm) -> atm
    | BinEval  (dst, var1, op, var2) -> dst
    | Fcall (fname, args,ret ) -> ret
    | Assgmt (dst, src) -> dst 
    | Label (a)-> err ("Unexpected: Label-> " ^ a)
    | Predicate (_, _, _)-> err "Unexpected: Predicate"
    | Branch _-> err "Unexpected: Branch"
    | Mov (_, _)-> err "Unexpected: Mov"
    | Ldr (_, _)-> err "Unexpected: Ldr"
    | Str (_, _)-> err "Unexpected: Str"
    | BinRes(ty) -> err ("Unexpected: BinRes " ^ 
      dbg_str_of_typs (List.hd ty))
    |Rval _ -> err "Unexpected: Rval"
    | VarArr(_,_) -> err "Unexpected: VarArr";;

let build_global_idx map = StringMap.empty;;
let gl_atm a = get_atom(List.hd ( List.rev a));;

let calc_offset sidx offset typlst = 
  let align_size = 4 in
  let offset = offset + get_size_type sidx typlst in
    match (List.hd typlst) with
      Char -> offset
    | _ ->  align_size * int_of_float(ceil ((float_of_int offset ) /.(float_of_int align_size)));;

let rec modify_formal_lst = function 
    [] -> []
    | hd :: tl -> ( (match List.hd (hd.vtype) with
        Arr(_)-> { hd with vtype = Ptr :: List.tl hd.vtype }
        |  _ -> hd ) :: (modify_formal_lst tl));;

let rec modify_local_lst  = function 
    [] -> []
    | hd :: tl -> ( (match List.hd (hd.vtype) with
        Arr(s)-> (match s with 
              Id(id) -> { hd with vtype = Ptr :: List.tl hd.vtype }
              | _ -> hd)
        |  _ -> hd ) :: (modify_local_lst tl));;


let rec build_local_idx map sidx offset ?(rev =0) = (function
    [] -> map
  | hd:: tl ->
    offset := (calc_offset sidx !offset hd.vtype);
    build_local_idx ~rev:rev 
    ( StringMap.add hd.vname
      {
        offset = !offset - (if rev =0 then rev else (get_size_type sidx hd.vtype));
        typ = hd.vtype
      } map
    )
    sidx offset tl);;

(* Translate a program in AST form into a bytecode program.  Throw an
 *   exception if something is wrong, e.g., a reference to an unknown
 *   variable or function *)
let translate prog =
(*
let getProg(a, b) = a in
let prog = getProg(sast) in
*)
let structs = prog.sdecls 
  and globals = prog.gdecls
  and functions = prog.fdecls in
  let count_loop = ref 0 
  and count_mem = ref (-1) 
  and count_ifelse = ref 0
  and count_label = ref 0 in

(* Allocate "addresses" for each global variable *)
(* TODO Code generation for globals *)
let global_indexes = build_global_idx globals in
(* Build structure specific symbol table*)
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
    ret_ty = [Int]
  }
  f3_index
in

let function_indexes =
  StringMap.add "free" 
  {
    param = [];
    ret_ty = [Int]
  }
  f4_index
in
(* Translate a function in AST form into a list of bytecode statements *)
let translate env fdecl=
  let curr_offset = ref 0 in

  let env = 
    {
      env with local_index = 
        (build_local_idx StringMap.empty env.struct_index curr_offset 
        ( (modify_local_lst fdecl.locals) 
        @ (modify_formal_lst fdecl.formals)))
    }
    in
  let add_temp typlst =
    curr_offset := (calc_offset env.struct_index !curr_offset typlst);
    Lvar(!curr_offset,(get_size_type env.struct_index typlst))
    in
  let get_func_entry name = 
    try StringMap.find name env.function_index
    with Not_found -> err ("Function not found : " ^ name) 
    in
  let get_type_varname table varname = 
    try (StringMap.find varname table).typ
    with Not_found -> err ("Varname not found: "^varname^(string_of_int
    (StringMap.cardinal table)))
    in
  let get_size_varname table varname =
    get_size_type env.struct_index (get_type_varname table varname)
    in
  let get_lvar_varname table strict var = 
    try Lvar((StringMap.find var table).offset, (get_size_varname table var))
    with Not_found -> 
      try
        if strict = 0 then 
          Gvar(var,(get_size_varname table var)) 
        else raise Not_found
      with Not_found -> err (var ^": Not found")
    in
  let get_ptrsize_type typlst =
    get_size_type env.struct_index (List.tl typlst)
    in
  let get_ptrsize_varname table varname =
    get_size_type env.struct_index (List.tl (get_type_varname table varname))
    in
  let get_binres_type e = 
    match List.hd e with
      BinRes(typ) -> typ
    | _ -> err "Unexpted type: Expected BinRes"
    in
  let gen_binres_type typ = 
    [BinRes(typ)]
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
      Lit (i) -> err ("Literal " ^ string_of_int i)
    | Cchar(ch) -> err "Const Char"
    | Sstr (s, l) -> err ("StringConst "^s)
    | Lvar (o,s) -> err " Lvar"
    | Gvar (_,_) -> err "Gvar"
    | Pntr (_,_) -> err "Pntr"
    | Addr (_) -> err "Addr"
    | Debug (_)  -> err "Debug"
    | Neg (_) -> err "Negative"
    in
  let rec conv2_byt_lvar = function
      [] -> []
    | hd::tl -> let entry = StringMap.find hd.vname env.local_index in
      Lvar(entry.offset, (get_size_type env.struct_index entry.typ))
                :: (conv2_byt_lvar tl) 
    in
  let get_loop_label num = "loop" ^ match num with
      0 -> string_of_int (count_loop := !count_loop + 1; !count_loop) ^ "_start"
    | 1 -> string_of_int !count_loop ^ "_end"
    | _ -> ""
    in
  let get_ifelse_label num = 
    match num with 
      0 -> "else" ^ string_of_int (count_ifelse := !count_ifelse + 1; !count_ifelse)
    | 1 -> "end" ^ string_of_int !count_ifelse
    | _ -> ""
    in
  let gen_atom atm = 
    [Atom (atm)]
    in
  let rec get_off_lvar lvar = 
    match lvar with
      Lvar(o,s) -> Lit o
    | Addr(l) -> get_off_lvar l
    | _ as a -> raise_error_atom a
    in
  let incr_by_ptrsz exp incrsz tmp = 
     [BinEval (tmp, (Lit incrsz), Mult, (gl_atm exp))]
    in
  let get_struct_table stct =
    (try (StringMap.find stct env.struct_index).memb_index
     with Not_found -> err (" struct " ^ stct ^ " is not a type"))
    in
  let gen_addr_lst v1 = v1 @
    gen_atom (Addr(gl_atm v1))
    in
  let add_base_offset btyp baddr off =
    let v3 = add_temp btyp in
    let v4 = get_ptrsize_type btyp in
    [BinEval (v3,baddr,Add,off)] @ (gen_atom (Pntr(v3,v4)))
    in
  let rec gen_vararr = function
    [] -> []
    | hd :: tl -> (match List.hd (hd.vtype) with
        Arr(s)-> (match s with
              Id(id) -> let tmp = 
                add_temp (List.tl hd.vtype)
                in
               (incr_by_ptrsz 
              (gen_atom (get_lvar_varname env.local_index 0 id))
              (get_ptrsize_type hd.vtype) tmp) @
                [VarArr((get_lvar_varname env.local_index 0 hd.vname),
                     tmp)]
              | _ -> [])
        |  _ -> []) @ (gen_vararr tl)
    in

    let binop_rest v1 v2 v1binres v2binres binres v3 op= 
      (gen_binres_type binres) @ (gen_atom v3) @ (List.tl v1) @
                (List.tl v2) @
                (match List.hd binres with
                Ptr | Arr(_) ->
                    (match List.hd v1binres with
                      Ptr | Arr(_) -> (let tmp = (add_temp v2binres) in
                       (incr_by_ptrsz v2 (get_size_type env.struct_index
                       (List.tl v1binres)) tmp) @ 
                       [BinEval (v3 ,(gl_atm v1), op, tmp)])
                      | _ -> (match List.hd v2binres with
                         Ptr | Arr(_) ->
                          let tmp = ((add_temp v1binres)) in
                          (incr_by_ptrsz v1 (get_size_type env.struct_index 
                          (List.tl v2binres)) tmp) @
                          [BinEval (v3 ,tmp, op,(gl_atm v2))]
                         | _ -> err "Cannot reach here" )
                      )
                | _ -> [BinEval (v3 ,(gl_atm v1), op,
                (gl_atm v2))])
    in
    (* Advantage of using bytecode: While implementing && and ||
     * It was easier to define the login in a slightly higher level
     * language than assembly *)
    let binop_logical  v1 v2 res op = let opvalue = (match op with
        Lor -> true
        | Land -> false
        | _ -> err "Logical only")in
        let endlbl =
      "lend" ^ string_of_int (
        count_label := !count_label + 1;
       !count_label) in
        [Assgmt (res,Lit(if opvalue then 1 else 0))] @ v1 @ 
        [Predicate ((gl_atm v1), opvalue, endlbl)] @ v2 @
        [Predicate ((gl_atm v2), false, endlbl)] @
        [Assgmt (res,Lit(if opvalue then 0 else 1))] @ [Label endlbl] @ 
        gen_atom(res)
    in
let rec expr ?(table = env.local_index) ?(strict=0) = function
        Literal i -> (gen_binres_type [Int]) @ gen_atom (Lit i)
      | String s -> 
                let lbl = incr count_mem; ".LC" ^
                (string_of_int !count_mem) in
                (gen_binres_type [Ptr;Char]) @ gen_atom(Sstr(s, lbl))
      | ConstCh(ch) -> (gen_binres_type [Char]) @ gen_atom(Cchar(ch.[1]))
      | Id s ->
                let retyp = get_type_varname table s in
                let v1 = (gen_binres_type(retyp)) @
                         gen_atom(get_lvar_varname table strict s) in 
                (match List.hd retyp with
                        Arr(_) -> gen_addr_lst v1
                        | _ -> v1)
      | MultiId(fexpr,Ind, e) -> expr (MultiId(Pointer(fexpr), Dot, e))
      | MultiId(fexpr,Dot,e) ->
                let v1 = expr fexpr in
                let tab = (match List.hd (get_binres_type v1) with
                  Struct(s) -> get_struct_table s
                  | _ -> err "Must be a struct") in
                let v2 = expr ~table:tab ~strict:1 e in
                let offset = (match gl_atm v2 with
                   Lvar(o,s) -> List.rev(List.tl(List.rev v2)) @
                   gen_atom (Lit o)
                   | Pntr(b,s) -> (*This will an array *)
                      (match (List.nth (List.rev v2) 1) with
                        BinEval(dst,op1,op,op2) -> 
                          (List.rev(List.tl(List.tl(List.rev v2)))) @
                          [BinEval(dst,(get_off_lvar op1),Add,op2)]
                          @ gen_atom dst
                        | _ -> err "Array was expected: MultiId")
                        
                   | _ -> err "Unexpected type in MultiId") in
                let baddr = (match gl_atm v1 with
                        Lvar(o,s) as l -> Addr(l)
                        | Pntr(b,s) -> b
                        | _ -> err "Unexpected type in MultiId") in
                        List.rev(List.tl(List.rev offset))
                        @ (add_base_offset ( List.hd (get_binres_type offset) 
                        ::(get_binres_type offset)) 
                        baddr (gl_atm offset))
      | Binop (e1, op, e2) -> let v1 = expr e1
                                and v2 = expr e2 in
                let v1binres = get_binres_type v1
                and v2binres = get_binres_type v2 in
                let binres = get_dom_type v1binres v2binres in 
                let res = (add_temp binres) in
                (match op with
                  Lor |Land -> binop_logical v1 v2 res op
                  | _ ->  binop_rest v1 v2 v1binres v2binres binres res op
                      )
      | Assign (s, e) ->
                      let v1 = (expr e) and v2 = (expr s)
                      in (gen_binres_type (get_binres_type v2)) 
                      @ v1 @ v2 @
                [Assgmt ((gl_atm v2),gl_atm v1)]
      | Call (fname, actuals) ->
                let param = List.map expr (List.rev actuals)
                and rettyp = (get_func_entry fname).ret_ty in
                let ret = (add_temp rettyp ) in 
                (gen_binres_type rettyp)@
                (gen_atom ret) @ List.concat param @ 
                [Fcall (fname,List.rev
                (List.map (fun par -> gl_atm par) param)
                ,ret)]
      | Pointer(e) -> let v1 = expr e in 
                 let binresv1 = (get_binres_type v1) in
                 (gen_binres_type (List.tl binresv1)) @
                v1 @ gen_atom (Pntr( (gl_atm v1),
                (get_ptrsize_type binresv1)))
      | Array(base,e) -> let v1 = expr e in
                         let v2 = expr base in
                         let off = add_temp (get_binres_type v1) in
                         let btyp = get_binres_type v2  in
                         let ptrsz = get_ptrsize_type btyp in 
                         let baddr = gl_atm v2 in
                         gen_binres_type(List.tl btyp) @
                         (incr_by_ptrsz v1 ptrsz off) @
                         (add_base_offset btyp baddr off)
      | Addrof(v) -> let v1 = expr v in gen_addr_lst v1
      | Negof(v)  -> let v1 = expr v in 
                gen_binres_type( (get_binres_type v1))
                @ v1 @ gen_atom (Neg(gl_atm v1))
      | Noexpr ->[Atom(Lit(0))]
    in
let rec stmt = function
  Block sl ->
    (List.fold_left (fun str lst -> str @ lst) [] (List.map stmt sl) )
  | Expr e -> expr e
  | Return e -> 
    let v1 = expr e in 
      v1 @ [Rval (gl_atm v1)]
  | If (p, t, f) -> 
    let pval = expr p and tval = stmt t and fval = stmt f in
      let v4 = (gl_atm pval) in
        let l1 = (get_ifelse_label 0) and l2 = (get_ifelse_label 1) in 
          (match fval with
           [] -> pval @ [Predicate (v4,false, l2)] @ tval  @ [Label l2]
          | _ -> pval @ [Predicate (v4,false, l1)] @ tval  @ [Branch (l2)]
                    @ [Label l1] @ fval @ [Label l2])
  | For (asn, cmp, inc, b) -> 
          stmt (Block (
              [Expr (asn); While(cmp, Block([b;Expr(inc)]))]
              ))

  | While (e, b) ->
    let v1 = stmt b and v2 = expr e and l0 = (get_loop_label 0) 
      and l1 = (get_loop_label 1) in
      let v3 = (gl_atm v2) in
        [Branch l1] @ [Label l0] @ v1 @ [Label l1] @ v2 @ [Predicate
        (v3,true,l0)]
  | _ -> []
in 

let stmtblock = (gen_vararr fdecl.locals) @ (stmt (Block fdecl.body)) in

(*[Global([Debug("Debug Message"); Debug("Yellow")])] @*)
[Fstart(fdecl.fname, (conv2_byt_lvar fdecl.formals), stmtblock, !curr_offset)]  

in let env = { function_index = function_indexes;
		           global_index   = global_indexes;
               struct_index   = struct_indexes;
		           local_index    = StringMap.empty 
             }
in

(* Code executed to start the program *)
let entry_function = try
  (StringMap.find "main" function_indexes); []
  with Not_found ->err ("no \"main\" function")
in 
(* Compile the functions *)
List.concat (entry_function :: List.map (translate env) functions);;
(* TODO: Globals might need to be passed before at the point where
 * entry_function is present. Globals can be passed as a list, like that of
 * Fstart *)

