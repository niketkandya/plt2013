open Ast

let rec code_expr = function
  Literal(x) -> "mov r0, #" ^ (string_of_int x) ^ "\n"
  | Call(f, var) ->
       "Args: " ^ String.concat " " (List.map (fun x -> string_of_expr x) var) ^
       "\n" ^
      "/* Push args here if necessary */" ^ "\n" ^
      "bl " ^ f ^ "\n"
  | Binop(e1, op, e2) ->
      (code_expr e1) ^
      "str r0, [sp,#-4]!" ^ "\n" ^
      (code_expr e2) ^
      "ldr r1, [sp], #4" ^ "\n" ^
      (match op with
      Add -> "add r0, r1, r0" ^ "\n"
      | Sub -> "sub r0, r1, r0" ^ "\n"
      | Mult -> "mul r2, r1, r0" ^ "\n" ^
                "mov r0, r2" ^ "\n"
      | Div -> "str r1, [sp,#-4]!" ^ "\n" ^
               "mov r1, r0" ^ "\n" ^
               "ldr r0, [sp], #4" ^ "\n" ^
               "bl __aeabi_idiv" ^ "\n")

let rec code_stmt = function
  Block(stmts) -> String.concat "" (List.map code_stmt stmts)
  | Expr(expr) | Return(expr) -> code_expr expr

let code_function fdecl =
  "\n" ^ (if fdecl.fname = "main" 
          then ".global " ^ fdecl.fname ^ "\n"
          else "" ) ^
  ".func " ^ fdecl.fname ^ "\n" ^
  fdecl.fname ^ ":" ^ "\n" ^
  "/* Save LR */" ^ "\n" ^
  "push {fp, lr}" ^ "\n" ^
  "/* Pop args here if necessary */" ^ "\n" ^
  String.concat "" (List.map code_stmt fdecl.body) ^
  (*String.concat " " fdecl.formals ^*)
  "/* Restore LR */" ^ "\n" ^
  "pop {fp, pc}" ^ "\n" ^
  "bx lr" ^ "\n" ^
  ".endfunc" ^ "\n"

let code_program (globals,functions) =
  let funcs = List.rev functions in
    String.concat "" (List.map code_function funcs)

let _ =
  let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
      let listing = code_program program in
        print_endline listing
