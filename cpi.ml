open Ast

let rec code_expr = function
  Literal(x) -> print_endline ("mov r0, #" ^ (string_of_int x))
  | Binop(e1, op, e2) ->
      code_expr e1;
      print_endline "str r0, [sp,#-4]!";
      code_expr e2;
      print_endline "ldr r1, [sp], #4";
      match op with
      Add -> print_endline "add r0, r1, r0"
      | Sub -> print_endline "sub r0, r1, r0"
      | Mult -> print_endline "mul r2, r1, r0";
                print_endline "mov r0, r2"
      | Div -> print_endline "sdiv r2, r1, r0";
               print_endline "mov r0, r2"

let rec code_stmt = function
  Expr(expr) -> code_expr expr

let code_function fdecl =
  List.map code_stmt fdecl.body

let code_program (globals,functions) =
  List.map code_function functions

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
    (print_endline ".global main\n.func main\nmain:";
     code_program program;
     print_endline "bx lr")
