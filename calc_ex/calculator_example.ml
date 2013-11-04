open Ast

let args = Array.make 10 0

let rec code = function
     Lit(x) -> print_endline ("mov r0, #" ^ (string_of_int x))
   | Binop(e1, op, e2) -> 
        code e1;
        print_endline "str r0, [sp,#-4]!";
        code e2;
        print_endline "ldr r1, [sp], #4";
        match op with
        Add -> print_endline "add r0, r1, r0"
        | Sub -> print_endline "sub r0, r1, r0"
        | Mul -> print_endline "mul r2, r1, r0";
                 print_endline "mov r0, r2"
        | Div -> print_endline "sdiv r2, r1, r0";
                 print_endline "mov r0, r2"

let rec eval = function
    Lit(x) -> x
  | Var(x) -> Array.get args x
  | Asn(variable, e1) -> 
      let v1 = eval e1 in
      ( Array.set args variable v1;
      v1 )
  | Seq(e1, e2) -> 
      let _ = eval e1 and v2 = eval e2 in v2
  | Binop(e1, op, e2) ->
      let v1 = eval e1 and v2 = eval e2 in
      match op with
	      Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
    (print_endline ".global main\n.func main\nmain:";
     code expr;
     print_endline "bx lr")
