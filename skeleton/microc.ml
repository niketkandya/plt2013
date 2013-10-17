type action = Ast | Interpret | Bytecode | Compile

let program = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  Compile.translate program;;

let some = List.map print_endline
(List.map Execute.execute_prog (List.concat program))


 
