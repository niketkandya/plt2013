type action = Ast | Interpret | Bytecode | Compile

let str = 
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  Compile.translate program

  let a = List.map (fun a -> print_string a; Printf.printf("\n") ) str

 
