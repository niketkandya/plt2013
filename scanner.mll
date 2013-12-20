{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { line_comment lexbuf }
| "#include"  { includes  lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSUBS }
| ']'      { RSUBS }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '&'      { AMPERSAND }
| '/'      { DIVIDE }
| '.'      { DOT }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "->"     { INDIRECTION }
| "&&"     { LAND }
| "||"     { LOR }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "char"   { CHAR }
| "struct" { STRUCT }
| "NULL"   { NULL }
| "void"   { VOID }
| ''' [ ^'''] as ch ''' { CONSTCHAR(ch) }
| '"' [^'"']* '"'  as lxm { STRING(lxm) }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and line_comment = parse
  "\n"  { token lexbuf }
| _     { line_comment lexbuf }

and includes = parse
  "\n"  { token lexbuf }
| _     { includes lexbuf }
