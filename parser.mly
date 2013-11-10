%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LSUBS RSUBS
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT CHAR STRUCT VOID
%token AMPERSAND 
%token <string> CONSTCHAR
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program sdecl { ($2 :: fst $1), snd $1 }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }


fdecl:
   retval formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = snd $1;
	 formals = $2; 
         locals = List.rev $5;
         body = List.rev $6;
         ret = fst $1
         } }

retval:
        INT ID LPAREN { Int, $2  }
        |CHAR ID LPAREN { Char, $2  }
        |VOID ID LPAREN { Void, $2  }

sdecl:
        STRUCT ID LBRACE vdecl_list RBRACE SEMI { Struct($2,$4) }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    tdecl                   { [$1] }
  | formal_list COMMA tdecl { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   | tdecl SEMI { $1 }

tdecl:
     INT ID { Var($2,Int,1) }
     | CHAR ID { Var($2,Char,1) }
     | INT ptr { Var($2, Intptr,1) }
     | CHAR ptr {Var($2, Charptr,1) }
     | CHAR arr { Var((fst $2),Chararr,(snd $2)) }
     | INT arr { Var((fst $2),Intarr,(snd $2)) }

arr:
        ID LSUBS LITERAL RSUBS { $1,$3 }

ptr:
        TIMES ID        {$2}

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | RETURN consts SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | lvalue           { $1 }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | lvalue ASSIGN expr   { Assign($1, $3) }
  | lvalue ASSIGN consts   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

lvalue:
        var     { $1 }
        | ptr   { Ptr($1) }


var:
        ID      { Id($1) }
        | arr   { Arr( fst $1, snd $1) } 

consts:
   AMPERSAND var      { Addrof($2)  }
  | CONSTCHAR        { ConstCh($1) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
   expr                    { [$1] }
  |consts                 { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
  | actuals_list COMMA consts { $3 :: $1 }
