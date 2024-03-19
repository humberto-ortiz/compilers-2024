/* parser.mly - menhir source for parser for epcp programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int> INT
%token <string> ID
%token LPAREN
%token RPAREN
%token INC
%token DEC
%token LET
%token IF

%start <Ast.ast> expr
%%

expr:
  | i = INT { Num i }
  | LPAREN INC e = expr RPAREN { Inc e }
  | LPAREN DEC e = expr RPAREN { Dec e }
  | LPAREN LET LPAREN id = ID  e = expr RPAREN e2 = expr RPAREN 
    {Let (id, e, e2)} 
  | id = ID { Ast.Id id }
  | LPAREN IF e1 = expr e2 = expr e3 = expr RPAREN { If (e1, e2, e3) }
