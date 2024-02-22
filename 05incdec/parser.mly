/* parser.mly - menhir source for parser for epcp programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int> INT
%token LPAREN
%token RPAREN
%token INC
%token DEC

%start <Ast.ast> expr
%%

expr:
  | i = INT { Num i }
  | LPAREN INC e = expr RPAREN { Inc e }
  | LPAREN DEC e = expr RPAREN { Dec e }
