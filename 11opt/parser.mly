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
%token PLUS
%token TIMES

%start <'a Ast.ast> expr
%%

expr:
  | i = INT { Num (i, $startpos) }
  | LPAREN INC e = expr RPAREN { Inc (e, $startpos) }
  | LPAREN DEC e = expr RPAREN { Dec (e, $startpos)}
  | LPAREN LET LPAREN id = ID  e = expr RPAREN e2 = expr RPAREN 
    {Let (id, e, e2, $startpos)} 
  | id = ID { Ast.Id (id, $startpos) }
  | LPAREN IF e1 = expr e2 = expr e3 = expr RPAREN { If (e1, e2, e3, $startpos) }
  | LPAREN e1 = expr PLUS e2 = expr RPAREN {BinOp (e1, Add, e2, $startpos)}
  | LPAREN e1 = expr TIMES e2 = expr RPAREN {BinOp (e1, Mul, e2, $startpos)}
