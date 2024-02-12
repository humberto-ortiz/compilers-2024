/* parser.mly - menhir source for parser for epcp programs 
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*/
%token <int> INT

%start <int> prog
%%

prog:
  | i = INT { i }
