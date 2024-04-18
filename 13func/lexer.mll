(* lexer.mll - ocamllex source for lexer for epcp
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*)
{
open Parser

exception SyntaxError of string
}

let int = '-'?['0'-'9']+
let bin = "0b" ['0'-'1']+
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read =
     parse
     | [' ' '\t']+      { read lexbuf }
     | '\r' | '\n' | "\r\n" {  read lexbuf }
     | int | bin { INT (int_of_string (Lexing.lexeme lexbuf)) }
     | '(' { LPAREN }
     | ')' { RPAREN }
     | "inc" { INC }
     | "dec" { DEC }
     | "let" { LET }
     | "if"  { IF }
     | '+'   { PLUS }
     | '*'   { TIMES }
     | "=="  { EQUAL }
     | "true" { TRUE }
     | "false" { FALSE }
     | "def" { DEF }
     | id   { ID (Lexing.lexeme lexbuf) }
     | _ { raise (SyntaxError ("Illegal character - " ^ Lexing.lexeme lexbuf)) }
