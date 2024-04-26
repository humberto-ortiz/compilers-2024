(* lexer.mll - ocamllex source for lexer for epcp
   Copyright (2023) Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE for details
*)
{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}
let int = '-'?['0'-'9']+
let bin = "0b" ['0'-'1']+
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read =
     parse
     | [' ' '\t']+      { read lexbuf }
     | '\r' | '\n' | "\r\n" { next_line lexbuf; read lexbuf }
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
