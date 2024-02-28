open Printf
open Ast

let pretty_op op =
  match op with
  | Add -> "+"
  | Mult -> "*"

let rec pretty_expr (e : ast) : string =
  match e with
  | Num n -> 
     sprintf "Num (%d)\n" n
  | Inc inc1 ->
     sprintf "Inc (%s)" (pretty_expr inc1)
  | Dec dec1 ->
     sprintf "Dec (%s)\n" (pretty_expr dec1)
  | BinOp (l, op, r) ->
     sprintf "BinOp (%s, %s, %s)\n" (pretty_expr l) (pretty_op op) (pretty_expr r)

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.expr Lexer.read lexbuf in
  close_in input_file;
  let program = (pretty_expr input_program) in
  printf "%s\n" program;;
