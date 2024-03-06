open Printf
open Ast

type reg = RAX | RSP

type arg =
  | Reg of reg
  | Const of int
  | RegOffset of reg * int

type instruction =
  | Mov of arg * arg
  | Add of arg * arg

type env = (string * int) list

let guardar ((v : string), (env : env)) : (int * env) =
  let n = 1 + List.length(env) in
  (n, (v, n) :: env)

let rec lookup (v, env) =
  match env with
  | [] -> failwith "No existe"
  | (x, n)::xs -> if x = v then n else lookup (v, xs)


let rec compile_expr (e : ast) (env : env) : instruction list =
  match e with
  | Num n -> 
     [ Mov (Reg RAX, Const n) ]
  | Inc inc1 ->
     compile_expr inc1 env
     @ [ Add (Reg RAX, Const 1) ]
  | Dec dec1 ->
     compile_expr dec1 env
     @ [ Add (Reg RAX, Const ~-1) ]
  | Let (let1, valor, valor2) ->
     let (slot, env') = guardar (let1, env) in
     compile_expr valor env @
       [ Mov (RegOffset (RSP, ~-8 * slot), Reg RAX) ] @
         compile_expr valor2 env'
  | Id v -> 
     let slot = lookup (v, env) in
     [ Mov (Reg RAX, RegOffset(RSP, ~-8*slot))]

let rec asm_to_string instrs =
  match instrs with
  | [] -> ""
  | (i::instrs) ->  (inst_to_string i) ^ (asm_to_string instrs)

and inst_to_string inst =
  match inst with
    | Mov (a1, a2) -> "mov " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Add (a1, a2) -> "add " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"

and arg_to_string a =
  match a with
  | Reg r -> reg_to_string r 
  | Const c -> string_of_int c
  | RegOffset (r, slot) -> "[" ^ reg_to_string r ^ string_of_int slot ^ "]"

and reg_to_string r =
  match r with
  | RAX -> "RAX"
  | RSP -> "RSP"

(* A very sophisticated compiler - insert the given integer into the mov
instruction at the correct place *)
let compile_program (program : ast) : string =
  let instrs = compile_expr program [] in
  let asm_string = asm_to_string instrs in

  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
  %s
  ret\n" asm_string;;

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.expr Lexer.read lexbuf in
  close_in input_file;
  let program = (compile_program input_program) in
  printf "%s\n" program;;
