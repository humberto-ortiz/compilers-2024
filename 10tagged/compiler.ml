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
  | Mul of arg * arg
  | Cmp of arg * arg
  | Jnz of string
  | Jmp of string
  | Label of string

type env = (string * int) list

let guardar ((v : string), (env : env)) : (int * env) =
  let n = 1 + List.length(env) in
  (n, (v, n) :: env)

let rec lookup (v, env) =
  match env with
  | [] -> failwith "No existe"
  | (x, n)::xs -> if x = v then n else lookup (v, xs)

let gensym =
  let counter = ref 0 in
  (fun basename ->
    counter := !counter + 1;
    sprintf "%s_%d" basename !counter)

let rec anf (e : ast) : aexpr =
  match e with
    | Num n -> AImm (ImmNum n)
    | Inc e -> 
       let tmpvar = gensym "_tmp" in
       ALet (tmpvar, (anf e), AAdd1 (ImmId tmpvar))
    | Dec e -> 
       let tmpvar = gensym "_tmp" in
       ALet (tmpvar, (anf e), ASub1 (ImmId tmpvar))
    | Let (s, e1, e2) ->
       ALet (s, anf e1, anf e2)
    | Id s ->
       AImm (ImmId s)
    | If (c, thn, els) ->
       let tmpvar = gensym "_if" in
       ALet (tmpvar, anf c,
             AIf (ImmId tmpvar, anf thn, anf els))
    | BinOp (e1, op, e2) ->
       let tmpvar1 = gensym "_bin" in
       let tmpvar2 = gensym "_bin" in
       ALet (tmpvar1, anf e1,
             ALet (tmpvar2, anf e2,
                   APrim2 (op, ImmId tmpvar1, ImmId tmpvar2)))

let rec compile_expr (e : aexpr) (env : env) : instruction list =
  let imm_to_arg (e : immexpr) =
    match e with
      | ImmNum n ->
         Const n 
      | ImmId v ->
         let slot = lookup (v, env) in
         RegOffset(RSP, ~-8*slot)
  in
  match e with
  | AImm imm -> 
     [Mov (Reg RAX, imm_to_arg imm) ]
  | AAdd1 inc1 ->
     [ Mov (Reg RAX, imm_to_arg inc1) ; 
       Add (Reg RAX, Const 1) ]
  | ASub1 dec1 ->
     [ Mov (Reg RAX, imm_to_arg dec1) ;
       Add (Reg RAX, Const ~-1) ]
  | ALet (let1, valor, valor2) ->
     let (slot, env') = guardar (let1, env) in
     compile_expr valor env @
       [ Mov (RegOffset (RSP, ~-8 * slot), Reg RAX) ] @
         compile_expr valor2 env'
  | AIf (e1, e2, e3) ->
     let lt = gensym "if_true" in
     let lf = gensym "if_false" in
     let ld = gensym "done" in
     [ Mov (Reg RAX, imm_to_arg e1) ;
       Cmp (Reg RAX, Const 0) ;
       Jnz lt ;
       Label lf ] @
       compile_expr e3 env
       @ [ Jmp ld ;
           Label lt ]
       @ compile_expr e2 env
       @ [ Label ld ]
  | APrim2 (Add, imm1, imm2) ->
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Add (Reg RAX, imm_to_arg imm2) ]
  | APrim2 (Mul, imm1, imm2) ->
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Mul (Reg RAX, imm_to_arg imm2) ]
     


let rec asm_to_string instrs =
  match instrs with
  | [] -> ""
  | (i::instrs) ->  (inst_to_string i) ^ (asm_to_string instrs)

and inst_to_string inst =
  match inst with
    | Mov (a1, a2) -> "mov " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Add (a1, a2) -> "add " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Mul (a1, a2) -> "imul " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Jnz label -> "jnz " ^ label ^ "\n"
    | Jmp label -> "jmp " ^ label ^ "\n"
    | Label label -> label ^ ":\n"
    | Cmp (a1, a2) -> "cmp " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2 ^ "\n"

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
  let anfed = anf program in
  let instrs = compile_expr anfed [] in
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
