open Printf
open Ast

type reg = RAX | RSP | RDI | RSI | RBP | RDX | RCX

type arg =
  | Reg of reg
  | Const of int
  | RegOffset of reg * int

type instruction =
  | Mov of arg * arg
  | Add of arg * arg
  | Mul of arg * arg
  | Cmp of arg * arg
  | Test of arg * arg
  | Sar of arg * arg
  | Jnz of string
  | Jmp of string
  | Label of string
  | Call of string
  | Push of arg
  | Pop of arg
  | Ret

type env = (string * int) list

let guardar ((v : string), (env : env)) : (int * env) =
  let n = 1 + List.length(env) in
  (n, (v, n) :: env)

let guardar_todos ((vs : string list), (env : env)) : (int list * env) =
  let rec help vs env slot_acc =
     match vs with
     | [] -> (slot_acc, env)
     | (v::vs) -> 
        let (slot, env') = guardar (v, env) in
        help vs env' (slot::slot_acc)
  in
  help vs env []

let rec lookup (v, env) =
  match env with
  | [] -> failwith "No existe"
  | (x, n)::xs -> if x = v then n else lookup (v, xs)

type tag = int

let rec anf (e : tag ast) : tag aexpr =
  match e with
    | Num (n, tag) -> AImm (ImmNum n, tag)
    | Bool (b, tag) -> AImm (ImmBool b, tag)
    | Inc (e, tag) -> 
       let tmpvar = sprintf "_tmp_%d" tag in
       ALet (tmpvar, (anf e), AAdd1 (ImmId tmpvar, tag), tag)
    | Dec (e, tag) -> 
       let tmpvar = sprintf "_tmp_%d" tag in
       ALet (tmpvar, (anf e), ASub1 (ImmId tmpvar, tag), tag)
    | Let (s, e1, e2, tag) ->
       ALet (s, anf e1, anf e2, tag)
    | Id (s, tag) ->
       AImm (ImmId s, tag)
    | If (c, thn, els, tag) ->
       let tmpvar = sprintf "_if_%d" tag in
       ALet (tmpvar, anf c,
             AIf (ImmId tmpvar, anf thn, anf els, tag), tag)
    | BinOp (e1, op, e2, tag) ->
       let tmpvar1 = sprintf "_bin_l_%d" tag in
       let tmpvar2 = sprintf "_bin_r_%d" tag in
       ALet (tmpvar1, anf e1,
             ALet (tmpvar2, anf e2,
                   APrim2 (op, ImmId tmpvar1, ImmId tmpvar2, tag), tag), tag)
    | Call (f, e1, e2, tag) -> 
       let tmpvar1 = sprintf "_call_l_%d" tag in
       let tmpvar2 = sprintf "_call_r_%d" tag in
       ALet (tmpvar1, anf e1,
             ALet (tmpvar2, anf e2,
                   ACall (f, ImmId tmpvar1, ImmId tmpvar2, tag), tag), tag)

let const_true = -1
let const_false = 1

let rec compile_expr (e : tag aexpr) (env : env) : instruction list =
  let imm_to_arg (e : immexpr) =
    match e with
      | ImmNum n ->
         Const (n * 2) 
      | ImmId v ->
         let slot = lookup (v, env) in
         RegOffset(RSP, ~-8*slot)
      | ImmBool true -> Const const_true
      | ImmBool false -> Const const_false
  in
  match e with
  | AImm (imm, _) -> 
     [Mov (Reg RAX, imm_to_arg imm) ]
  | AAdd1 (inc1, _) ->
     [ Mov (Reg RAX, imm_to_arg inc1) ; 
       Test (Reg RAX, Const 0x0001) ;
       Jnz "error_not_number";
       Add (Reg RAX, Const 2) ]
  | ASub1 (dec1, _) ->
     [ Mov (Reg RAX, imm_to_arg dec1) ;
       Test (Reg RAX, Const 0x0001) ;
       Jnz "error_not_number";
       Add (Reg RAX, Const ~-2) ] (* la representacion de -1 es -2 *)
  | ALet (let1, valor, valor2, _) ->
     let (slot, env') = guardar (let1, env) in
     compile_expr valor env @
       [ Mov (RegOffset (RSP, ~-8 * slot), Reg RAX) ] @
         compile_expr valor2 env'
  | AIf (e1, e2, e3, tag) ->
     let lt = sprintf "if_true_%d" tag in
     let lf = sprintf "if_false_%d" tag in
     let ld = sprintf "done_%d" tag in
     [ Mov (Reg RAX, imm_to_arg e1) ;
       Cmp (Reg RAX, Const const_false) ;
       Jnz lt ;
       Label lf ] @
       compile_expr e3 env
       @ [ Jmp ld ;
           Label lt ]
       @ compile_expr e2 env
       @ [ Label ld ]
  | APrim2 (Add, imm1, imm2, _) ->
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Test (Reg RAX, Const 0x0001) ;
       Jnz "error_not_number";
       Mov (Reg RAX, imm_to_arg imm2) ;
       Test (Reg RAX, Const 0x0001) ;
       Jnz "error_not_number";
       Mov (Reg RAX, imm_to_arg imm1) ; (* volver a copiar imm1 *)
       Add (Reg RAX, imm_to_arg imm2) ]
  | APrim2 (Mul, imm1, imm2, _) ->
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Test (Reg RAX, Const 0x0001) ;
       Jnz "error_not_number";
       Mov (Reg RAX, imm_to_arg imm2) ;
       Test (Reg RAX, Const 0x0001) ;
       Jnz "error_not_number";
       Mov (Reg RAX, imm_to_arg imm1) ; (* volver a copiar imm1 *)
       Mul (Reg RAX, imm_to_arg imm2) ;
       Sar (Reg RAX, Const 1) ]
  | APrim2 (Equal, imm1, imm2, tag) ->
     let ld = sprintf "done_%d" tag in
     let lf = sprintf "false_%d" tag in
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Cmp (Reg RAX, imm_to_arg imm2) ;
       Jnz lf ;
       Mov (Reg RAX, Const const_true) ;
       Jmp ld ;
       Label lf ;
       Mov (Reg RAX, Const const_false);
       Label ld
     ]
  | ACall (f, i1, i2, _) ->
     [ Mov (Reg RDI, imm_to_arg i1) ;
       Mov (Reg RSI, imm_to_arg i2) ;
       Call f
     ]
  (* | _ -> failwith "no se compilar eso" *)


let rec asm_to_string instrs =
  match instrs with
  | [] -> ""
  | (i::instrs) ->  (inst_to_string i) ^ (asm_to_string instrs)

and inst_to_string inst =
  match inst with
    | Mov (a1, a2) -> "mov " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Add (a1, a2) -> "add " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Mul (a1, a2) -> "imul " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Sar (a1, a2) -> "sar " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Jnz label -> "jnz " ^ label ^ "\n"
    | Jmp label -> "jmp " ^ label ^ "\n"
    | Label label -> label ^ ":\n"
    | Cmp (a1, a2) -> "cmp " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2 ^ "\n"
    | Test (a1, a2) -> "test " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2 ^ "\n"
    | Call f -> "call " ^ f ^ "\n"
    | Push a -> "push " ^ arg_to_string a ^ "\n"
    | Ret -> "ret\n"
    | Pop a -> "pop " ^ arg_to_string a ^ "\n"

and arg_to_string a =
  match a with
  | Reg r -> reg_to_string r 
  | Const c -> string_of_int c
  | RegOffset (r, slot) -> "[" ^ reg_to_string r ^ string_of_int slot ^ "]"

and reg_to_string r =
  match r with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | RDI -> "RDI"
  | RSI -> "RSI"
  | RBP -> "RBP"
  | RCX -> "RCX"
  | RDX -> "RDX"

let anf_decl d =
  match d with
  | DFun (string, args, e, tag) ->
     ADFun (string, args, anf e, tag)

let rec anf_decls ds =
  match ds with
  | [] -> []
  | d::ds -> (anf_decl d)::(anf_decls ds)
     
let anf_program (p : tag program) =
  match p with
  | Program (ds, e) ->
     AProgram (anf_decls ds, anf e)

let take n xs =
  let rec help n xs res =
   match xs with
    | [] -> res
    | x::xs -> 
      if n = 0 then res
      else help (n-1) xs (x::res)
  in List.rev (help n xs [])

let put_slots slots =
  let places = [RDI; RSI; RDX; RCX] in (* lo arreglamos despues *)
  let poner slot place =
    Mov (RegOffset (RSP, ~-8 * slot), Reg place) in
  if List.length slots > 4 then failwith "demasiados argumentos" else
    let sources = take (List.length slots) places in
   (* hacer algo con places y sources y map2? *)
    List.map2 poner slots sources
 
let compile_decl d env =
  match d with
    | ADFun (f, args, e, _) ->
       let (slots, env') = guardar_todos (args, env) in
       
       [ Label f;
         Push (Reg RBP);
         Mov (Reg RBP, Reg RSP) ] @
         put_slots slots @
       compile_expr e env' @
       [ Mov (Reg RSP, Reg RBP);
         Pop (Reg RBP);
         Ret ]

let rec compile_decls ds env =
  match ds with
  | [] -> []
  | d::ds -> compile_decl d env @ compile_decls ds env

let compile_aprogram p env =
  match p with
  | AProgram (ds, e) ->
     compile_decls ds env , compile_expr e env

(* A very sophisticated compiler - insert the given integer into the mov
instruction at the correct place *)
let compile_program (program : tag program) : string =
  let anfed = anf_program program in
  let defs, instrs = compile_aprogram anfed [] in
  let main_string = asm_to_string instrs in
  let defs_string = asm_to_string defs in

  sprintf "
section .text

extern error
extern max

global our_code_starts_here
our_code_starts_here:
  push RBP          ; save (previous, caller's) RBP on stack
  mov RBP, RSP 
  %s                ; el main
  mov RSP, RBP      ; restore value of RSP to that just before call
                    ; now, value at [RSP] is caller's (saved) RBP
  pop RBP           ; so: restore caller's RBP from stack [RSP]
  ret

error_not_number:
  mov rdi, 1
  mov rsi, rax
  call error

; las defs
%s
\n" main_string defs_string;;

let rec tag_decl (d, cur) =
  match d with
    | DFun (f, args, e, _) ->
       (DFun (f, args, tag_ast e, cur), cur + 1)

and tag_decls (ds : 'a decl list) cur : tag decl list = (* pichea *)
  match ds with
    | [] -> []
    | d::ds ->
       let (d_tag, next) = tag_decl (d, cur) in
       d_tag::(tag_decls ds next)

and tag_program (p : 'a program) : tag program =
  match p with
    | Program (decls, e) ->
       Program (tag_decls decls 0, tag_ast e)

and tag_ast (e : 'a ast) : tag ast =
  let rec help (e : 'a ast) (cur : tag) : (tag ast * tag) =
    match e with
    | Num (n, _) -> (Num (n, cur), cur + 1)
    | Bool (b, _) -> (Bool (b, cur), cur + 1)
    | Inc (e, _) ->
      let (tag_e, next_tag) = help e (cur + 1) in
      (Inc (tag_e, cur), next_tag)
    | Dec (e, _) ->
      let (tag_e, next_tag) = help e (cur + 1) in
      (Dec (tag_e, cur), next_tag)
    | Let (v, e1, e2, _) ->
      let (tag_e1, next_tag) = help e1 (cur + 1) in
      let (tag_e2, next_tag) = help e2 (next_tag + 1) in
      (Let (v, tag_e1, tag_e2, cur), next_tag)
    | Id (v, _) -> (Id (v, cur), cur + 1)
    | If (c, t, e, _) ->
      let (tag_c, next_tag) = help c (cur + 1) in
      let (tag_t, next_tag) = help t (next_tag + 1) in
      let (tag_e, next_tag) = help e (next_tag + 1) in
      (If (tag_c, tag_t, tag_e, cur), next_tag)
    | BinOp (e1, op, e2, _) ->
      let (tag_e1, next_tag) = help e1 (cur + 1) in
      let (tag_e2, next_tag) = help e2 (next_tag + 1) in
      (BinOp (tag_e1, op, tag_e2, cur), next_tag)
    | Call (f, e1, e2, _) -> 
      let (tag_e1, next_tag) = help e1 (cur + 1) in
      let (tag_e2, next_tag) = help e2 (next_tag + 1) in
      (Call (f, tag_e1, tag_e2, cur), next_tag)
  in
  let (tagged, _) = help e 1 in tagged;;

(* Some OCaml boilerplate for reading files and command-line arguments *)
(* Use code from https://mukulrathi.com/create-your-own-programming-language/parsing-ocamllex-menhir/ to catch syntax errors *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let maybe_program = Front.parse_file input_file in
  close_in input_file;
  match maybe_program with
  | Ok input_program ->
     let tagged = tag_program input_program in
     let program = (compile_program tagged) in
     printf "%s\n" program
  | Error e -> eprintf "%s" (Core.Error.to_string_hum e) ; exit 1
