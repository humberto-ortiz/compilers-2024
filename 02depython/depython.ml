type op = Add | Mult

type depython = 
 | Const of int
 | BinOp of depython * op * depython 
 | Assign of string * depython
 | Name of string

let t1 = BinOp (Const 5, Add, BinOp (Const 25, Mult, Const 2))

let t2 = [ Assign ("n", Const 2) ; BinOp (Const 5, Add, BinOp (Const 25, Mult, Name "n")) ]

type env = (string * int) list

let guardar ((v : string), (n : int), (env : env)) : (int * env) =
  (n, (v, n) :: env)

let rec lookup (v, env) =
  match env with
  | [] -> failwith "No existe"
  | (x, n)::xs -> if x = v then n else lookup (v, xs)


let rec calc ((e : depython), (env : env)) : (int * env) =
  match e with
  | Const n -> (n, env) 
  | BinOp (l, Add, r) -> 
     let lv, env' = calc (l, env) in
     let rv, env'' = calc (r, env') in
     lv + rv, env''
  | BinOp (l, Mult, r) -> 
     let lv, env' = calc (l, env) in
     let rv, env'' = calc (r, env') in
     lv * rv, env''
  | Assign (x, e) -> 
     let lv, env' = calc (e, env) in
     guardar (x, lv, env')
  | Name x -> (lookup (x, env), env)

let t3 = Assign ("x", Assign ("y", Const 3));;

let interp (es : depython list) : int =
  let rec help (es, env, n) =
    match es with
    | [] -> n
    | (e :: es) -> 
       let n, env' = calc (e, env) in
       help (es, env', n) 
  in
  help (es, [], 0)
