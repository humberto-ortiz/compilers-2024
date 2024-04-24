type op =
  | Add
  | Mul
  | Equal

type 'a ast = 
  | Num of int * 'a
  | Bool of bool * 'a
  | Inc of 'a ast * 'a
  | Dec of 'a ast * 'a
  | Let of string * 'a ast * 'a ast * 'a
  | Id of string * 'a
  | If of 'a ast * 'a ast * 'a ast * 'a
  | BinOp of 'a ast * op * 'a ast * 'a
  | Call of string * 'a ast * 'a ast * 'a

type immexpr =
  | ImmNum of int
  | ImmId of string
  | ImmBool of bool

type 'a aexpr =
  | AImm of immexpr * 'a
  | AAdd1 of immexpr * 'a
  | ASub1 of immexpr * 'a
  | APrim2 of op * immexpr * immexpr * 'a
  | AIf of immexpr * 'a aexpr * 'a aexpr * 'a
  | ALet of string * 'a aexpr * 'a aexpr * 'a
  | ACall of string * immexpr * immexpr * 'a

type 'a decl =
  (* function name, argument names, body, tag *)
  | DFun of string * string list * 'a ast * 'a

type 'a program =
  | Program of 'a decl list * 'a ast

type 'a adecl =
  | ADFun of string * string list * 'a aexpr * 'a

type 'a aprogram =
  | AProgram of 'a adecl list * 'a aexpr

