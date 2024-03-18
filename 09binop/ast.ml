type op =
  | Add
  | Mul

type ast = 
  | Num of int
  | Inc of ast
  | Dec of ast
  | Let of string * ast * ast
  | Id of string 
  | If of ast * ast * ast
  | BinOp of ast * op * ast

type immexpr =
  | ImmNum of int
  | ImmId of string

type aexpr =
  | AImm of immexpr
  | AAdd1 of immexpr
  | ASub1 of immexpr
  | APrim2 of op * immexpr * immexpr
  | AIf of immexpr * aexpr * aexpr
  | ALet of string * aexpr * aexpr

