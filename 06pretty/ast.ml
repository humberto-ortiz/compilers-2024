type op =
  | Add
  | Mult

type ast = 
  | Num of int
  | Inc of ast
  | Dec of ast
  | BinOp of ast * op * ast
