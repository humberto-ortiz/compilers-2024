type ast = 
  | Num of int
  | Inc of ast
  | Dec of ast
  | Let of string * ast * ast
  | Id of string 
  | If of ast * ast * ast
