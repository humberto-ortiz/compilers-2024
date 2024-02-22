type ast = 
  | Num of int
  | Inc of ast
  | Dec of ast
