type op =
  | Add
  | Mul

type 'a ast = 
  | Num of int * 'a
  | Inc of 'a ast * 'a
  | Dec of 'a ast * 'a
  | Let of string * 'a ast * 'a ast * 'a
  | Id of string * 'a
  | If of 'a ast * 'a ast * 'a ast * 'a
  | BinOp of 'a ast * op * 'a ast * 'a

type immexpr =
  | ImmNum of int
  | ImmId of string

type 'a aexpr =
  | AImm of immexpr * 'a
  | AAdd1 of immexpr * 'a
  | ASub1 of immexpr * 'a
  | APrim2 of op * immexpr * immexpr * 'a
  | AIf of immexpr * 'a aexpr * 'a aexpr * 'a
  | ALet of string * 'a aexpr * 'a aexpr * 'a

