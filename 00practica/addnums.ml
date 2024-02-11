type number = Int of int | Float of float

let addnum x y =
  match x with
  | Int n ->
     begin
       match y with
       | Int m -> Int (n + m)
       | Float f1 -> Float (f1 +. float_of_int n)
     end
  | Float f1 ->
     begin
       match y with
       | Int m -> Float (f1 +. float_of_int m)
       | Float f2 -> Float (f1 +. f2)
     end;;

addnum (Int 4) (Float 3.5)
