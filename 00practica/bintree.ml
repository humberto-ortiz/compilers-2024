type bintree = 
  | Node of int * bintree * bintree
  | Leaf

(* ejemplo *)
let arbol = Node (5 ,Node (30, Leaf, Leaf), Node (25, Leaf, Leaf)) ;;

let rec sumtree (bt : bintree) : int =
  match bt with
  | Leaf -> 0
  | Node (v, l, r) -> v + sumtree l + sumtree r
