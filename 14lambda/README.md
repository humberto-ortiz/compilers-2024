# The lambda lecture (in ocaml)

## Introduction

Alonzo Church proposed the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) in 1936 as a universal model of computation. In the lambda calculus, the only available abstraction is function definition and application. Everything else has to be built up and encoded as functions.

Let's see how much we can build from pure lambdas in ocaml.

In class I used [definitions and encodings](https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec27-lambda/lambda.htm) from Michael R. Clarkson's Data Strucrures and Functional Programming class at Cornell.

## Lambdas

In Church's lambda calculus, functions are written using λ, so the identity function is λx.x. In ocaml, we have to write this as `fun x -> x`.

We can type this into ocaml, but nothing much happens:

```
utop # fun x -> x;;
- : 'a -> 'a = <fun>
```

We've just defined an anonymous lambda function, and utop displays the signature and returns to the prompt.

To do anything useful, the function must be applied to an argument:

```
utop # (fun x -> x) 3;;
- : int = 3
```
The parenthesis around the funcion definition causes ocaml to apply the function to the argument 3.

## Let

We could also bind the function to a name, such as id:

```
utop # let id = fun x -> x;;
val id : 'a -> 'a = <fun>
```
now I can use the name to call the function:
```
utop # id 3;;
- : int = 3
```
You might say I'm already cheating, as let isn't function definition or application, but this is exactly what lambda does, assign a name to a function argument. Let is just syntax over a lambda:
```
utop # (fun id -> id 3) (fun x -> x);;
- : int = 3
```
so I'll use let and pretend I'm still in the pure lambda calculus.

## Truth, what is it anyway?

We can use lambdas to encode true and false:
```
utop # let mytrue = fun x -> fun y -> x;;
val mytrue : 'a -> 'b -> 'a = <fun>
utop # let myfalse = fun x -> fun y -> y;;
val myfalse : 'a -> 'b -> 'b = <fun>
```
and we can use the identity function to select alternatives:
```
utop # let myif = id;;
val myif : 'a -> 'a = <fun>

utop # myif mytrue 1 2;;
- : int = 1
utop # myif myfalse 1 2;;
- : int = 2
```

## Pairs

We can build more complex data structures using functions, here we build pairs:
```
utop # let mypair = fun x -> fun y -> fun c -> (c x y);;
val mypair : 'a -> 'b -> ('a -> 'b -> 'c) -> 'c = <fun>
```
since mytrue takes a pair of arguments and returns the first, we can use it to access the first element of a pair:
```
utop # let myfst = fun d -> (d mytrue);;
val myfst : (('a -> 'b -> 'a) -> 'c) -> 'c = <fun>

utop # let mysnd = fun d -> (d myfalse);;
val mysnd : (('a -> 'b -> 'b) -> 'c) -> 'c = <fun>

utop # myfst (mypair 1 2);;
- : int = 1

utop # mysnd (mypair 1 2);;
- : int = 2
```

## Lists

We already have lists, just use `mypair`, `myfst` and `mysnd`, and the appropriate nil:
```
utop # let mynil = fun x -> mytrue;;
val mynil : 'a -> 'b -> 'c -> 'b = <fun>
```
Here we're really starting to torture ocaml's type system:
```
utop # let undostres = (mypair 1 (mypair 2 (mypair 3 mynil)));;
val undostres :
  (int ->
   ((int ->
     ((int -> ('_weak2 -> '_weak3 -> '_weak4 -> '_weak3) -> '_weak5) -> '_weak5) ->
     '_weak6) ->
    '_weak6) ->
   '_weak7) ->
  '_weak7 = <fun>
```
But we can get data back out of a list as usual:
```
utop # (myfst (mysnd undostres));;
- : int = 2
```

## Church numerals

We *have* been cheating. Several of our examples use ocaml integers. We can actually get rid of them, and replace them by Church numerals.

```
utop # let cero = fun f -> fun x -> x;;
val cero : 'a -> 'b -> 'b = <fun>

utop # let uno = fun f -> fun x -> (f x);;
val uno : ('a -> 'b) -> 'a -> 'b = <fun>

utop # let dos = fun f -> fun x -> (f (f x));;
val dos : ('a -> 'a) -> 'a -> 'a = <fun>
```
so, in general, the n'th Church numeral is a function that takes a function f and applies it n times to an argument x.

We can even define a successor function, that applies one more f to it's argument:
```
utop # let add1 = fun n -> fun f -> fun x -> (f (n f x));;
val add1 : (('a -> 'b) -> 'c -> 'a) -> ('a -> 'b) -> 'c -> 'b = <fun>

utop # let tres = (add1 dos);;
val tres : ('_weak10 -> '_weak10) -> '_weak10 -> '_weak10 = <fun>
```
However, it's hard to see whats happening. Lets set up a helper function to visualize Church numerals:
```
utop # let palito = fun () -> print_char '/';;
val palito : unit -> unit = <fun>
```
so, palito is a function that takes unit (), and prints a tally:
```
utop # palito ();;
/- : unit = ()
```
And tres should be a Church numeral that takes a function f and an argument x and applies f three times to x:
```
utop # tres palito ();;
///- : unit = ()
```

## Addition

Since Church numerals already apply their arguments multiple times, we can use them to apply functions repeatedly:

```
utop # let myadd = fun m -> fun n -> (m add1 n);;
val myadd :
  (((('a -> 'b) -> 'c -> 'a) -> ('a -> 'b) -> 'c -> 'b) -> 'd -> 'e) ->
  'd -> 'e = <fun>

utop # (myadd (add1 dos) dos) palito ();;
/////- : unit = ()
```

## Exponentiation

In class, we accidentally defined exponentiation:
```
utop # ((add1 dos) dos) palito ();;
////////- : unit = ()
```
The output is 8 (2^3).
```
utop # let myexp = fun m -> fun n -> (m n);;
val myexp : ('a -> 'b) -> 'a -> 'b = <fun>

utop # (myexp dos (myexp dos dos)) palito ();;
////////////////- : unit = ()
```
