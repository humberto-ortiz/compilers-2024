# Adding Booleans to our compiler

Following the recommendations in [Lecture
5](https://course.ccs.neu.edu/cs4410/lec_tagging-values_notes.html), we will add
Booleans to our compiler.

The compiler is currently half broken, but we can parse some expressions, and
tag them, anf them and even compile some of them:

```
$ dune build
$ dune utop
utop# #use "load.ml";;
utop # let input_file = open_in "true.int";;
val input_file : in_channel = <abstr>
─( 15:15:06 )─< command 2 >────────────────────────────────────{ counter: 0 }─
utop # let Ok(program) = Front.parse_file input_file;;
Line 1, characters 4-15:
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Error _
val program : Lexing.position ast =
  Bool (true,
   {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0})
─( 15:15:17 )─< command 3 >────────────────────────────────────{ counter: 0 }─
utop # let tagged = tag program;;
val tagged : tag ast = Bool (true, 1)
─( 15:15:23 )─< command 4 >────────────────────────────────────{ counter: 0 }─
utop # let anfed = anf tagged;;
val anfed : tag aexpr = AImm (ImmBool true, 1)
─( 15:16:06 )─< command 5 >────────────────────────────────────{ counter: 0 }─
utop # compile_expr anfed [];;
- : instruction list = [Mov (Reg RAX, Const (-1))]
```

We still need to fix a *bunch* of stuff.
