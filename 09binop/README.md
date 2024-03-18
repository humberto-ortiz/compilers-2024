# Compiler for BinOp

I have here half of a compiler for binary + and *. I never implemented the parser, so it doesn't work, but we can run the compiler by hand, like we did in class.

```
$ dune utop
# #use "load.ml";;
# print_string (compile_program (BinOp (Num 1, Add, Num 2)));;

section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, 1
mov [RSP-8], RAX
mov RAX, 2
mov [RSP-16], RAX
mov RAX, [RSP-8]
add RAX, [RSP-16]

  ret
- : unit = ()
```
