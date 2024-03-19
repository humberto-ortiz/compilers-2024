# Tagged Compiler for BinOp

Wierd errors?
```
File "front.ml", line 4, characters 5-9:
4 | open Core
         ^^^^
Error: Unbound module Core
```
do `opam install core`.

This compiler can parse binary operations:

```
$ cat unoydos.int
((inc 0) + 2)
$ make unoydos.run
dune exec ./compiler.exe unoydos.int > unoydos.s
nasm -f elf64 -o unoydos.o unoydos.s
clang -g -m64 -o unoydos.run main.c unoydos.o
rm unoydos.s unoydos.o
$ ./unoydos.run
3
```

And even better, gives compiler errors for invalid syntax.

```
$ cat > badplus.int
(1 ++ 2)
$ make badplus.run
dune exec ./compiler.exe badplus.int > badplus.s
Line:1 Position:6: syntax error
make: *** [Makefile:8: badplus.s] Error 1
rm badplus.s
```

The cost? A 142-line change to 6 source files, including a whole new file for the compiler front-end. Plus installing 60 or so packages in opam (827MB in `~/.opam/default/lib`).
