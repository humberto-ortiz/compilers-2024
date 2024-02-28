# Pretty Printer

Here's a pretty printer for abstract syntax trees. It can print out the internal AST for expressions in our language.

You can build the `pretty.exe` with dune:

```
$ dune build
```

And run it on a test program:

```
$ dune exec ./pretty.exe  inc5.int
Inc (Num (5)
)
```
I have already added BinOp to the `ast.ml` file, but our lexer and parser can't recognize binary expressions.

## Assignment

Add support for binary `+` and `*` in the `lexer.mll` and `parser.mly`. You'll
need to declare tokens and establish regular expressions in the lexer and
productions in the grammar to deal with the operations. Assume all binary
expressions are fully parenthesized. Here are several example programs that
should parse and produce output:

```
(5 * 6)
```

```
((inc 0) + 2)
```
