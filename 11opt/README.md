# Optimization

The ANF transformation we're using to make compiling binary operations simpler
has the unfortunate side effect of introducing a bunch of unnecessary temporary
variables. We saw this in class when looking at the example program:

```
((let (x 1) x) + (let (x 2) x))
```

That produces the following assembly code:
```
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, 1
mov [RSP-8], RAX
mov RAX, [RSP-8]
mov [RSP-8], RAX
mov RAX, 2
mov [RSP-16], RAX
mov RAX, [RSP-16]
mov [RSP-16], RAX
mov RAX, [RSP-8]
add RAX, [RSP-16]

  ret
```

I have added a call to `optimize`, a function to optimize a list of
instructions, removing unnecessary instructions. It currently does nothing.
Modify it to work correctly instead. Be careful, this program should still work:

```
(let (x 1) ((x + 2) + (3 + x)))
```
