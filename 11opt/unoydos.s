
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, 0
mov [RSP-8], RAX
mov RAX, [RSP-8]
add RAX, 1
mov [RSP-8], RAX
mov RAX, 2
mov [RSP-16], RAX
mov RAX, [RSP-8]
add RAX, [RSP-16]

  ret

