
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, 5
mov [RSP-8], RAX
mov RAX, [RSP-8]
add RAX, 1

  ret

