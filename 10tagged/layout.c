#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = 10;
  int b = 20;
  int *c;

  c = (int *) malloc(sizeof(int));

  printf("main   = %p\n", main);
  printf("&a     = %p\n", &a);
  printf("c      = %p\n", c);
  printf("printf = %p\n", printf);

  return 0;
}
