#include <stdlib.h>
#include <stdio.h>

__attribute__((__noinline__)) void bad(const int x, int** y) {
  int z = x + 1;
  *y = &z;
}

__attribute__((__optnone__)) int main(int argc, char * argv[]) {
  int *num;

  // leak address of local to num
  bad(2, &num);

  // Failure should occur here where we try to deference num
  printf("%p, %d\n", num, *num);

  return 0;
}
