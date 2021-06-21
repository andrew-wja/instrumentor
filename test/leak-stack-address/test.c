#include <stdlib.h>
#include <stdio.h>

void bad(const int x, int** y) {
  int z = x + 1;
  *y = &z;
}

int main(int argc, char * argv[]) {
  int *num;

  // leak address of local to num
  bad(2, &num);

  // Failure should occur here where we try to deference num
  printf("%p, %d\n", num, *num);

  return 0;
}
