#include <stdlib.h>
#include <stdio.h>

void bad(const int x, int** y) {
  int z = x + 1;
  *y = &z;
}

void bad2(const int x, int** y) {
  int z; // this gets the same stack address as int z above
  printf("%p, %d\n", &z, z);
}

int main(int argc, char * argv[]) {
  int *num1, *num2;

  // leak address of local to num1
  bad(2, &num1);
  // leak address of local to num2
  bad(3, &num2);

  printf("%p, %d\n", num1, *num1);
  printf("%p, %d\n", num2, *num2);

  *num2 = 6;

  bad2(10, &num1);

  return 0;
}
