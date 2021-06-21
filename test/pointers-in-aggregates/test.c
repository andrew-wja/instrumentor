#include <stdlib.h>
#include <stdio.h>

struct foo {
  int first;
  int second;
  int* third;
};

int main(int argc, char * argv[]) {
  struct foo *f = (struct foo*)malloc(sizeof(struct foo));

  f->third = (int*)malloc(sizeof(int));

  printf("%p, %d\n", f->third, *(f->third));

  free(f->third);

  // Failure should occur here
  printf("%p, %d\n", f->third, *(f->third));

  free(f);

  return 0;
}
