#include <stdlib.h>
#include <stdio.h>

struct foo {
  int first;
  int second;
  int* third;
};

int main(int argc, char * argv[]) {
  struct foo *f = (struct foo*)malloc(sizeof(struct foo));

  int* ptr_to_first = &(f->first);

  printf("%p, %d\n", ptr_to_first, *(ptr_to_first));

  ptr_to_first += 1;

  // Failure should occur here
  printf("%p, %d\n", ptr_to_first, *(ptr_to_first));

  free(f);

  return 0;
}
