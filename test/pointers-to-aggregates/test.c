#include <stdlib.h>
#include <stdio.h>

typedef struct {
  int first;
  int second;
} baz;

typedef struct {
  int first;
  short second;
  baz* third;
} bar;

typedef struct {
  int first;
  int second;
  bar* third;
} foo;

int main(int argc, char * argv[]) {
  // This looks a bit messy, but it's just to prevent the compiler from totally eliding the structure types
  foo *f = (foo*)malloc(sizeof(foo));
  baz *z = (baz*)malloc(sizeof(baz));
  z->first = 2;
  z->second = 1;
  bar b = {1, 1, z};
  f->third = &b;

  printf("%p\n", f->third);

  printf("%p, %d\n", f->third->third, f->third->third->first);

  int* field_ptr = &(f->third->third->first);

  // Failure should occur here
  printf("%p, %d\n", field_ptr+1, *(field_ptr+1));

  free(f);

  return 0;
}
