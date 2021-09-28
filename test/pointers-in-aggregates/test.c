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

__attribute__((__optnone__)) int main(int argc, char * argv[]) {
  foo *f = (foo*)malloc(sizeof(foo));
  baz z = {2, 1};
  bar b = {1, 1, &z};
  f->third = &b;

  printf("%p, %d\n", f->third->third, f->third->third->first);

  free(f);

  // Failure should occur here
  printf("%p, %d\n", f->third->third, f->third->third->first);

  return 0;
}
