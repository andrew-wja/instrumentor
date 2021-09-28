#include <stdlib.h>
#include <stdio.h>

__attribute__((__optnone__)) int main(int argc, char * argv[]) {
  char *t = malloc(1);
  // this should not cause any checks to be generated
  *t = 'a';

  printf("%p, %c\n", t, *t);
  free(t);
  return 0;
}
