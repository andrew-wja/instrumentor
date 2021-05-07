#include <stdlib.h>
#include <stdio.h>

char * foo() {
  char *f = (char *)malloc(sizeof(char) * 10);
  for (int i = 0; i < 10; i++)
    f[i] = ' ';
  f[9] = 0;
  return f;
}

void bad(char* str) {
  free(str);
}

void print(char* str) {
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = foo();
  print(ptr1);
  // this should trigger a failure
  bad(ptr1);
  free(ptr1);
  return 0;
}
