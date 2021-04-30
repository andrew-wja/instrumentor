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
  str[33] = 'x';
}

void print(char* str) {
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = foo();
  char * ptr2 = foo();
  char * ptr3 = foo();

  // this should trigger a failure
  bad(ptr2);

  print(ptr1);
  print(ptr2);
  print(ptr3);

  free(ptr1);
  free(ptr2);
  free(ptr3);

  return 0;
}
