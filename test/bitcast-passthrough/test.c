#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

char * foo() {
  char *f = (char *)malloc(sizeof(char) * 10);
  for (int i = 0; i < 10; i++)
    f[i] = 'a'+(char)i;
  f[9] = 0;
  return f;
}

void bad(int* str) {
  (*(char*)str) = 'a';
}

void print(char* str) {
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = foo();
  char * ptr2 = foo();

  char * ptr3;

  ptr3 = ((ptr1[3] == 'd') ? ptr2 : ptr1);

  print(ptr1);
  print(ptr2);
  print(ptr3);

  free(ptr1);
  free(ptr2);

  // this should trigger a failure
  bad((int*)ptr3);

  print(ptr3);
  return 0;
}
