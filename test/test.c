#include <stdlib.h>
#include <stdio.h>

//Just allocate a buffer of size 10
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
  char * some_ptr = foo();
  char * some_ptr2 = foo();
  char * some_ptr3 = foo();

  // this should trigger a failure
  bad(some_ptr2);

  print(some_ptr);
  print(some_ptr2);
  print(some_ptr3);

  free(some_ptr);
  free(some_ptr2);
  free(some_ptr3);

  // temporal load dereference check
  //~ char test = some_ptr[9];
  // temporal store deference check
  //~ some_ptr[9] = '-';

  //~ print(some_ptr);
  //~ print(some_ptr2);
  //~ print(some_ptr3);

  return 0;
}
