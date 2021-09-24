#include <stdlib.h>
#include <stdio.h>
#include <string.h>

__attribute__((__noinline__)) char * foo() {
  char *f = (char *)malloc(sizeof(char) * 10);
  for (int i = 0; i < 10; i++)
    f[i] = ' ';
  f[9] = 0;
  return f;
}

__attribute__((__noinline__)) char* bad(char* str) {
  if (strlen(str) > 5) {
    return NULL;
  } else {
    return str;
  }
}

__attribute__((__noinline__)) void print(char* str) {
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = foo();
  print(ptr1);
  char* ptr2 = bad(ptr1);
  // this should trigger a failure
  char x = ptr2[3];
  print(&x);
  return 0;
}
