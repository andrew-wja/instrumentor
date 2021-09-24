#include <stdlib.h>
#include <stdio.h>

char global_array[13] = "Hello World!\0";
char *global_ptr = global_array;

__attribute__((__noinline__)) void bad(char* str) {
  free(str);
}

__attribute__((__noinline__)) void print(char* str) {
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = &global_array[0];
  char * ptr2 = global_ptr;
  print(ptr1);
  print(ptr2);
  // this should trigger a failure
  bad(ptr1);
  return 0;
}
