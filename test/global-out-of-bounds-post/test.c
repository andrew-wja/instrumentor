#include <stdlib.h>
#include <stdio.h>

char global_array[13] = "Hello World!\0";
char *global_ptr = global_array;

__attribute__((__noinline__)) void print(char* str) {
  char c = *str;
  printf("%p, %c, %s\n", str, c, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = &global_array[0];
  char * ptr2 = global_ptr;
  print(ptr1);
  print(ptr2);
  // this should not trigger a failure
  print(&global_array[12]);
  // this should trigger a failure
  print(&global_array[13]);
  return 0;
}
