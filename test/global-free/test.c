#include <stdlib.h>
#include <stdio.h>

char global_array[13] = "Hello World!\0";

void bad(char* str) {
  free(str);
}

void print(char* str) {
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = &global_array[0];
  print(ptr1);
  // this should trigger a failure
  bad(ptr1);
  return 0;
}
