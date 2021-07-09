#include <stdlib.h>
#include <stdio.h>

char global_array[13] = "Hello World!\0";
char *global_ptr = global_array;

char * foo() {
  char *f = (char *)malloc(sizeof(char) * 13);
  for (int i = 0; i < 13; i++)
    f[i] = 'a'+(char)i;
  f[12] = 0;
  return f;
}

void print(char* str) {
  str[0] = 'a';
  printf("%p, %s\n", str, str);
}

int main(int argc, char * argv[]) {
  char * ptr1 = global_ptr;
  char * ptr2 = foo();

  char * ptr3;

  ptr3 = ((ptr1[3] == 'd') ? ptr2 : ptr1);

  print(ptr1);
  print(ptr2);
  print(ptr3);

  // this should trigger a failure
  free(ptr3);

  print(ptr3);
  return 0;
}
