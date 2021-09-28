#include <stdlib.h>
#include <stdio.h>

char global_array[13] = "Hello World!\0";
char *global_ptr = global_array;

__attribute__((__noinline__)) char * foo() {
  char *f = (char *)malloc(sizeof(char) * 13);
  for (int i = 0; i < 13; i++)
    f[i] = 'a'+(char)i;
  f[12] = 0;
  return f;
}

__attribute__((__noinline__)) void print(char* str) {
  str[0] = 'a';
  printf("%p, %s\n", str, str);
}

__attribute__((__optnone__)) int main(int argc, char * argv[]) {
  char * ptr2 = foo();

  char ** ptr3;

  ptr3 = ((global_ptr[3] == 'd') ? &ptr2 : &global_ptr);

  print(global_ptr);
  print(ptr2);
  print(*ptr3);

  // this should trigger a failure
  free(*ptr3);

  print(*ptr3);
  return 0;
}
