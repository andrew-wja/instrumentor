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

int main(int argc, char * argv[]) {

	char * some_ptr = foo();
        char * some_ptr2 = foo();
        char * some_ptr3 = foo();
        some_ptr3[0] = '-';
        some_ptr3[1] = '-';
        some_ptr3[2] = '-';
        some_ptr2[33] = 'x'; // this should trigger a failure
	printf("%p, %s\n%p, %s\n%p, %s\n", some_ptr, some_ptr,
                                           some_ptr2, some_ptr2,
                                           some_ptr3, some_ptr3);
	free(some_ptr);
        free(some_ptr2);
        free(some_ptr3);
	return 0;
}
