typedef int (function)(int);
typedef void broken_array[8];
typedef int int_array[8];

broken_array x;

function test(void);

broken_array test2(void);
int_array test3(void);
