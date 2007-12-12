typedef int (function)(int);
typedef void broken_array[8];
typedef int int_array[8];

broken_array x;

typedef function func2(void);

func2 test;

broken_array test2(void);
int_array test3(void);
