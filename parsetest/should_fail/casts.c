char *p1 = (char *)0.0;
char *p2 = (char *)4.5;

struct S { int a; } x;
struct T { int b; } y = (struct T)x;
