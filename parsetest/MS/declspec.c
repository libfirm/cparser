#include "declspec.h"

int test2(void) {
	return 0;
}

int __declspec(align(4)) x;

int __declspec(dllimport) y;
int __declspec(dllexport, align(4)) z;

int __declspec(noinline, naked, deprecated("Mist"))func(void)
{
	return 42;
}

struct x {
	int __declspec(property(get=get_a, put=put_a)) a;
};

__declspec(restrict) char * malloc_like();
int __declspec(noalias) test1(void *a, void *b);

void test3(void) {
	test2();
	func();
}

__declspec(deprecated) int (*ptr)(void) = ****test2;

int main(void) {
	int x  = ptr();
	printf("%I64d\n", x);
	return x;
}
