#include <stdio.h>

struct x {};
struct x a[] = { {}, {} };

int size = sizeof(a);

int main() {
	printf("%d\n", sizeof(a));
}
