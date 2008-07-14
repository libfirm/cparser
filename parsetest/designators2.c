#include <assert.h>

typedef struct blub {
	int i;
	char a[4];
} blub;

blub a = { .a[2] = 2, 3, .i = 23 };

int main(void)
{
	assert(a.a[2] == 2);
	assert(a.a[3] == 3);
	return 0;
}
