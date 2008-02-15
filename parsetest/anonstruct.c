int printf(const char *str, ...);

typedef struct {
	struct {
		union {
			int    a;
			double b;
		};
		struct {
			float c;
			int   k;
		};
	} v;

	int l;
} bla;

bla bl;

int main(void)
{

	bl.v.a = 12;
	bl.l = 32;
	bl.v.c = 23.4;

	printf("%d\n", (char*)&bl.v.c - (char*)&bl);

	return 0;
}
