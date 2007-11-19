
struct Bla {
	struct blup {
		int a, b;
	} d;
	int c;
};

struct blup f = { 1, 2 };

int func(void)
{
	struct blup {
		char str[20];
	};
	struct blup b = { "hullo" };
	return 0;
}

struct blup k = { 1, 2};
