static const int l = 5;

struct k {
	int c, d;
};

struct S {
	int a;
	struct k k;
};

struct S arr[] = { 1, (struct k) { 4, 5 } };

typedef int k[];

k var = { 1, 2, 3, 4, 5 };
k var2 = { 1, 2, 3, 4 };

int main(void)
{
	if((sizeof(var2)/sizeof(int)) != 4)
		return 1;

	if((sizeof(var)/sizeof(int)) != 5)
		return 2;

	return 0;
}
