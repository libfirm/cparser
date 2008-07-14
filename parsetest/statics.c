int printf(const char *str, ...);

void f(void)
{
	static int k = 42;
	{
		static int k = 13;
		++k;
		printf("%d ", k);
	}
	--k;
	printf("%d\n", k);
}

int main(void)
{
	f();
	f();
	f();
	f();
	return 0;
}
