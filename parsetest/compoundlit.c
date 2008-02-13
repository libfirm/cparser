int puts(const char *str);

struct bla {
	int a;
	float b;
	char c[20];
};

int main(void)
{
	struct bla a;
	a = (struct bla) { 4, 5.2, "Hello" };
	puts(a.c);
	return 0;
}
