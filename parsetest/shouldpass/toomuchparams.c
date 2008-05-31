int puts(const char *str);

int test(s)
	char *s;
{
	puts(s);
}

int main(void) {
	test("Hello World", 0);
	return 0;
}
