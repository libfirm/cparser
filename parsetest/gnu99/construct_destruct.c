int puts(const char *str);

void __attribute__((constructor)) construct(void)
{
	puts("Hello");
}

void __attribute__((destructor)) destruct(void)
{
	puts("Goobye");
}

int main(void)
{
	puts("Main");
	return 0;
}
