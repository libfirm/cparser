int puts(const char *str);

int get_size(void)
{
	puts("sideeffect");
	return 10;
}

int main(void)
{
	int k[get_size()];

	if(sizeof(k)/sizeof(int) != 10)
		return 1;

	return 0;
}
