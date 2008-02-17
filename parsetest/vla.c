int puts(const char *str);
void *memcpy(void *dest, const void *src, unsigned int n);

int get_size(void)
{
	puts("sideeffect");
	return 10;
}

int get_size2(void)
{
	puts("sideeffect2");
	return 10;
}

int gk[10];

int main(int argc, char **argv)
{
	int k[get_size()];
	int c[argc][argc];
	int z[10];

	if(sizeof(k)/sizeof(int) != 10)
		return 1;
	if(sizeof(int[get_size2()])/sizeof(int) != 10)
		return 2;
	sizeof(c[puts("effect")]);
	sizeof(c[puts("effect2")][puts("but not this time")]);
	sizeof(z[puts("no effect")]);

	k[5] = 42;
	k[2] = 10;
	memcpy(gk, k, sizeof(gk));

	return 0;
}
