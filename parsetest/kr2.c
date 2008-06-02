int printf(const char *str, ...);

int a(int first, float second, const char *third);

int main(void)
{
	a(42, 42.42, "What is 6 times 9?\n");
	return 0;
}

int a(first, second, third)
	float second;
	const char *third;
	int first;
{
	printf("Args: %d %f %s\n", first, second, third);
	return 0;
}
