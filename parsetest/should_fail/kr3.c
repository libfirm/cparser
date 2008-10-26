extern int k;

int a(first, second, third)
	const char *third;
	float first;
	int second;
	int k;
	float l;
{
	printf("Args: %d %f %s\n", first, second, third);
	return 0;
}

int main(void)
{
	a(42.32, 42, "What is 6 times 9?\n");
	return 0;
}
