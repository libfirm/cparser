struct X {
	int x;
};

struct X bla = { x: 23 };

int main(void)
{
	printf("%d\n", bla.x);
	return 0;
}
