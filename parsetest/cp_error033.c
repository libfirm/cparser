struct A {
	int a, b;
};

static struct A deduce_conversion(int from, int to)
{
	struct A result = { 1, 2 };
	return result;
}

struct A *globa;

int main(int argc, char **argv)
{
	*globa = deduce_conversion(1, 2);
	return 0;
}
