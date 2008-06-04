struct A {
	int a, b;
};

static struct A
deduce_conversion(int from, int to)
{
	struct A result = { 1, 2 };
	return result;
}

struct A globa[4];
int x = 1;

int main(int argc, char **argv)
{
	globa[x] = deduce_conversion(1, 2);
	return 0;
}
