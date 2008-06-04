
struct A {
	int a, b;
};

enum typecode { C1, C2 };

static struct A
deduce_conversion(from, to)
	enum typecode from, to;
{
	struct A result = { 1, 2 };
	return result;
}

struct A globa[4];
int x = 1;

int main(int argc, char **argv)
{
	globa[x] = deduce_conversion(C1, C2);
	return 0;
}
