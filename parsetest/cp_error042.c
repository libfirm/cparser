static int f(void);

int f(void) {
	return 42;
}

static int f(void);

int main(void)
{
	return f() != 42;
}
