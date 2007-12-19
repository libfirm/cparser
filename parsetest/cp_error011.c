struct bla { int a; };
void g(const struct bla);
void h(struct bla);
void f(void)
{
	const struct bla x = { 0 };
}

int main(void) {
	return 0;
}
