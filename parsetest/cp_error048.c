static void f(void) {}
static void g(void p(void)) {}

int main(void)
{
	g(f);
	return 0;
}
