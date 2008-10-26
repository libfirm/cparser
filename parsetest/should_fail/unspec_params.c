
void f();

void f() {
}

int main(void)
{
	/* should at least give a warning */
	f(5);
	return 0;
}
