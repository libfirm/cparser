static void foo();

static void kaputt(void) {
	foo(1);
}

static void foo(int a, int b) {
	(void) a;
	(void) b;
	printf("%d %d\n", a, b);
}

int main(void) {
	kaputt();
	return 0;
}
