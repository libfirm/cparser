int printf(const char *fmt, ...);

static void foo();

static void kaputt(void) {
	foo(1);
}

static void foo(int a, int b) {
	(void) a;
	(void) b;
}

int main(void) {
	kaputt();
	return 0;
}
