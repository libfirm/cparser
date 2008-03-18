int test1(int a) __attribute__((noreturn));
int test2(int a) __attribute__((const));
/* empty args are allowed */
int test3(int a) __attribute__((weak()));
int test4(int a) __attribute__((unknown_error("bla", 3)));
int test5(int a) __attribute__((alias("test2")));
int test6(int a) __attribute__((section("extra")));
int test7(int a, const char *fmt, ...) __attribute__((format(printf,2,3)));

struct X {
	char y;
	int x __attribute__((aligned(4+4)));
};

int test2(int a) {
	return a;
}

int main(void) {
	return test5(0);
}
