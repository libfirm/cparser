extern int puts(const char *s);

const char *test1(void) {
	return __func__;
}

const char *test2(void) {
	return __FUNCTION__;
}

const char *test3(void) {
	return __FUNCSIG__;
}

const char *test4(void) {
	return __FUNCDNAME__;
}

int main(void) {
	puts(test1());
	puts(test2());
	puts(test3());
	puts(test4());
}
