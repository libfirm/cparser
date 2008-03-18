extern int puts(const char *s);

const char *test1(void) {
	return __func__;
}

const char *test2(void) {
	return __FUNCTION__;
}

#ifdef __GNUC__
const char *test3(void) {
	return __PRETTY_FUNCTION__;
}
#endif

int main(void) {
	puts(test1());
	puts(test2());
#ifdef __GNUC__
	puts(test3());
#endif
}
