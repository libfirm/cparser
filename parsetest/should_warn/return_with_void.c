void test(unsigned a, unsigned b) {
	unsigned x = a - b;
	if (a >= b)
		return x;
	return 0;
}

int main(int argc, char *argv[]) {
	test(3,4);
	return 0;
}
