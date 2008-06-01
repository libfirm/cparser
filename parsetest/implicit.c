void test1() {
	puts("Hello");
}

void test2() {
	int i;
	for(i = rand(); i < 20; ++i)
		break;
}

int main(void) {
	test1();
	test2();
	return 0;
}
