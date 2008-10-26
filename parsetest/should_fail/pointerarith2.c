int main(void) {
	int (*a)(void) = main;
	a += 2;
	return 0;
}
