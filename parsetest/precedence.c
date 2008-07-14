int printf(const char *std, ...);

int h(int a, int b) {
	return 1 << a % b;
}

int main(void) {
	printf("%d\n", h(5, 2));
	return 0;
}
