int printf(const char *str, ...);

int printpass(int x) {
	printf("Val: %d\n", x);
	return x;
}

int f(int g) {
	int k = g * 2;

	printf ("pass1\n");

	int arr[printpass(k)];
	printf ("pass2: %zd\n", sizeof(arr));
	return 0;
}

int main(void) {
	return f(10);
}
