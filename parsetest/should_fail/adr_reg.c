int test(int *a) {
	return *a;
}

int main() {
	register int a = 3;

	printf("%d\n", test(&a));
}
