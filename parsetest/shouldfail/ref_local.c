int *test(void) {
	int x = 3;

	return &x;
}

int main() {
	return *test();
}
