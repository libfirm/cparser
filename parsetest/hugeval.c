int printf(const char *str, ...);

int main(void) {
	printf("%f\n", __builtin_huge_val());
	return 0;
}
