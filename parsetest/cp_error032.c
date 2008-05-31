int printf(const char *str, ...);

int main(void) {
	printf("%d\n", (int) sizeof (const void*));
	return 0;
}
