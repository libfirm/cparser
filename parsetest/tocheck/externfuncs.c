static void puts(void) {
	extern int puts(const char *str);
	puts("myputs");
}

int main(void) {
	puts();
	return 0;
}
