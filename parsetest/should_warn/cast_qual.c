/*$ -Wcast-qual $*/
void test(const char *p) {
	char *q = (char *)p;
	volatile char *r = q;
	int *i = p;

	(void)p;

	*q = 1;
}

int main(int argc, char *argv[]) {
	return 0;
}
