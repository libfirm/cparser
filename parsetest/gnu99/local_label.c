extern void puts(const char *str);

int test(int a, int b) {
	puts("here");
	if (a) {
		__label__ label;
		puts("P1");
		if (b) {
			puts("before goto");
			goto label;
		}
		puts("behind if");
		b = 1;
label:
label2:
		puts("behind label (1)");
		++b;
	}

	if (a) {
		__label__ label;
		puts("P2");
		goto label;
		if (a) {
			__label__ label;
label3:
			puts("behind label3");
			goto label;
label: ;
			puts("behind label (2)");
		}
		if (b) {
			puts("goto fininsh");
			goto finish;
		}
		b = 1;
label:
		puts("behind label (3)");
		++b;
		goto label3;
	}
finish:
	puts("finished");
	return b;
}

int main(int argc, char *argv[]) {
	test(1, 42);
	return 0;
}
