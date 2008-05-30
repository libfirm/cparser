int a;
int b;

int main(int argc, char **argv) {
	(void) argv;
	argc ? a = b : b = a;
	return 0;
}
