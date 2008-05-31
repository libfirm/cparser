int main(int argc, char *argv[]) {
	enum { A, B, C } a = argc;
	(void) argv;

	return a != B;
}
