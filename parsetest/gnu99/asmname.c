int foo asm ("myfoo") = 0;

extern func () asm ("FUNC");

int func(int x) {
	return x;
}

int main(int argc, char *argv[]) {
	return func(foo);
}
