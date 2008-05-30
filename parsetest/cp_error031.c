int foo();

int kaputt(void) {
	return foo(42, 2, 3);
}

int foo(int a) {
	return a;
}

int main(void) {
	return kaputt() != 42;
}
