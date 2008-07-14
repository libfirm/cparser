int a,b,c;

int f1(void) {
	while(a < 10)
		return 0;
	return 42;
}

int f2(void) {
	while(0) {
		return 42;
	}
	return 0;
}

int f3(void) {
	while(f2());
	return 0;
}

int f5(void) {
	while(0);
	return 0;
}

int f4(void) {
	goto label;

	while(0) {
label:
		return 0;
	}
	return 42;
}

int f6(void) {
	return 0;

	while(0);
}

int f7(void) {
	while(1) {
		return 0;
	}
}

int endless(void) {
	while(1);
}

int main(void) {
	return f1() + f2() + f3() + f4() + f5() * f6() + f7();
}
