struct A {
	int k;
	struct B {
		int a, b[4];
	} blup[2];
	struct C {
		int a : 5;
		int b : 3;
	} c;
};

int k = __builtin_offsetof(struct A, blup[2].b[3]);

int main(void)
{
	return k == 0;
}
