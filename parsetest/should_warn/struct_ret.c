/*$ -Waggregate-return $*/

struct S { int a, b; };
union U { int a, b; };

struct S testS(void) {
	struct S ret = { 1, 2 };

	return ret;
}

union U testU(void) {
	union U ret = { 2 };

	return ret;
}

int main(int argc, char *argv[]) {
	struct S x = testS();
	union U y = testU();

	return 0;
}
