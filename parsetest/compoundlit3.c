int a, b, c;

typedef struct S {
	int k[3];
	struct lku {
		double d, e;
	} elem;
} S;
S kg;

int main(void) {
	S k = { a, b };

	kg = k;

	return 0;
}
