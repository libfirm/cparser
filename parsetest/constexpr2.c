struct foo {
	int a;
	int b;
};
struct foo bar = { (int) &((struct foo*)0)->b, 0 };

int main(void) {
	return 0;
}
