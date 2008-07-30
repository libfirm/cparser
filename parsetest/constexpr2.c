struct foo {
	int a;
	int b;
};
int glob = (int) &((struct foo*)0)->b;

int main(void) {
	return 0;
}
