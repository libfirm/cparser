static int test(int a, int b);

static int adr(int *x);

static int test1(int a, int b) {
	return test(a,b);
}

static int test2(int a, int b) {
	int arr[2];

	arr[0] = a;
	arr[1] = b;

	adr(arr);
	return arr[0] + arr[1];
}

int main(void)
{
	return 0;
}
