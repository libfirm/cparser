int test(int a, int b);

int adr(int *x);

int test1(int a, int b) {
	return test(a,b);
}

int test2(int a, int b) {
	int arr[2];

	arr[0] = a;
	arr[1] = b;

	adr(arr);
	return arr[0] + arr[1];
}
