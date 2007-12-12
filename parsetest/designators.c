#include <stdio.h>

int a() {
	printf("a\n");
	return 1;
}

int b() {
	printf("b\n");
	return 2;
}

int c() {
	printf("c\n");
	return 3;
}

int main(void)
{
	int arr[] = { [1] = a(), [0] = b(), [1] = c(), [2] = a() };
	int size  = sizeof(arr) / sizeof(arr[0]);

	for(int i = 0; i < size; ++i) {
		printf("%d ", arr[i]);
	}
	printf("\n");

	return 0;
}
