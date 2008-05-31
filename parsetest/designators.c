#include <stdio.h>

int a() {
	return 1;
}

int b() {
	return 2;
}

int c() {
	return 3;
}

int main(void)
{
	int arr[] = { [1] = a(), [0] = b(), [1] = c(), [2] = a() };
	int size  = sizeof(arr) / sizeof(arr[0]);

	printf("Size: %d\n", size);

	for(int i = 0; i < size; ++i) {
		printf("%d ", arr[i]);
	}
	printf("\n");

	return 0;
}
