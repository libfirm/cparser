int puts(const char *str);

int arr[100];

int main(void) {
	arr[puts("hello1")] += 5;
	arr[puts("hello2")] -= 5;
	arr[puts("hello3")] *= 5;
	arr[puts("hello4")] /= 5;
	arr[puts("hello5")] %= 5;
	arr[puts("hello6")] &= 5;
	arr[puts("hello7")] |= 5;
	arr[puts("hello8")] ^= 5;
	arr[puts("hello9")]++;
	arr[puts("hello10")]--;
	++arr[puts("hello11")];
	--arr[puts("hello12")];
	return 0;
}
