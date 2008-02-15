int main(void) {
	int k = sizeof( (int[]) { 1, 2, 3 } ) / sizeof(int);

	if(k != 3)
		return 1;

	return 0;
}
