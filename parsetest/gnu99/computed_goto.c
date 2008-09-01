int test(int x) {
	static void *array[] = { &&foo, &&bar, &&hack };

	goto *array[x];
foo:	return 1;
bar:	return 2;
hack:	return 3;
}

int main(int argc, char *argv[]) {
	return test(1) == 2 ? 0 : 1;
}
