int f(int a) {
	int g(int k) {
		return k + a;
	}

	return g(1);
}

int main(int argc, char *argv[]) {
	return f(-1);
}
