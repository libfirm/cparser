int f(int a) {
	__label__ end;

	int g(int k) {
		if (k == 0)
			goto end;
		return k + a;
	}

	return g(1);
end:
	return 23;
}

int main(int argc, char *argv[]) {
	return f(-1);
}
