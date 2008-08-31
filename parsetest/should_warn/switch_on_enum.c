enum e {
	a, b, c
};

enum e E;

int main(int argc, char *argv[]) {
	switch (E) {
	case a: return 1;
	}
	return 0;
}
