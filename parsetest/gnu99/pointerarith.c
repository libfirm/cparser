typedef int (*func)(void);

void *add(void *p) {
	return p + 1;
}

func next(func f) {
	return f + 1;
}

int main(void) {
	void *a = 0;
	a += 5;
	return 0;
}
