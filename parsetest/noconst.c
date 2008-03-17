int func(void) { return 0; }

int (*ptr)(void) = **func;

int main(void) {
	return ptr();
}
