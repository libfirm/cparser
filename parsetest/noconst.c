int func(void) { return 0; }

int (*ptr)(void) = func;
int (*ptr2)(void) = *&*&func;

int main(void) {
	return ptr2() + ptr();
}
