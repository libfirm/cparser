static void f(void) {
}

void (*ptr)(void);

void init(void) {
	ptr = f;
}

int main(void)
{
	return 0;
}
