#define fool(x)    bar

int printf(const char *str, ...);

void barfool(void)
{
	printf("Hello World\n");
}

int main(void)
{
	/* If you preprocess this with EDG and then compile it, it gets invalid
	 * because EDG emits 2 identifiers directly behind each other (without
	 * space in between them) */
	void (*func)(void) = fool(2)fool
;
	func();
	return 0;
}
