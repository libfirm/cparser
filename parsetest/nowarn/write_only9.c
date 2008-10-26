int main(void)
{
	int d = 23;
	int e = 42;
	asm("" : "+r" (e) : "r" (d));
	return 0;
}
