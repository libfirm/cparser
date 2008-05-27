int printf(const char *fmt, ...);

int main(void)
{
	printf("Res: %d\n", (int) '\377');
	return 0;
}
