int printf(const char *str, ...);

__int8          blup = 42;
signed __int8   blup2 = 43;
unsigned __int8 blup3 = 44;
__int64          blup4 = 0xfffffff00000045;
unsigned __int64 blup5 = 0xffffffff00000045;

int main(void)
{
	printf("%d %d %u %I64d %I64u\n", blup, blup2, blup3, blup4, blup5);
}
