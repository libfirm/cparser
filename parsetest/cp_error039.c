#pragma pack(push,2)
typedef struct teststruct {
	char a;
	int b;
} teststruct_t;
#pragma pack(pop)

typedef struct teststruct2 {
	char a;
	int b;
} teststruct2_t;

int main(void)
{
	struct teststruct t;
	struct teststruct2 t2;

	memset(&t, 0, sizeof(t));
	t.a = 0xEF;
	t.b = 0x12345678;

	memset(&t2, 0, sizeof(t2));
	t2.a = 0xEF;
	t2.b = 0x12345678;

	printf("%.8X %.2X %.8X %.2X\n", *(unsigned int *) &t,
		(unsigned int) *((unsigned char *) &t + 4),
		*(unsigned int *) &t2,
		(unsigned int) *((unsigned char *) &t2 + 4));

	return 0;
}
