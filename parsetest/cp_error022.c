typedef struct {
	const char *string;
} symbol_t;

typedef struct {
	int type;
	union {
		symbol_t  *symbol;
		long long  blo;
	} v;
} token_t;

int main(void)
{
	if(__builtin_offsetof(token_t, v.symbol) != sizeof(int))
		return 1;
	return 0;
}
