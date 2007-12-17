typedef struct arc
{
	struct arc *nextout, *nextin;
	long ident;
} arc_t;

struct arc k;
arc_t l;

int main(void)
{
	k = l;
	return 0;
}
