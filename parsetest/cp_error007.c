struct Bla {
	struct arc *ptr;
};

typedef struct arc
{
	struct Bla bla;
} arc_t;

struct arc k;

int main(void)
{
	k.bla.ptr = &k;
	k.bla.ptr->bla.ptr = &k;

	return 0;
}
