typedef struct {
	int a, b;
} Stru1;

typedef struct {
	double d;
	Stru1 stru;
	float c;
} Stru2;

static const Stru1 k = { 4, 2 };
static const Stru2 a = { 2.4, k, 0.4f };
