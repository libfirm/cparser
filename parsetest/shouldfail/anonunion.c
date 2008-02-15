typedef struct {
	int a;
	union {
		struct {
			double a;
		};
	};

	float b;
} blup;

int main(void)
{
	blup b;
	return 0;
}
