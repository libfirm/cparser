static const int l = 5;

struct k {
	int c, d;
};

struct S {
	int a;
	struct k k;
};

struct S arr[] = { 1, (struct k) { 4, 5 } };

int main(void)
{
	return 0;
}
