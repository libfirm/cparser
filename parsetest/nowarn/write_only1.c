struct X { int i[10]; int j; struct X* next; } gX;

int main(void)
{
	struct X* x4 = &gX;
	x4->j++;
	return 0;
}
