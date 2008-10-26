struct X { int i[10]; int j; struct X* next; } gX;

int main(void)
{
	struct X* x3 = &gX;
	x3->i[2] = 0;
	return 0;
}
