struct X { int i[10]; int j; struct X* next; } gX;

int main(void)
{
	struct X* x2 = &gX;
	x2->next = 0;
	return 0;
}
