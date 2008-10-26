struct X { int i[10]; int j; struct X* next; } gX;

extern int rand(void);

int main(void)
{
	for (struct X* px = &gX; rand() % 2 == 0; px = px->next)
	{}

	return 0;
}
