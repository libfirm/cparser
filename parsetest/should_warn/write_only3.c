extern int rand(void);

int main(void)
{
	for(int z = 0; rand() % 2 != 0; ++z)
		z;
	return 0;
}
