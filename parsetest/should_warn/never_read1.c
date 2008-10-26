/* -w -Wunused-variable */
extern int rand(void);

int main(void)
{
	int x = 23;
	x = rand();
	return 0;
}
