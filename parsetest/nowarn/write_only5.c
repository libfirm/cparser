int A;

int main(void)
{
	int *a = &A;
	a[*a] = 0;

	return 0;
}
