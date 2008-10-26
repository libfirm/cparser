int A;

int main(void)
{
	int *a = &A;
	a = &a[*a];

	return 0;
}
