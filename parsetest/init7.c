struct S {
	int a, b;
} glob;

struct S *globptr  = &glob;
struct S *globptr2 = &*&glob;
int      *intptr   = &*&glob.b;
int      *intp2    = &(&glob + 3)->b;

int arr[10];
int *p = arr + 4;

int main(void)
{
	return 0;
}
