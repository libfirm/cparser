struct S {
	int a, b;
} glob;

struct S *globptr  = &glob;
struct S *globptr2 = &*&glob;
int      *intptr   = &*&glob.b;
int      *intp2    = ((int*) ((short) &intptr));

int main(void)
{
	return 0;
}
