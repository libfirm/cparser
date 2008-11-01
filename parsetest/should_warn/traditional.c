/*$ -Wtraditional $*/

int t1(int a) { return a; }
int t2(double a) { return a; }
int t3(char a) { return a; }

int x() {
	int i = +3;
	return t1(0.0) + t2(0) + t3('a');
}

int sw1(long x) {
	switch(x) {
	default: return x;
	}
}

int sw2(unsigned x) {
	switch(x) {
	default: return x;
	}
}

char *str() {
	return "a" "b";
}

int suff(void);

int suff() {
	unsigned a = 1U;
	float f = 1.0F;
	double d = 1.0L;
	unsigned long long bla = 1ULL;
	return (int) a;
}

int main(void)
{
	return 0;
}
