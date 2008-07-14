int printf(const char *str, ...);

int main(void) {
	struct {
		int a : 8;
		int b : 12;
	} s;

	s.b = 0xff;
	s.a = 0;
	s.a += 20;
	s.a /= 2;
	s.a %= 4;
	s.a -= 12;
	s.a++;
	s.b--;
	printf("%d %d\n", s.a, s.b);
	return 0;
}
