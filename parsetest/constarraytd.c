typedef float vec3_t[3];

void f(const vec3_t foo) {
}

int main(void) {
	const float *bla = 0;
	f(bla);
	return 0;
}
