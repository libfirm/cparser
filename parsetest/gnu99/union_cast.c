union foo { int i; double d; };
int x;
double y;

union foo u;

void hack (union foo p) {
	y = p.d;
}

int main(int argc, char *argv[]) {
	u = (union foo) x;

	hack ((union foo) x);
	return 0;
}
