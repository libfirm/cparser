struct { char str[10]; int y; } bla[] = { [13].str = { 1, 2,3 } };

int main() {
	return bla[13].str[1] != 2;
}
