int main(void) {
	void (*var)(void);

	var = (void (*)(void)) 0;

	return (int) var;
}
