int puts(const char *msg);

int k(const char *msg) {
	puts(msg);
	return 42;
}

int main(void)
{
	double k = 12;

	{
		int k(const char *msg);
		int res = k("Hallo");
		return res != 42;
	}
}
