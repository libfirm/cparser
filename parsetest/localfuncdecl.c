int puts(const char *msg);

int k(const char *msg) {
	puts(msg);
	double gl = 12;
	{
		extern int gl;
		return gl;
	}
}

int gl = 42;

int main(void)
{
	double k = 12;

	{
		int k(const char *msg);
		int res = k("Hallo");
		return res != 42;
	}
}
