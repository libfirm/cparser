int puts(const char *msg);

int main(int argc, char **argv)
{
	void *nu = 0;
	char *arg1 = argc > 1 ? argv[1] : 0;
	char *arg2 = argc <= 1 ? 0 : argv[1];
	char *arg3 = argc > 1 ? argv[1] : nu;
	char *arg4 = argc <= 1 ? nu : argv[1];

	int blo = (int) (argc < 100 ? 0 : argv[1]);

	if(arg1 != 0) {
		puts(arg1);
		puts(arg2);
		puts(arg3);
		puts(arg4);
	}

	return blo;
}
