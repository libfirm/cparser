int puts(const char *msg);

int main(int argc, char **argv)
{
	char *arg = argc > 1 ? argv[1] : 0;

	if(arg != 0) {
		puts(arg);
	}

	return 0;
}
