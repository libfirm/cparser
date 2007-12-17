extern char ofname[];
char ofname[1024];
char ofname[];

int main(void)
{
	return sizeof(ofname) != 1024;
}
