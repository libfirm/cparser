unsigned char digest[16] = {""};
unsigned char digest2[16] = {"a"};
int puts(const char *str);

int main(void)
{
	puts((const char*) digest);
	puts((const char*) digest2);
	return 0;
}
