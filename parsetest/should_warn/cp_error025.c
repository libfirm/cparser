struct s {
	char x[30];
};

struct s x[] = { "blup" };

int main(void)
{
	return x[0].x[0] != 'b';
}
