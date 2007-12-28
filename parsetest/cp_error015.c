void (*var) (void);

extern void puts(void);

void (*var) (void) = puts;

int main(void)
{
	return 0;
}
