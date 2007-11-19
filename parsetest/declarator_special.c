int f(void)
{
	return 42;
}

int (* const (fip) (void))(void)
{
	return &f;
}

int main(void) {
	int(*func)(void) = fip();
	return func() == 42 ? 0 : 1;
}
