long ret42(void)
{
  long retval;
  __asm__("movl $42, %0"  : "=m" (retval));
  return retval;
}

int main(void)
{
	return ret42() != 42;
}
