
void t(void)
{
	typedef int       Int;
	typedef const Int CInt;
	typedef CInt      MyCInt;

	MyCInt k;

	k = 5; /* should fail, assignment to const */
}
