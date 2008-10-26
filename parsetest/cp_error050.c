int main(void)
{
	// This is sizeof of a compound literal, not sizeof a type followed by garbage
	return sizeof(int[]){ 0, 1, 2 } != 3*sizeof(int);
}
