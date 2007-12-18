int FirstOne(long long arg1)
{
	union doub {
		unsigned short i[4];
		long long d;
	};

	union doub xx;
	x.d = arg1;
	return x.i[2];
}

int main(void)
{
	return FirstOne(0);
}
