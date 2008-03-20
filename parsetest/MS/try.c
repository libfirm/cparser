int test(int a, int b) {
	__try {
		a = a / b;
	}
	if(0) {
		a = 0;
	}
}

int main(int argc, char *argv[]) {
	int x;
	__try {
		x = argc / 0;
	}
	__finally {
		printf("Should always print this!\n");
	}
	return 0;
}
