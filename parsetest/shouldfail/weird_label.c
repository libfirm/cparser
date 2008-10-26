#include <stdio.h>

int main(int argc, char *argv) {
	switch (argc) {
		({ weird: printf("It's forbidden to jump here\n"); argc; });
	case -1:
		break;
	default:
		goto weird;
	}
	return 0;
}
