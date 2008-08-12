typedef int i16 __attribute__ ((__mode__ (__HI__))), i32;
/*typedef int i16;*/

int size16 = sizeof(i16);
/*int size32 = sizeof(i32); */

i16 p16;
short *ps16 = &p16;

int main(void) {
	return size16 != 2;
}
