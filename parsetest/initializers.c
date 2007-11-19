int bla = 1;
int bla2 = 2;
int bla3 = { 3 };
int bla4 = { 4, };
/* should fail:
int bla5 = { { 2 } };
int bla6 = { 1, 2 };
int *bla7 = { 2, };
int bla5 = 1, ;
*/

char str1[] = "Hello";
char str2[5] = "Hello";
char str3[10] = "Hello";
signed char str4[] = "Hello";
unsigned char str5[] = "Hello";
/* char str4[4] = "Hello"; unclear wether this should be an error or warning
 * gcc produces a warning, icc an error */

struct foo {
	int a, b;
};

struct foo f1 = { 1, 2 };
struct foo f2 = { { 1, }, 2 }; /* produces a warning on icc and gcc... */
struct foo f3 = { { { 1, } }, 2 }; /* produces a warning on icc and gcc... */

struct foob {
	int a;
	struct foobb {
		float c, d;
	};
	int e;
};

struct foob ff2 = { 1, 2.5, 4, 2 };

union foou {
	int a;
	float b;
};

union foou g1 = { 5 };
