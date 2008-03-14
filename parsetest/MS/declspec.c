int __declspec(align(4)) x;

int __declspec(dllimport) y;
int __declspec(dllexport, align(4)) z;

int __declspec(noinline, naked, deprecated("Mist"))func(void);

struct x {
	int __declspec(property(get=get_a, put=put_a)) a;
};

__declspec(restrict) char * malloc_like();
int __declspec(noalias) test1(void *a, void *b);
