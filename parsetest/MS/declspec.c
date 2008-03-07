int __declspec(align(4)) x;

int __declspec(dllimport) y;
int __declspec(dllexport) z;

int __declspec(noinline naked)func(void);
