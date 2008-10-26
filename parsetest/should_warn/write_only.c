struct X { int i[10]; int j; struct X* next; };

void f(int parm)
{{
#if 0
	parm++;
	int x = 0;
	int y = 1;
	x = x + y;
	for (int z = 0;; ++z) {
		z;
	}
	int* a = 0;
	a = &a[*a];
	int* b = 0;
	b = b[*b] = 0;
	for (struct X* px = 0;; px = px->next) {}
	struct X* x2 = 0;
	x2->next = 0;
	int c;
	c = *&c = 0;
	int d = 23;
	int e = 42;
	asm("" : "+r" (e) : "r" (d));
	struct X* x3 = 0;
	x3->i[2] = 0;
	int i[10];
	i[0] = 1;
	int j[10];
	j[j[j[0]]] = 23;
#endif
	struct X* x4 = 0;
	x4->j++;
}}
