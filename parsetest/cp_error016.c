
typedef struct Person {
	int bla;
} Person;

int f(void) {
	Person *Person;
}

int bla(Person *p) {
	return p->bla;
}

int main(void)
{
	Person p = { 0 };
	return bla(&p);
}
