struct x {
  unsigned x1:1;
  unsigned x2:2;
  unsigned x3:3;
  unsigned x4:4;
};

void foobar (int x, int y, int z)
{
  struct x a = {x, y, z, 7};
  struct x b = {x, y, z, 3};
  struct x *c = &b;

  c->x3 += (a.x2 - a.x1) * c->x2;
  if (a.x1 != 1 || c->x3 != 5)
    abort ();
  exit (0);
}

int main()
{
  foobar (1, 2, 3);
}
