int test(int a, int b) {
  if (a) {
    __label__ label;
    if (b)
      goto label;
    b = 1;
label:
    ++b;
  }
  if (a) {
    __label__ label;
    if (b)
      goto label;
    b = 1;
label:
    ++b;
  }
  return b;
}

int main(int argc, char *argv[]) {
  return 0;
}
