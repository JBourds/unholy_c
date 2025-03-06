static int a;
extern int a;

int main(void) {
  a += 1;
  return 0;
}

static int a = 5;

extern int a;

static int a = 7;
