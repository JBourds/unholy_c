int main(void) {
    static double zero = 0.0;
    double nan = 0.0 / zero; // make this constant-folding proof

    if (nan) {
    } else {
      return 1;
    }

    return 0;
}
