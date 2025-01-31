int foo(int a) {
    return a;
}

int main(void) {
    // Because it specified no args, this is interpreted as "any number of args"
    int foo();
    return foo(5);
}
