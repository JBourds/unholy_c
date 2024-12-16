#include <stdio.h>
#include <stdlib.h>

int main() {
    // This is a comment
    int x = 10; /* Another comment */
    float y = 3.14159;
    char *str = "Hello, world!";

    if (x > 5) {
        printf("x is greater than 5\n");
    } else {
        printf("x is less than or equal to 5\n");
    }

    for (int i = 0; i < 10; i++) {
        printf("%d ", i);
    }
    printf("\n");

    while (y > 0) {
        y -= 0.5;
        printf("%.2f ", y);
    }
    printf("\n");

    printf("%s\n", str);

    return 0;
}
