#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Test case 1: Use After Free
void test1() {
    int *ptr = malloc(sizeof(int));
    if (ptr == NULL) {
        perror("Failed to allocate memory");
        return;
    }
    *ptr = 42;
    free(ptr); // Memory is freed
    printf("Attempting to access freed memory: %d\n", *ptr); // Use after free
}

// Test case 2: Buffer Overflow
void test2() {
    char buffer[10];
    strncpy(buffer, "Too long string", sizeof(buffer) - 1); // Prevents overflow, leaves space for null terminator
    buffer[sizeof(buffer) - 1] = '\0'; // Ensure null termination
    printf("Buffer content: %s\n", buffer);
}

// Test case 3: Double Free
void test3() {
    int *ptr = malloc(sizeof(int));
    if (ptr == NULL) {
        perror("Failed to allocate memory");
        return;
    }
    free(ptr); // First free
    printf("Attempting to free memory again\n");
    free(ptr); // Double free
}

int main() {
    test1();
    test2();
    test3();
    return 0;
}