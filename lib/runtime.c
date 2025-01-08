#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printString(char* str) {
    printf("%s\n", str);
}

void printInt(int num) {
    printf("%d\n", num);
}

int readInt() {
    int num;
    if (scanf("%d", &num) != 1) {
        fprintf(stderr, "Error reading integer.\n");
        exit(1);
    }
    return num;
}

char* readString() {
    char* buffer = malloc(256);
    if (!fgets(buffer, 256, stdin)) {
        fprintf(stderr, "Error reading string.\n");
        exit(1);
    }
    buffer[strcspn(buffer, "\n")] = '\0';
    return buffer;
}

void error() {
    fprintf(stderr, "Runtime error\n");
    exit(1);
}

int _strlen(char* str) {
    return (int)strlen(str);
}

char* _strcat(char* dest, char* src) {
    return strcat(dest, src);
}

int _strcmp(char* str1, char* str2) {
    return strcmp(str1, str2);
}

char* _strcpy(char* dest, char* src) {
    return strcpy(dest, src);
}
