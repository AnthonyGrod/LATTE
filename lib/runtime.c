#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

void printString(char* str) {
    printf("%s\n", str);
}

void printInt(int num) {
    printf("%d\n", num);
}

int readInt() {
    int num;
    if (scanf("%d\n", &num) != 1) {
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
    if (!dest || !src) {
        return NULL;
    }
    size_t len1 = strlen(dest);
    size_t len2 = strlen(src);
    char* new_str = malloc(len1 + len2 + 1);
    if (!new_str) {
        return NULL;
    }
    strcpy(new_str, dest);
    strcat(new_str, src);
    return new_str;
}

bool _strcmp(char* str1, char* str2) {
    return strcmp(str1, str2) == 0;
}

char* _strcpy(char* dest, char* src) {
    return strcpy(dest, src);
}
