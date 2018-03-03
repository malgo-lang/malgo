#include <stdio.h>
#include <gc.h>
#include <string.h>

typedef struct {} Unit;

Unit unit = {};

Unit print(char* str, char* unused) {
  printf("%s", str);
  return unit;
}

Unit println(char* str, char* unused) {
  printf("%s\n", str);
  return unit;
}

Unit print_int(int n, char* unused) {
  printf("%d", n);
  return unit;
}

Unit print_float(double d, char* unused) {
  printf("%f", d);
  return unit;
}

Unit flush(Unit unused, char* unused1) {
  fflush(stdout);
  return unit;
}

char getChar(Unit unused, char* unused1) {
  return fgetc(stdin);
}

int ord(char c, char* unused) {
  return c;
}

char chr(int i, char* unused) {
  return i;
}

int size(char* str, char* unused) {
  return strlen(str);
}

char* substring(char* str, int i1, int i2, char* unused) {
  char* str1 = (char*) GC_malloc(sizeof(char) * (i2 - i1) + 1);
  strncpy(str1, str + i1, i2 - i1);
  return str1;
}

char* concat(char* str1, char* str2, char* unused) {
  char* str = (char*) GC_malloc(strlen(str1) + strlen(str2));
  strncpy(str, str1, strlen(str1));
  strncat(str, str2, strlen(str2));
  return str;
}

Unit newline(Unit unused, char* unused1) {
  printf("\n");
  return unit;
}
