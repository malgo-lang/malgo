#include <stdio.h>
#include <gc.h>
#include <string.h>

typedef struct {} Unit;

Unit unit = {};

Unit print(char* str) {
  printf("%s", str);
  return unit;
}

Unit println(char* str) {
  printf("%s\n", str);
  return unit;
}

Unit print_int(int n) {
  printf("%d", n);
  return unit;
}

Unit print_float(double d) {
  printf("%f", d);
  return unit;
}

Unit print_bool(int b) {
  if (b) {
    printf("true");
  } else {
    printf("false");
  }
  return unit;
}

Unit flush(Unit unused) {
  fflush(stdout);
  return unit;
}

char getChar(Unit unused) {
  return fgetc(stdin);
}

int ord(char c) {
  return c;
}

char chr(int i) {
  return i;
}

int size(char* str) {
  return strlen(str);
}

char* substring(char* str, int i1, int i2) {
  char* str1 = (char*) GC_malloc(sizeof(char) * (i2 - i1) + 1);
  strncpy(str1, str + i1, i2 - i1);
  return str1;
}

char* concat(char* str1, char* str2) {
  char* str = (char*) GC_malloc(strlen(str1) + strlen(str2));
  strncpy(str, str1, strlen(str1));
  strncat(str, str2, strlen(str2));
  return str;
}

Unit newline(Unit unused) {
  printf("\n");
  return unit;
}
