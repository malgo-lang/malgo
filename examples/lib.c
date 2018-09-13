#include <stdio.h>
#include <gc.h>
#include <string.h>
#include <stdbool.h>

typedef struct {} Unit;

void init_gc(void) {
  GC_INIT();
}

void* malloc_gc(size_t sz) {
  return GC_MALLOC(sz);
}

bool le_i64(long x, long y) {
  return x <= y;
}

bool ge_i64(long x, long y) {
  return x >= y;
}

long sub_i64(long x, long y) {
  return x - y;
}

long add_i64(long x, long y) {
  return x + y;
}

bool eq_i64(long x, long y) {
  return x == y;
}

bool neq_i64(long x, long y) {
  return x != y;
}

Unit unit = {};

Unit print(char* str) {
  printf("%s", str);
  return unit;
}

Unit println(char* str) {
  printf("%s\n", str);
  return unit;
}

Unit print_int(long n) {
  printf("%ld", n);
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

long ord(char c) {
  return c;
}

char chr(long i) {
  return i;
}

long size(char* str) {
  return strlen(str);
}

char* substring(char* str, long i1, long i2) {
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
