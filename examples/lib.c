#include <stdio.h>
#include <unistd.h>
#include <gc.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>
#include <stdlib.h>

/* Unit type */
typedef struct {} Unit;

/* GC */
void init_gc(void) {
  GC_INIT();
}

void* malloc_gc(size_t sz) {
  return GC_MALLOC(sz);
}

/* operators */

long add_i64(long x, long y) { return x + y; }
long sub_i64(long x, long y) { return x - y; }
long mul_i64(long x, long y) { return x * y; }
long div_i64(long x, long y) { return x / y; }
long mod_i64(long x, long y) { return x % y; }
double add_double(double x, double y) { return x + y; }
double sub_double(double x, double y) { return x - y; }
double mul_double(double x, double y) { return x * y; }
double div_double(double x, double y) { return x / y; }
bool eq_i64(long x, long y) { return x == y; }
bool neq_i64(long x, long y) { return x != y; }
bool lt_i64(long x, long y) { return x < y; }
bool gt_i64(long x, long y) { return x > y; }
bool le_i64(long x, long y) { return x <= y; }
bool ge_i64(long x, long y) { return x >= y; }
bool eq_i8(char x, char y) { return x == y; }
bool neq_i8(char x, char y) { return x != y; }
bool lt_i8(char x, char y) { return x < y; }
bool gt_i8(char x, char y) { return x > y; }
bool le_i8(char x, char y) { return x <= y; }
bool ge_i8(char x, char y) { return x >= y; }
bool eq_i1(bool x, bool y) { return x == y; }
bool neq_i1(bool x, bool y) { return x != y; }
bool lt_i1(bool x, bool y) { return x < y; }
bool gt_i1(bool x, bool y) { return x > y; }
bool le_i1(bool x, bool y) { return x <= y; }
bool ge_i1(bool x, bool y) { return x >= y; }
bool eq_double(double x, double y) { return x == y; }
bool neq_double(double x, double y) { return x != y; }
bool lt_double(double x, double y) { return x < y; }
bool gt_double(double x, double y) { return x > y; }
bool le_double(double x, double y) { return x <= y; }
bool ge_double(double x, double y) { return x >= y; }

bool or(bool x, bool y) {
  return x || y;
}

bool and(bool x, bool y) {
  return x && y;
}

bool not(bool x) { return !x; }

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

Unit print_char(char c) {
  printf("%c", c);
  return unit;
}

Unit gen_seed() {
  srand((unsigned) time(NULL));
  return unit;
}

bool rand_bool() {
  return (bool)(rand() % 2);
}

Unit flush() {
  fflush(stdout);
  return unit;
}

char getChar() {
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

Unit newline() {
  printf("\n");
  return unit;
}

typedef struct {
  bool* raw;
  long size;
} bool_array;

bool_array* copy_bool_array(bool_array* arr) {
  bool* cpy = (bool*) GC_malloc(sizeof(bool) * arr->size);
  bool_array* cpy_arr = (bool_array*) GC_malloc(sizeof(bool_array));
  for (int i = 0; i < arr->size; i++) {
    cpy[i] = arr->raw[i];
  }
  cpy_arr->raw = cpy;
  cpy_arr->size = arr->size;
  return cpy_arr;
}

Unit malgo_sleep(long sec) {
  sleep(sec);
  return unit;
}

Unit pulsar(bool_array* cells) {
  cells->raw[11 * 50 + 23] = true;
  cells->raw[12 * 50 + 23] = true;
  cells->raw[13 * 50 + 23] = true;

  cells->raw[12 * 50 + 25] = true;

  cells->raw[11 * 50 + 27] = true;
  cells->raw[12 * 50 + 27] = true;
  cells->raw[13 * 50 + 27] = true;

  return unit;
}
