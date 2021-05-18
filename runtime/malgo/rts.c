#include <gc.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Arithmetic operators
int32_t add_i32(int32_t x, int32_t y) { return x + y; }

int64_t add_i64(int64_t x, int64_t y) { return x + y; }

float add_float(float x, float y) { return x + y; }

double add_double(double x, double y) { return x + y; }

int32_t sub_i32(int32_t x, int32_t y) { return x - y; }

int64_t sub_i64(int64_t x, int64_t y) { return x - y; }

float sub_float(float x, float y) { return x - y; }

double sub_double(double x, double y) { return x - y; }

int32_t mul_i32(int32_t x, int32_t y) { return x * y; }

int64_t mul_i64(int64_t x, int64_t y) { return x * y; }

float mul_float(float x, float y) { return x * y; }

double mul_double(double x, double y) { return x * y; }

int32_t div_i32(int32_t x, int32_t y) { return x / y; }

int64_t div_i64(int64_t x, int64_t y) { return x / y; }

float div_float(float x, float y) { return x / y; }

double div_double(double x, double y) { return x / y; }

// Comparison operators
int32_t le_int32(int32_t x, int32_t y) { return x <= y; }

int32_t le_int64(int64_t x, int64_t y) { return x <= y; }

int32_t le_float(float x, float y) { return x <= y; }

int32_t le_double(double x, double y) { return x <= y; }

int32_t ge_int32(int32_t x, int32_t y) { return x >= y; }

int32_t ge_int64(int64_t x, int64_t y) { return x >= y; }

int32_t ge_float(float x, float y) { return x >= y; }

int32_t ge_double(double x, double y) { return x >= y; }

// String operators
char malgo_string_at(int64_t i, char *s) { return s[i]; }

char *malgo_string_append(char *s1, char *s2) {
  char *new = GC_MALLOC(sizeof(char) * strlen(s1) * strlen(s2) + 1);
  strcpy(new, s1);
  strcat(new, s2);
  return new;
}

char *append_string(char *s1, char *s2) {
  char *new = GC_MALLOC(sizeof(char) * strlen(s1) * strlen(s2) + 1);
  strcpy(new, s1);
  strcat(new, s2);
  return new;
}

char *show_double(double d) {
  size_t size = 4;
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size) {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%lf", d);
  }
  return new;
}

typedef struct {
  uint8_t tag;
  struct {
  } payload;
} Unit;

const Unit unit = {0, {}};

const Unit *print_int(int64_t i) {
  printf("%" PRId64, i);
  return &unit;
}

const Unit *print_int32(int32_t i) {
  printf("%" PRId32, i);
  return &unit;
}

const Unit *print_int64(int64_t i) {
  printf("%" PRId64, i);
  return &unit;
}

const Unit *print_float(float f) {
  printf("%f", f);
  return &unit;
}

const Unit *print_double(double d) {
  printf("%lf", d);
  return &unit;
}

const Unit *newline(Unit *__attribute__((unused)) unused) {
  puts("");
  return &unit;
}

const Unit *print_char(char x) {
  printf("%c", x);
  return &unit;
}

const Unit *print_string(char *x) {
  printf("%s", x);
  return &unit;
}

void *unsafe_cast(void *x) { return x; }

void **new_vector(int64_t len, void *init) {
  void **ptr = GC_MALLOC(sizeof(void *) * len);
  for (int64_t i = 0; i < len; i++) {
    ptr[i] = init;
  }
  return ptr;
}

void *read_vector(int64_t index, void **ptr) { return ptr[index]; }

const Unit *write_vector(int64_t index, void **ptr, void *val) {
  ptr[index] = val;
  return &unit;
}