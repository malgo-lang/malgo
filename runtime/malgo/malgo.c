#include <gc.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Unit
typedef struct {
  uint8_t tag;
  struct {
  } payload;
} MalgoUnit;

const MalgoUnit malgo_unit = {0, {}};

// Cast
void *malgo_unsafe_cast(void *x) { return x; }

// Arithmetic operators
int32_t malgo_add_int32_t(int32_t x, int32_t y) { return x + y; }
int64_t malgo_add_int64_t(int64_t x, int64_t y) { return x + y; }
float malgo_add_float(float x, float y) { return x + y; }
double malgo_add_double(double x, double y) { return x + y; }
int32_t malgo_sub_int32_t(int32_t x, int32_t y) { return x - y; }
int64_t malgo_sub_int64_t(int64_t x, int64_t y) { return x - y; }
float malgo_sub_float(float x, float y) { return x - y; }
double malgo_sub_double(double x, double y) { return x - y; }
int32_t malgo_mul_int32_t(int32_t x, int32_t y) { return x * y; }
int64_t malgo_mul_int64_t(int64_t x, int64_t y) { return x * y; }
float malgo_mul_float(float x, float y) { return x * y; }
double malgo_mul_double(double x, double y) { return x * y; }
int32_t malgo_div_int32_t(int32_t x, int32_t y) { return x / y; }
int64_t malgo_div_int64_t(int64_t x, int64_t y) { return x / y; }
float malgo_div_float(float x, float y) { return x / y; }
double malgo_div_double(double x, double y) { return x / y; }

// Comparison operators
int32_t malgo_eq_int32_t(int32_t x, int32_t y) { return x == y; }
int32_t malgo_eq_int64_t(int64_t x, int64_t y) { return x == y; }
int32_t malgo_eq_float(float x, float y) { return x == y; }
int32_t malgo_eq_double(double x, double y) { return x == y; }
int32_t malgo_ne_int32_t(int32_t x, int32_t y) { return x != y; }
int32_t malgo_ne_int64_t(int64_t x, int64_t y) { return x != y; }
int32_t malgo_ne_float(float x, float y) { return x != y; }
int32_t malgo_ne_double(double x, double y) { return x != y; }
int32_t malgo_lt_int32_t(int32_t x, int32_t y) { return x < y; }
int32_t malgo_lt_int64_t(int64_t x, int64_t y) { return x < y; }
int32_t malgo_lt_float(float x, float y) { return x < y; }
int32_t malgo_lt_double(double x, double y) { return x < y; }
int32_t malgo_gt_int32_t(int32_t x, int32_t y) { return x > y; }
int32_t malgo_gt_int64_t(int64_t x, int64_t y) { return x > y; }
int32_t malgo_gt_float(float x, float y) { return x > y; }
int32_t malgo_gt_double(double x, double y) { return x > y; }
int32_t malgo_le_int32_t(int32_t x, int32_t y) { return x <= y; }
int32_t malgo_le_int64_t(int64_t x, int64_t y) { return x <= y; }
int32_t malgo_le_float(float x, float y) { return x <= y; }
int32_t malgo_le_double(double x, double y) { return x <= y; }
int32_t malgo_ge_int32_t(int32_t x, int32_t y) { return x >= y; }
int32_t malgo_ge_int64_t(int64_t x, int64_t y) { return x >= y; }
int32_t malgo_ge_float(float x, float y) { return x >= y; }
int32_t malgo_ge_double(double x, double y) { return x >= y; }

// String operators
char malgo_string_at(int64_t i, char *s) { return s[i]; }

char *malgo_string_append(char *s1, char *s2) {
  char *new = GC_MALLOC(sizeof(char) * strlen(s1) * strlen(s2) + 1);
  strcpy(new, s1);
  strcat(new, s2);
  return new;
}

char *malgo_int32_t_to_string(int32_t x) {
  size_t size = 4;  
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size) {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%" PRId32, x);
  }
  return new;
}
char *malgo_int64_t_to_string(int64_t x) {
  size_t size = 4;  
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size) {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%" PRId64, x);
  }
  return new;
}
char *malgo_float_to_string(float x) {
  size_t size = 4;  
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size) {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%f", x);
  }
  return new;
}
char *malgo_double_to_string(double x) {
  size_t size = 4;  
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size) {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%lf", x);
  }
  return new;
}

// IO functions
const MalgoUnit *malgo_newline(MalgoUnit *__attribute__((unused)) unused) {
  puts("");
  return &malgo_unit;
}

const MalgoUnit *malgo_print_char(char x) {
  printf("%c", x);
  return &malgo_unit;
}

const MalgoUnit *print_string(char *x) {
  printf("%s", x);
  return &malgo_unit;
}

// Vector
void **malgo_new_vector(int64_t len, void *init) {
  void **ptr = GC_MALLOC(sizeof(void *) * len);
  for (int64_t i = 0; i < len; i++) {
    ptr[i] = init;
  }
  return ptr;
}

void *malgo_read_vector(int64_t index, void **ptr) { return ptr[index]; }

const MalgoUnit *malgo_write_vector(int64_t index, void **ptr, void *val) {
  ptr[index] = val;
  return &malgo_unit;
}
