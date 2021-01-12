#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int32_t add_i32(int32_t x, int32_t y) {
  return x + y;
}

int64_t add_i64(int64_t x, int64_t y) {
  return x + y;
}

float add_float(float x, float y) {
  return x + y;
}

double add_double(double x, double y) {
  return x + y;
}

int32_t sub_i32(int32_t x, int32_t y) {
  return x - y;
}

int64_t sub_i64(int64_t x, int64_t y) {
  return x - y;
}

float sub_float(float x, float y) {
  return x - y;
}

double sub_double(double x, double y) {
  return x - y;
}

int32_t mul_i32(int32_t x, int32_t y) {
  return x * y;
}

int64_t mul_i64(int64_t x, int64_t y) {
  return x * y;
}

float mul_float(float x, float y) {
  return x * y;
}

double mul_double(double x, double y) {
  return x * y;
}

int32_t div_i32(int32_t x, int32_t y) {
  return x / y;
}

int64_t div_i64(int64_t x, int64_t y) {
  return x / y;
}

float div_float(float x, float y) {
  return x / y;
}

double div_double(double x, double y) {
  return x / y;
}

typedef struct {
  uint8_t tag;
  struct {} payload;
} Unit;

const Unit unit = {0, {}};

const Unit* print_int(int64_t i) {
  printf("%" PRId64, i);
  return &unit;
}

const Unit* print_int32(int32_t i) {
  printf("%" PRId32, i);
  return &unit;
}

const Unit* print_int64(int64_t i) {
  printf("%" PRId64, i);
  return &unit;
}

const Unit* print_float(float f) {
  printf("%f", f);
  return &unit;
}

const Unit* print_double(double d) {
  printf("%lf", d);
  return &unit;
}

const Unit* newline(Unit* __attribute__((unused)) unused) {
  puts("");
  return &unit;
}

const Unit* print_string(char* x) {
  printf("%s", x);
  return &unit;
}