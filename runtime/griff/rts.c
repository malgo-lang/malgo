#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

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