#include <stdio.h>
#include <stdint.h>
#include <gc.h>

typedef struct {
  int64_t tag;
  struct {} payload;
} Unit;

Unit* new_Unit(void) {
  Unit* val = GC_MALLOC(sizeof(Unit));
  val->tag = 0;
  return val;
}

typedef struct {
  int64_t tag;
  int64_t payload;
} Int;

Int* new_Int(int64_t x) {
  Int* val = GC_MALLOC(sizeof(Int));
  val->tag = 0;
  val->payload = x;
  return val;
}

Unit* print_int(Int* x) {
  printf("%lld", x->payload);
  return new_Unit();
}

Unit* newline(Unit* unused) {
  puts("");
  return new_Unit();
}