#include <gc.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int64_t tag;
  struct {} payload;
} Unit;

Unit* new_Unit(void) {
  Unit* val = GC_MALLOC(sizeof(Unit));
  val->tag = 0;
  return val;
}

Unit* print_int(int64_t i) {
    printf("%lld", i);
    return new_Unit();
}

Unit* newline(Unit* unused) {
  puts("");
  return new_Unit();
}

Unit* print_string(char* x) {
  printf("%s", x);
  return new_Unit();
}