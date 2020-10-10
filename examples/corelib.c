#include <gc.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

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

typedef struct {
  int64_t tag;
  int8_t payload;
} Bool;

Bool* new_Bool(bool x) {
  Bool* val = GC_MALLOC(sizeof(Bool));
  val->tag = 0;
  val->payload = x;
  return val;
}

typedef struct {
  int64_t tag;
  char* payload;
} String;

Unit* print_int(Int* x) {
  printf("%lld", x->payload);
  return new_Unit();
}

Unit* print_bool(Bool* x) {
  if (x->payload) {
    printf("true");
  } else {
    printf("false");
  }
  return new_Unit();
}

Unit* println(String* x) {
  printf("%s\n", x->payload);
  return new_Unit();
}

Unit* newline(Unit* unused) {
  puts("");
  return new_Unit();
}

typedef struct {
  int64_t tag;
  struct {
    Bool* fst;
    Bool* snd;
  } payload;
} Tuple2Bool;

Unit* print_tuple2bool(Tuple2Bool* x) {
  print_bool(x->payload.fst);
  print_bool(x->payload.snd);
  return new_Unit();
}

Unit* malgo_sleep(Int* sec) {
  sleep(sec->payload);
  return new_Unit();
}

Unit* gen_seed(Unit* unused) {
  srand((unsigned) time(NULL));
  return new_Unit();
}

typedef struct {
  int64_t tag;
  char payload;
} Char;

Unit* print_char(Char* c) {
  printf("%c", c->payload);
  return new_Unit();
}

Bool* rand_bool(Unit* unused) {
  return new_Bool((bool)(rand() % 2));
}