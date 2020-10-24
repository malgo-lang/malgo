#include <gc.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

// type defintions

// :: {}
typedef struct
{
  int64_t tag;
  struct
  {
  } payload;
} Unit;

Unit *new_Unit(void)
{
  Unit *val = GC_MALLOC(sizeof(Unit));
  val->tag = 0;
  return val;
}

// :: Int
typedef struct
{
  int64_t tag;
  int64_t payload;
} Int;

// :: Float
typedef struct
{
  int64_t tag;
  double payload;
} Float;

// :: Bool
typedef struct
{
  int64_t tag;
  int8_t payload;
} Bool;

Bool *new_Bool(bool x)
{
  Bool *val = GC_MALLOC(sizeof(Bool));
  val->tag = 0;
  val->payload = x;
  return val;
}

// :: String
typedef struct
{
  int64_t tag;
  char *payload;
} String;

// :: Char
typedef struct
{
  int64_t tag;
  char payload;
} Char;

// print
Unit *print_int(Int *x)
{
  printf("%lld", x->payload);
  return new_Unit();
}

Unit *print_float(Float *x)
{
  printf("%lf", x->payload);
  return new_Unit();
}

Unit *print_bool(Bool *x)
{
  if (x->payload)
  {
    printf("true");
  }
  else
  {
    printf("false");
  }
  return new_Unit();
}

Unit *print_char(Char *c)
{
  printf("%c", c->payload);
  return new_Unit();
}

Unit *print(String *x)
{
  printf("%s", x->payload);
  return new_Unit();
}

Unit *println(String *x)
{
  printf("%s\n", x->payload);
  return new_Unit();
}

Unit *newline(Unit *unused)
{
  puts("");
  return new_Unit();
}

// string operations
String *concat(String *s1, String *s2)
{
  String *s3 = GC_MALLOC(sizeof(String));
  s3->tag = 0;
  s3->payload = GC_MALLOC(sizeof(char) * (strlen(s1->payload) + strlen(s2->payload) + 1));
  strcat(s3->payload, s1->payload);
  strcat(s3->payload, s2->payload);
  return s3;
}

String *substring(String* s, Int* from, Int* until) {
  String* result = GC_MALLOC(sizeof(String));
  result->tag = 0;
  result->payload = GC_MALLOC(sizeof(char) * (until->payload - from->payload + 1));
  strncpy(result->payload, s->payload + from->payload, until->payload - from->payload);
  return result;
}

// sleep
Unit *malgo_sleep(Int *sec)
{
  sleep(sec->payload);
  return new_Unit();
}

// random
Unit *gen_seed(Unit *unused)
{
  srand((unsigned)time(NULL));
  return new_Unit();
}

Bool *rand_bool(Unit *unused)
{
  return new_Bool((bool)(rand() % 2));
}