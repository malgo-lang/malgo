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

bool le_i32(int x, int y) {
  return x <= y;
}

bool ge_i32(int x, int y) {
  return x >= y;
}

int sub_i32(int x, int y) {
  return x - y;
}

int add_i32(int x, int y) {
  return x + y;
}

bool eq_i32(int x, int y) {
  return x == y;
}

bool neq_i32(int x, int y) {
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

Unit print_int(int n) {
  printf("%d", n);
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

int ord(char c) {
  return c;
}

char chr(int i) {
  return i;
}

int size(char* str) {
  return strlen(str);
}

char* substring(char* str, int i1, int i2) {
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

struct int_array {
  int size;
  int* array;
};

struct int_array* int_array_create(int n, int init) {
  struct int_array* a = GC_MALLOC(sizeof(struct int_array));

  int* ptr = GC_MALLOC(sizeof(int) * n);
  for (int i = 0; i < n; i++) {
    ptr[i] = init;
  }

  a->size = n;
  a->array = ptr;

  return a;
}

int int_array_access(struct int_array* array, int index) {
  return array->array[index];
}

struct int_array* int_array_update(struct int_array* a1, int index, int val) {
  struct int_array* a2 = GC_MALLOC(sizeof(struct int_array));
  int* ptr = GC_MALLOC(sizeof(int) * a1->size);
  for (int i = 0; i < a1->size; i++) {
    ptr[i] = a1->array[i];
  }

  ptr[index] = val;

  a2->size = a1->size;
  a2->array = ptr;

  return a2;
}
