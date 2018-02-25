#include <stdio.h>
#include <gc.h>
#include <string.h>

typedef struct {} Unit;

Unit unit = {};

Unit print(char* str, char* unused) {
  printf("%s", str);
  return unit;
}

Unit println(char* str, char* unused) {
  printf("%s\n", str);
  return unit;
}

Unit print_int(int n, char* unused) {
  printf("%d", n);
  return unit;
}

Unit print_float(double d, char* unused) {
  printf("%f", d);
  return unit;
}

Unit flush(Unit unused, char* unused1) {
  fflush(stdout);
  return unit;
}

char getChar(Unit unused, char* unused1) {
  return fgetc(stdin);
}

int ord(char c, char* unused) {
  return c;
}

char chr(int i, char* unused) {
  return i;
}

int size(char* str, char* unused) {
  return strlen(str);
}

typedef struct {
  char* str;
  int i1;
  int i2;
} Tuple_String_Int_Int;

char* substring(Tuple_String_Int_Int* arg, char* unused) {
  char* str = (char*) GC_malloc(sizeof(char) * (arg->i2 - arg->i1) + 1);
  strncpy(str, arg->str + arg->i1, arg->i2 - arg->i1);
  return str;
}

typedef struct {
  char* str1;
  char* str2;
} Tuple_String_String;

char* concat(Tuple_String_String* arg, char* unused) {
  char* str = (char*) GC_malloc(strlen(arg->str1) + strlen(arg->str2));
  strncpy(str, arg->str1, strlen(arg->str1));
  strncat(str, arg->str2, strlen(arg->str2));
  return str;
}

Unit newline(Unit unused, char* unused1) {
  printf("\n");
  return unit;
}
