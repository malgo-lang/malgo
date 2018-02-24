#include <stdio.h>

typedef struct {} Unit;

Unit unit = {};

Unit newline(Unit unused, char* unused1) {
  printf("\n");
  return unit;
}

Unit print_int(int n, char* unused) {
  printf("%d", n);
  return unit;
}

Unit println(char* str, char* unused) {
  printf("%s\n", str);
  return unit;
}
