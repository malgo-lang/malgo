#include <stdio.h>

typedef int* Unit;

int *unit;

Unit println(char* str) {
  puts(str);
  return unit;
}

Unit print(char* str) {
  printf("%s", str);
  return unit;
}

Unit print_int(int n) {
  printf("%d", n);
  return unit;
}
