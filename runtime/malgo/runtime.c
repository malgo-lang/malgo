#include <gc.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Unit
typedef struct
{
  uint8_t tag;
  struct
  {
  } payload;
} MalgoUnit;

const MalgoUnit malgo_unit = {0, {}};

// Cast
void *malgo_unsafe_cast(void *x) { return x; }

// Panic
void *malgo_panic(char *message)
{
  fprintf(stderr, "%s", message);
  exit(1);
  return NULL;
}

// Arithmetic operators
int32_t malgo_add_int32_t(int32_t x, int32_t y) { return x + y; }
int32_t malgo_sub_int32_t(int32_t x, int32_t y) { return x - y; }
int32_t malgo_mul_int32_t(int32_t x, int32_t y) { return x * y; }
int32_t malgo_div_int32_t(int32_t x, int32_t y) { return x / y; }
int64_t malgo_add_int64_t(int64_t x, int64_t y) { return x + y; }
int64_t malgo_sub_int64_t(int64_t x, int64_t y) { return x - y; }
int64_t malgo_mul_int64_t(int64_t x, int64_t y) { return x * y; }
int64_t malgo_div_int64_t(int64_t x, int64_t y) { return x / y; }
float malgo_add_float(float x, float y) { return x + y; }
float malgo_sub_float(float x, float y) { return x - y; }
float malgo_mul_float(float x, float y) { return x * y; }
float malgo_div_float(float x, float y) { return x / y; }
double malgo_add_double(double x, double y) { return x + y; }
double malgo_sub_double(double x, double y) { return x - y; }
double malgo_mul_double(double x, double y) { return x * y; }
double malgo_div_double(double x, double y) { return x / y; }

// Comparison operators
int32_t malgo_eq_int32_t(int32_t x, int32_t y) { return x == y; }
int32_t malgo_ne_int32_t(int32_t x, int32_t y) { return x != y; }
int32_t malgo_lt_int32_t(int32_t x, int32_t y) { return x < y; }
int32_t malgo_gt_int32_t(int32_t x, int32_t y) { return x > y; }
int32_t malgo_le_int32_t(int32_t x, int32_t y) { return x <= y; }
int32_t malgo_ge_int32_t(int32_t x, int32_t y) { return x >= y; }
int32_t malgo_eq_int64_t(int64_t x, int64_t y) { return x == y; }
int32_t malgo_ne_int64_t(int64_t x, int64_t y) { return x != y; }
int32_t malgo_lt_int64_t(int64_t x, int64_t y) { return x < y; }
int32_t malgo_gt_int64_t(int64_t x, int64_t y) { return x > y; }
int32_t malgo_le_int64_t(int64_t x, int64_t y) { return x <= y; }
int32_t malgo_ge_int64_t(int64_t x, int64_t y) { return x >= y; }
int32_t malgo_eq_float(float x, float y) { return x == y; }
int32_t malgo_ne_float(float x, float y) { return x != y; }
int32_t malgo_lt_float(float x, float y) { return x < y; }
int32_t malgo_gt_float(float x, float y) { return x > y; }
int32_t malgo_le_float(float x, float y) { return x <= y; }
int32_t malgo_ge_float(float x, float y) { return x >= y; }
int32_t malgo_eq_double(double x, double y) { return x == y; }
int32_t malgo_ne_double(double x, double y) { return x != y; }
int32_t malgo_lt_double(double x, double y) { return x < y; }
int32_t malgo_gt_double(double x, double y) { return x > y; }
int32_t malgo_le_double(double x, double y) { return x <= y; }
int32_t malgo_ge_double(double x, double y) { return x >= y; }
int32_t malgo_eq_char(char x, char y) { return x == y; }
int32_t malgo_ne_char(char x, char y) { return x != y; }
int32_t malgo_lt_char(char x, char y) { return x < y; }
int32_t malgo_gt_char(char x, char y) { return x > y; }
int32_t malgo_le_char(char x, char y) { return x <= y; }
int32_t malgo_ge_char(char x, char y) { return x >= y; }

int32_t malgo_eq_string(char *x, char *y) { return strcmp(x, y) == 0; }
int32_t malgo_ne_string(char *x, char *y) { return strcmp(x, y) != 0; }
int32_t malgo_lt_string(char *x, char *y) { return strcmp(x, y) < 0; }
int32_t malgo_gt_string(char *x, char *y) { return strcmp(x, y) > 0; }
int32_t malgo_le_string(char *x, char *y) { return strcmp(x, y) <= 0; }
int32_t malgo_ge_string(char *x, char *y) { return strcmp(x, y) >= 0; }

// char operators

int32_t malgo_char_ord(char c)
{
  return (int32_t)c;
}

int32_t malgo_is_digit(char c)
{
  return '0' <= c && c <= '9';
}

int32_t malgo_is_lower(char c)
{
  return 'a' <= c && c <= 'z';
}

int32_t malgo_is_upper(char c)
{
  return 'A' <= c && c <= 'Z';
}

int32_t malgo_is_alphanum(char c)
{
  return malgo_is_digit(c) || malgo_is_lower(c) || malgo_is_upper(c);
}

// String operators
char malgo_string_at(int64_t i, char *s) { return s[i]; }

char *malgo_string_cons(char c, char *str)
{
  char *new = GC_MALLOC(sizeof(char) * (1 + strlen(str) + 1));
  new[0] = c;
  new[1] = '\0';
  strcat(new, str);
  return new;
}

char *malgo_string_append(char *s1, char *s2)
{
  char *new = GC_MALLOC(sizeof(char) * (strlen(s1) + strlen(s2) + 1));
  strcpy(new, s1);
  strcat(new, s2);
  return new;
}

int64_t malgo_string_length(char *s) { return strlen(s); }

char *malgo_substring(char *str, int64_t start, int64_t end)
{
  char *new = GC_MALLOC(sizeof(char) * (end - start + 1));
  memcpy(new, &str[start], end - start);
  new[end - start] = '\0';
  return new;
}

struct StringBuilder
{
  char *buf;
  size_t capacity;
  size_t length;
};

struct StringBuilder *new_sb(void)
{
  struct StringBuilder *sb = GC_MALLOC(sizeof(struct StringBuilder));
  sb->buf = GC_MALLOC(8 * sizeof(char));
  sb->capacity = 8;
  sb->length = 0;
  return sb;
}

void sb_putc(struct StringBuilder *sb, char c)
{
  while (sb->length >= sb->capacity)
  {
    sb->capacity += 8;
    sb->buf = GC_REALLOC(sb->buf, sizeof(char) * sb->capacity);
  }
  sb->buf[sb->length] = c;
  sb->length++;
}

void sb_puts(struct StringBuilder *sb, char *str)
{
  for (size_t i = 0; i < strlen(str); i++)
  {
    sb_putc(sb, str[i]);
  }
}

void sb_destory(struct StringBuilder *sb)
{
  GC_FREE(sb);
}

char *sb_run(struct StringBuilder *sb)
{
  sb_putc(sb, '\0');
  char *ret = sb->buf;
  sb_destory(sb);
  return ret;
}

char *malgo_int32_t_to_string(int32_t x)
{
  size_t size = 4;
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size)
  {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%" PRId32, x);
  }
  return new;
}
char *malgo_int64_t_to_string(int64_t x)
{
  size_t size = 4;
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size)
  {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%" PRId64, x);
  }
  return new;
}
char *malgo_float_to_string(float x)
{
  size_t size = 4;
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size)
  {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%"
                                 "f",
                      x);
  }
  return new;
}
char *malgo_double_to_string(double x)
{
  size_t size = 4;
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size)
  {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%"
                                 "lf",
                      x);
  }
  return new;
}
char *malgo_char_to_string(char x)
{
  size_t size = 4;
  char *new = GC_MALLOC(sizeof(char) * size);
  int writed = -1;
  while (writed < 0 || writed >= size)
  {
    size++;
    new = GC_REALLOC(new, sizeof(char) * size);
    writed = snprintf(new, size, "%"
                                 "c",
                      x);
  }
  return new;
}

// IO functions
const MalgoUnit *malgo_exit_failure(MalgoUnit *__attribute__((unused)) unused)
{
  exit(1);
}

const MalgoUnit *malgo_newline(MalgoUnit *__attribute__((unused)) unused)
{
  puts("");
  return &malgo_unit;
}

const MalgoUnit *malgo_print_char(char x)
{
  printf("%c", x);
  return &malgo_unit;
}

const MalgoUnit *malgo_print_string(char *x)
{
  printf("%s", x);
  return &malgo_unit;
}

const MalgoUnit *malgo_flush(MalgoUnit *__attribute__((unused)) unused)
{
  fflush(stdout);
  return &malgo_unit;
}

char malgo_get_char(MalgoUnit *__attribute__((unused)) unused)
{
  return getchar();
}

char *malgo_get_contents(MalgoUnit *__attribute__((unused)) unused)
{
  struct StringBuilder *sb = new_sb();

  char c;
  while ((c = fgetc(stdin)) != EOF)
  {
    sb_putc(sb, c);
  }
  return sb_run(sb);
}

// Vector
void **malgo_new_vector(int64_t len, void *init)
{
  void **ptr = GC_MALLOC(sizeof(void *) * len);
  for (int64_t i = 0; i < len; i++)
  {
    ptr[i] = init;
  }
  return ptr;
}

void *malgo_read_vector(int64_t index, void **ptr) { return ptr[index]; }

const MalgoUnit *malgo_write_vector(int64_t index, void **ptr, void *val)
{
  ptr[index] = val;
  return &malgo_unit;
}

// HashTable
#define HASH_TABLE_BUCKET_SIZE 16

struct bucket
{
  char *key;
  void *value;
  struct bucket *next;
};

// Hash table using chaining
struct hash_table
{
  struct bucket buckets[HASH_TABLE_BUCKET_SIZE];
  size_t size;
};

// `hash` compute hash value for a string
// hash(x) must be included in [0, HASH_TABLE_BUCKETS)
static int hash(char *x)
{
  int h = 0;
  for (size_t i = 0; i < strlen(x); i++)
  {
    h = h * 31 + x[i];
  }
  return h % HASH_TABLE_BUCKET_SIZE;
}

// `malgo_hash_table_new` create a new hash table.
struct hash_table *malgo_hash_table_new()
{
  struct hash_table *ht = GC_MALLOC(sizeof(struct hash_table));
  for (size_t i = 0; i < HASH_TABLE_BUCKET_SIZE; i++)
  {
    ht->buckets[i].next = NULL;
  }
  ht->size = 0;
  return ht;
}

// `malgo_hash_table_insert` insert new key-value pair to the hash table.
void malgo_hash_table_insert(struct hash_table *ht, char *key, void *value)
{
  int h = hash(key);
  struct bucket *b = &ht->buckets[h];
  while (b->next != NULL)
  {
    // update value if key already exists
    if (strcmp(b->key, key) == 0)
    {
      b->value = value;
      return;
    }
    b = b->next;
  }
  // b == NULL
  // insert new key-value pair
  b->key = key;
  b->value = value;
  b->next = GC_MALLOC(sizeof(struct bucket));
  ht->size++;
}

// `malgo_hash_table_get` get a value by a key from the hash table.
void *malgo_hash_table_get(struct hash_table *ht, char *key)
{
  int h = hash(key);
  struct bucket *b = &ht->buckets[h];
  while (b != NULL)
  {
    if (strcmp(b->key, key) == 0)
    {
      return b->value;
    }
    b = b->next;
  }
  return NULL;
}