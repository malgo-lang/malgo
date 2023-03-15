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

// String operators
// char *malgo_string_cons(char c, char *str)
// {
//   char *new = GC_MALLOC(sizeof(char) * (1 + strlen(str) + 1));
//   new[0] = c;
//   new[1] = '\0';
//   strcat(new, str);
//   return new;
// }

// char *malgo_string_append(char *s1, char *s2)
// {
//   char *new = GC_MALLOC(sizeof(char) * (strlen(s1) + strlen(s2) + 1));
//   strcpy(new, s1);
//   strcat(new, s2);
//   return new;
// }

// int64_t malgo_string_length(char *s) { return strlen(s); }

// char *malgo_substring(char *str, int64_t start, int64_t end)
// {
//   char *new = GC_MALLOC(sizeof(char) * (end - start + 1));
//   memcpy(new, &str[start], end - start);
//   new[end - start] = '\0';
//   return new;
// }

struct StringBuilder
{
  char *buf;
  size_t capacity;
  size_t length;
};

struct StringBuilder *new_sb(void)
{
  struct StringBuilder *sb = malloc(sizeof(struct StringBuilder));
  sb->buf = malloc(8 * sizeof(char));
  sb->capacity = 8;
  sb->length = 0;
  return sb;
}

void sb_putc(struct StringBuilder *sb, char c)
{
  while (sb->length >= sb->capacity)
  {
    sb->capacity += 8;
    sb->buf = realloc(sb->buf, sizeof(char) * sb->capacity);
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
  free(sb);
}

char *sb_run(struct StringBuilder *sb)
{
  sb_putc(sb, '\0');
  char *ret = sb->buf;
  sb_destory(sb);
  return ret;
}

// IO functions
// const MalgoUnit *malgo_newline(MalgoUnit *__attribute__((unused)) unused)
// {
//   puts("");
//   return &malgo_unit;
// }

// const MalgoUnit *malgo_print_char(char x)
// {
//   printf("%c", x);
//   return &malgo_unit;
// }

// const MalgoUnit *malgo_print_string(char *x)
// {
//   printf("%s", x);
//   return &malgo_unit;
// }

// const MalgoUnit *malgo_flush(MalgoUnit *__attribute__((unused)) unused)
// {
//   fflush(stdout);
//   return &malgo_unit;
// }

// char malgo_get_char(MalgoUnit *__attribute__((unused)) unused)
// {
//   return getchar();
// }

// char *malgo_get_contents(MalgoUnit *__attribute__((unused)) unused)
// {
//   struct StringBuilder *sb = new_sb();
// 
//   int c;
//   while ((c = fgetc(stdin)) != EOF)
//   {
//     sb_putc(sb, c);
//   }
//   return sb_run(sb);
// }

// Vector
// void **malgo_new_vector(int64_t len, void *init)
// {
//   void **ptr = GC_MALLOC(sizeof(void *) * len);
//   for (int64_t i = 0; i < len; i++)
//   {
//     ptr[i] = init;
//   }
//   return ptr;
// }

// void *malgo_read_vector(int64_t index, void **ptr) { return ptr[index]; }

// const MalgoUnit *malgo_write_vector(int64_t index, void **ptr, void *val)
// {
//   ptr[index] = val;
//   return &malgo_unit;
// }

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