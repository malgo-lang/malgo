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