#include <stdlib.h>
#include <string.h>

#include "mem.h"
#include "object.h"
#include "table.h"
#include "value.h"

// We reallocate when the array is at least 75% full.
#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
  // What is the bucket index where we can place the entry?
  uint32_t index = key->hash % capacity;

  Entry* tombstone = NULL;

  // Controlling the load factor prevents this from falling into an infinite loop, where we always collide with buckets.
  // There will always be empty buckets.
  for (;;) {
    Entry* entry = &entries[index];

    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // This allows the tombstone to be treated as a valid bucket during writing.
        // In cases where we have insertions and deletions, the number of tombstones increases slightly and then stabilizes.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone.
        if (tombstone == NULL) {
          tombstone = entry;
        }
      }
    } else if (entry->key == key) {
      // Found the key.
      return entry;
    }

    // Handle collision here. Continue probing.

    // Implement linear probing.
    index = (index + 1) % capacity;
  }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
  // Prevent null deref in bucket array and optimize this path
  if (table->count == 0) return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);

  // Does not exist
  if (entry->key == NULL) return false;

  *value = entry->value;
  return value;
}

static void adjustCapacity(Table* table, int capacity) {
  Entry* entries = ALLOCATE(Entry, capacity);

  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  // Don't count tombstones.
  table->count = 0;

  // Resizing a hash table causes the hash indices to change, since the size changes.
  // The index is calculated using the hash modulo the array size.
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];

    if (entry->key == NULL) continue;

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableSet(Table* table, ObjString* key, Value value) {
  // load factor = count / capacity
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }
  
  Entry* entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = entry->key == NULL;

  // The count is not just the number of filled entries, but also the number of tombstones.
  // This prevents infinite loops requiring array growth during multiple deletions in the hash table.
  // For the load factor, we consider tombstones as multiple full buckets.
  // It only increments the number of entries when we are not inserting into a tombstone, because that has already been accounted for.
  if (isNewKey && IS_NIL(entry->value)) {
    table->count++;
  }

  // Tombstones avoids the high cost of tidying up the probing chain, allowing for efficient insertion afterwards,
  // and also avoids lazy removal at a cost in searches and wasted memory.

  entry->key = key;
  entry->value = value;

  return isNewKey;
}

void tableAddAll(Table *from, Table *to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry* entry = &from->entries[i];

    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
  // This specialized function searches for keys by value.

  if (table->count == 0) return NULL;

  uint32_t index = hash % table->capacity;

  for (;;) {
    Entry* entry = &table->entries[index];

    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) return NULL;
    } else if (entry->key->length == length &&
      entry->key->hash == hash &&
      memcmp(entry->key->chars, chars, length) == 0) {
        return entry->key;
    }

    index = (index + 1) % table->capacity;
  }
}

bool tableDelete(Table* table, ObjString* key) {
  if (table->count == 0) return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);

  // Entry does not exists.
  if (entry->key == NULL) return false;

  // Place a tombstone at the entry to prevent broken probe sequences during linear probing.
  entry->key = NULL;
  entry->value = BOOL_VAL(true);

  return true;
}