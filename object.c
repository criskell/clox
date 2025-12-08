#include <stdio.h>
#include <string.h>

#include "object.h"
#include "value.h"
#include "mem.h"
#include "vm.h"
#include "table.h"

#define ALLOCATE_OBJ(type, objectType) \
  (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;
  object->next = vm.objects;
  vm.objects = object;
  return object;
}

uint32_t hashString(const char* key, int length) {
  // It implements a short hash function called FNV-1a.
  // A good hash function for a hash table should have certain properties:
  // - Deterministic: Always create a hash for the same number.
  // - Uniform: The outputs should be evenly distributed, dispersing throughout the numerical range
  //            to avoid collisions and clustering.
  // - Fast: Because each operation on the table requires a hash.
  
  // We start with an initial hash, which is a constant with some mathematical properties.
  uint32_t hash = 2166136261u;

  // We iterated through every byte.
  for (int i = 0; i < length; i++) {
    // Mix the byte into the hash value.
    hash ^= (uint8_t)key[i];

    // It scrambles the resulting bits around a value.
    hash *= 16777619;
  }
  
  return hash;
}

void internString(ObjString* key) {
  tableSet(&vm.strings, key, NIL_VAL);
}

// Creates a string in the heap using the flexible array member concept in structs.
ObjString* makeString(int length) {
  ObjString* string = (ObjString*)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;
  
  return string;
}

ObjString* copyString(const char* chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

  if (interned != NULL) {
    return interned;
  }

  ObjString* string = makeString(length);
  
  string->hash = hash;
  internString(string);

  memcpy(string->chars, chars, length);
  string->chars[length] = '\0';

  return string;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
  }
}