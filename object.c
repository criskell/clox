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

ObjClosure* newClosure(ObjFunction* function) {
  ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);

  for (int i = 0; i < function->upvalueCount; i++) {
    upvalues[i] = NULL;
  }

  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;
  return closure;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;
  function->upvalueCount = 0;
  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}

ObjNative* newNative(NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
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

ObjUpvalue* newUpvalue(Value* slot) {
  ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
  upvalue->location = slot;
  upvalue->next = NULL;
  upvalue->closed = NIL_VAL;
  return upvalue;
}

static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }

  printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_NATIVE:
      printf("<native fn>");
      break;
    
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;

    case OBJ_CLOSURE:
      printFunction(AS_CLOSURE(value)->function);
      break;

    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;

    case OBJ_UPVALUE:
      printf("upvalue");
      break;
  }
}