#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj* next;
};

struct ObjString {
  // Implements struct inheritance / type prunning.
  Obj obj;
  int length;

  uint32_t hash;
  
  // C99 flexible array member syntax
  char chars[];
};

// Take ownership of the string.
ObjString* makeString(int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);
uint32_t hashString(const char* key, int length);
void internString(ObjString* key);

// Instead of a macro, we put this is a inline function to prevent evaluating value twice.
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif