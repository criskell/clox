#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

// Each type supported by the VM
typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
  VAL_OBJ,
} ValueType;

// This is a tagged union.
//
// 4 bytes + 4 padding = 8 bytes (type)
// 8 bytes (value)
//
// Most CPU architectures prefer values ​​that are aligned with their size.
// The compiler places 4 bytes of padding after the type to place the value field at the nearest 8-byte boundary.
//
// It doesn't make that much difference putting the type after the union, since most of the time we'll be dealing with arrays of values.
typedef struct {
  ValueType type;

  // We have a dynamically typed programming language, where each variable can contain different types at different times.
  // Currently, at the time I am writing this annotation, it is unityped, which means that a variable can
  // only have one type, in this case, number.
  //
  // A union looks like a struct, but all of its values overlaps in the memory.
  union {
    bool boolean;
    double number;

    // It's probably a pointer to an object allocated on the heap.
    Obj* obj;
  } as;
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER) 
#define IS_OBJ(value) ((value).type == VAL_OBJ)

#define AS_OBJ(value) ((value).as.obj)
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif