#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * 256 * 256)

typedef struct {
  ObjClosure* closure;
  // Dereferencing a pointer is faster than accessing an array by its index.
  // The JVM calls this a Program Counter (PC). x86, x64, and CLR call it an Instruction Pointer (IP).
  // It always points to the next instruction.
  uint8_t* ip;
  Value* slots;
} CallFrame;

typedef struct {
  CallFrame frames[FRAMES_MAX];
  int frameCount;
  Value stack[STACK_MAX];
  Value* stackTop;
  Table globals;
  Obj* objects;
  Table strings;
  ObjUpvalue* openUpvalues;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif