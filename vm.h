#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunk;
  // Dereferencing a pointer is faster than accessing an array by its index.
  // The JVM calls this a Program Counter (PC). x86, x64, and CLR call it an Instruction Pointer (IP).
  // It always points to the next instruction.
  uint8_t* ip;

  Value stack[STACK_MAX];
  Value* stackTop;
  Table globals;
  Obj* objects;
  Table strings;
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