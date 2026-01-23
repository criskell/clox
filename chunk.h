#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  // One trade-off when having the same instructions but for different sizes is that it increases the code size of the main loop,
  // thus harming (spatial-)locality.
  // Locality is the property of programs that describes how they access memory.
  // - Temporal locality: If you've used a piece of data recently, you're likely to use it again in the future.
  //                      This data tends to remain cached.
  // - Spatial locality: If you access an address, you're likely accessing nearby data in memory.
  //                     Using data that is already close together avoids accessing slow memory.
  OP_CONSTANT_LONG,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_GET_GLOBAL,
  OP_DEFINE_GLOBAL,
  OP_SET_GLOBAL,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_NEGATE,
  OP_PRINT,
  OP_RETURN,
} OpCode;

typedef struct {
  int offset;
  int line;
} LineStart;

// Bytecode would be a compact, serialized version of the AST, highly optimized for how the interpreter will deserialize it.
typedef struct {
  // This implements a dynamic array.
  // A dynamic array consists of a pointer to a fixed array, the number of elements, and the capacity.
  // It has several advantages:
  // - It is a cache-friendly data structure with dense storage.
  // - Searching for elements by index is O(1).
  // - Inserting elements at the end of the array is O(1).
  // It works by reallocating and copying elements when the count exceeds capacity. It appears that the time complexity is linear,
  // but in an amortized analysis, it becomes O(1) because the total is O(n) but the number of operations is n: O(n) / n = O(1).
  // That is, the occasional cost of some operations is spread among several inexpensive operations, resulting in a low average cost.
  int count;
  int capacity;
  uint8_t* code;
  // This is inefficient in terms of memory but good in terms of caching, since an interpreter only cares about the operand and opcodes,
  // causing fewer cache misses on the CPU.
  // Stores an array that matches instruction offsets in the bytecode to lines of compiled code.
  int* lines;

  int lineCount;
  int lineCapacity;
  
  LineStart* lineChunks;

  // Stores constant pool.
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);
void writeConstant(Chunk* chunk, Value value, int line);
int getLine(Chunk* chunk, int instructionOffset);

#endif