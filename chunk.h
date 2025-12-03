#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

typedef enum {
  OP_RETURN,
} OpCode;

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
} Chunk;

void initChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte);

#endif