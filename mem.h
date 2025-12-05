#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

// It avoids unnecessary relocations and copies at the beginning of the object.
#define GROW_CAPACITY(capacity) \
  ((capacity < 8) ? 8 : (capacity * 2))

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
  (type*)reallocate(pointer, sizeof(type) * (oldCount), \
    sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
  reallocate(pointer, sizeof(type) * (oldCount), 0)

// The arguments passed to this function control allocation, deallocation, and reallocation.
// oldSize = non-zero, newSize = 0: Free.
// oldSize = 0, newSize = non-zero: Allocate.
// oldSize = non-zero, newSize < oldSize: Shrink existing allocation.
// oldSize = non-zero, newSize > oldSize: Grow existing allocation.  
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif