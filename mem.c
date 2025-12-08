#include <stdlib.h>

#include "mem.h"
#include "vm.h"

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  // If the new size is zero, it means that it is allowed to release the allocation.
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  // If it's different from zero, it means we need a reallocation.
  // When oldSize is zero, realloc behaves like malloc.
  void* result = realloc(pointer, newSize);

  // An interesting case occurs when the new size is larger than the previous size. If there is no available space
  // immediately after the current block, realloc allocates a new memory block, copies the data from the previous block,
  // frees the previous block, and returns the new pointer to a new data block with the updated size.
  // If it is smaller, it only updates the bookkeeping information.
  // Many implementations of malloc store the size in memory right before the returned address.

  if (result == NULL) exit(1);

  return result;
}

static void freeObject(Obj* object) {
  switch (object->type) {
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      FREE_ARRAY(char, string->chars, string->length + 1);
      FREE(ObjString, object);
      break;
    }
  }
}

void freeObjects() {
  Obj* object = vm.objects;

  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
}