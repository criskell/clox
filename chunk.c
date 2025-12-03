#include <stdlib.h>

#include "chunk.h"

void initChunk(Chunk* chunk) {
  chunk->code = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
}