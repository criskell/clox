#include "chunk.h"
#include "mem.h"
#include "value.h"

void initChunk(Chunk* chunk) {
  chunk->code = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  chunk->count = 0;

  chunk->lineCount = 0;
  chunk->lineCapacity = 0;
  chunk->lineChunks = NULL;

  // Initialize constant pool too
  initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  freeValueArray(&chunk->constants);
  FREE_ARRAY(LineStart, chunk->lineChunks, chunk->lineCapacity);
  initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
  if (chunk->capacity < chunk->count + 1) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  chunk->count++;

  // Same line?
  // This applies compression, Run-Length Encoding (RLE) technique.
  if (chunk->lineCount > 0 && chunk->lineChunks[chunk->lineCount - 1].line == line) {
    return;
  }

  if (chunk->lineCapacity < chunk->lineCount + 1) {
    int oldCapacity = chunk->lineCapacity;
    chunk->lineCapacity = GROW_CAPACITY(oldCapacity);
    chunk->lineChunks = GROW_ARRAY(LineStart, chunk->lineChunks, oldCapacity, chunk->lineCapacity);
  }

  LineStart* lineStart = &chunk->lineChunks[chunk->lineCount++];
  lineStart->offset = chunk->count - 1;
  lineStart->line = line;
}

int addConstant(Chunk* chunk, Value value) {
  writeValueArray(&chunk->constants, value);
  return chunk->constants.count - 1;
}

void writeConstant(Chunk* chunk, Value value, int line) {
  int index = addConstant(chunk, value);

  if (index < 256) {
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, (uint8_t)index, line);
  } else {
    writeChunk(chunk, OP_CONSTANT_LONG, line);
    // We chose the little-endian order.
    // 24 bits in total.
    writeChunk(chunk, (uint8_t)(index & 0xff), line);
    writeChunk(chunk, (uint8_t)((index >> 8) & 0xff), line);
    writeChunk(chunk, (uint8_t)((index >> 16) & 0xff), line);
  }
}

int getLine(Chunk* chunk, int instructionOffset) {
  // It performs a binary search to find the largest offset in lineChunks that is less than or equal to the desired offset.
  // When we find it, we return the associated line.
  // N.B.: The sorting key needs to grow monotonically for binary search to work.

  // Define the limits of binary search.
  int start = 0;
  int end = chunk->lineCount - 1;

  for (;;) {
    // It is the index in between start and end.
    int mid = (start + end) / 2;

    // This is the entry related to the middle.
    LineStart* line = &chunk->lineChunks[mid];

    if (instructionOffset < line->offset) {
      // If the desired offset is less than the offset stored in that middle, it means the correct line is earlier,
      // so the end is moved to mid -1.
      end = mid - 1;
    } else if (mid == chunk->lineCount - 1 ||
      instructionOffset < chunk->lineChunks[mid + 1].offset) {
        // Check two conditions:
        // Either mid is the last line chunk or the next line chunk starts after the instructionOffset.
        // If either of these conditions is true, it means that the instructionOffset is included in that line chunk.
        return line->line;
    } else {
      // The desired offset is further ahead, so we move the lower limit to +1.
      start = mid + 1;
    }
  }
}