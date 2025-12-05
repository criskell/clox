#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

#include <stdio.h>

int main(int argc, const char* argv[]) {
  initVM();

  Chunk chunk;

  initChunk(&chunk);
  
  // OP_CONSTANT <1.2>
  int constantIndex = addConstant(&chunk, 1.2);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constantIndex, 123);

  // OP_CONSTANT <3.4>
  constantIndex = addConstant(&chunk, 3.4);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constantIndex, 123);

  // OP_ADD
  writeChunk(&chunk, OP_ADD, 123);

  // OP_CONSTANT <5.6>
  constantIndex = addConstant(&chunk, 5.6);
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constantIndex, 123);

  // OP_DIVIDE
  writeChunk(&chunk, OP_DIVIDE, 123);

  // OP_NEGATE
  writeChunk(&chunk, OP_NEGATE, 123);
  
  // OP_RETURN
  writeChunk(&chunk, OP_RETURN, 123);
  
  disassembleChunk(&chunk, "test chunk");
  interpret(&chunk);
  freeVM();
  freeChunk(&chunk);

  return 0;
}