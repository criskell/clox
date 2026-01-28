#include <stdio.h>

#include "debug.h"
#include "chunk.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);

  for (int offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

static int constantInstruction(const char *name, Chunk* chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];

  // `%-16s`
  // Prints a 16-character string aligned to the left because of the "-".
  // Adds spaces to the end of the string if it is shorter than 16 characters.
  // `%4d`
  // Prints an integer (%d) occupying 4 characters, right-aligned.
  // If the number requires fewer characters, it is pushed to the right with spaces occupying the left.
  printf("%-16s %4d '", name, constant);
  printValue(chunk->constants.values[constant]);
  printf("'\n");

  // The instruction format requires 2 bytes (opcode + operand).
  return offset + 2;
}

static int longConstantInstruction(const char* name, Chunk* chunk, int offset) {
  uint32_t constantIndex = chunk->code[offset + 1]
    | (chunk->code[offset + 2] << 8)
    | (chunk->code[offset + 3] << 16);

  printf("%-16s %4d '", name, constantIndex);
  printValue(chunk->constants.values[constantIndex]);
  printf("'\n'");
  
  return offset + 4;
}

static int simpleInstruction(const char* name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
  uint8_t slot = chunk->code[offset + 1];
  printf("%-16s %4d\n", name, slot);
  return offset + 2;
}

static int shortInstruction(const char* name, Chunk* chunk, int offset) {
  uint8_t slot = (chunk->code[offset + 1] << 8) | chunk->code[offset + 2];
  printf("%-16s %4d\n", name, slot);
  return offset + 3;
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("%04d ", offset);

  int line = getLine(chunk, offset);

  if (offset > 0 && getLine(chunk, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
  }

  uint8_t instruction = chunk->code[offset];

  switch (instruction) {
    case OP_CONSTANT:
      return constantInstruction("OP_CONSTANT", chunk, offset);
    case OP_NIL:
      return simpleInstruction("OP_NIL", offset);
    case OP_TRUE:
      return simpleInstruction("OP_TRUE", offset);
    case OP_FALSE:
      return simpleInstruction("OP_FALSE", offset);
    case OP_POP:
      return simpleInstruction("OP_POP", offset);
    case OP_GET_LOCAL:
      return byteInstruction("OP_GET_LOCAL", chunk, offset);
    case OP_GET_LOCAL_LONG:
      return shortInstruction("OP_GET_LOCAL_LONG", chunk, offset);
    case OP_SET_LOCAL:
      return byteInstruction("OP_SET_LOCAL", chunk, offset);
    case OP_SET_LOCAL_LONG:
      return shortInstruction("OP_SET_LOCAL_LONG", chunk, offset);
    case OP_GET_GLOBAL:
      return constantInstruction("OP_GET_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL:
      return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL:
      return constantInstruction("OP_SET_GLOBAL", chunk, offset);
    case OP_EQUAL:
      return simpleInstruction("OP_EQUAL", offset);
    case OP_GREATER:
      return simpleInstruction("OP_GREATER", offset);
    case OP_LESS:
      return simpleInstruction("OP_LESS", offset);
    case OP_ADD:
      return simpleInstruction("OP_ADD", offset);
    case OP_SUBTRACT:
      return simpleInstruction("OP_SUBTRACT", offset);
    case OP_MULTIPLY:
      return simpleInstruction("OP_MULTIPLY", offset);
    case OP_DIVIDE:
      return simpleInstruction("OP_DIVIDE", offset);
    case OP_NOT:
      return simpleInstruction("OP_NOT", offset);
    case OP_NEGATE:
      return simpleInstruction("OP_NEGATE", offset);
    case OP_CONSTANT_LONG:
      return longConstantInstruction("OP_CONSTANT_LONG", chunk, offset);
    case OP_PRINT:
      return simpleInstruction("OP_PRINT", offset);
    case OP_RETURN:
      return simpleInstruction("OP_RETURN", offset);
    default:
      printf("Unknown opcode: %d\n", instruction);
      return offset + 1;
  }
}