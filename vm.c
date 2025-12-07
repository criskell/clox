#include <stdio.h>
#include <stdarg.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"
#include "value.h"

VM vm;

static void resetStack() {
  vm.stackTop = vm.stack;
}

// "..." means that the function accepts a variable number of arguments.
static void runtimeError(const char* format, ...) {
  // Stores the state for iterating over variadic variables.
  // It's like a cursor that moves through the variadic arguments.
  va_list args;

  // It tells the compiler to start reading the additional arguments.
  // Now args points to the first extra argument.
  va_start(args, format);

  // A variation of fprintf that explicitly retrieves a va_list.
  vfprintf(stderr, format, args);

  // Release whatever the implementation needs.
  va_end(args);

  fputs("\n", stderr);

  // "-1" is used to retrieve the instruction that just executed.
  size_t previousInstructionIndex = vm.ip - vm.chunk->code - 1;
  
  int line = vm.chunk->lines[previousInstructionIndex - 1];

  fprintf(stderr, "[line %d] in script\n", line);

  // It puts the machine in a consistent state.
  resetStack();
}

void initVM() {
  resetStack();
}

void freeVM() {
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// The `do/while` loop here is a fun technique. It allows you to expand the macro into a set of statements within a block
// and still allow a semicolon after it. If we only used blocks, we wouldn't be able to put a semicolon after the macro.
#define BINARY_OP(valueType, op) \
  do { \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
      runtimeError("Operands must be numbers."); \
      return INTERPRET_RUNTIME_ERROR; \
    }\
    double b = AS_NUMBER(pop()); \
    double a = AS_NUMBER(pop()); \
    push(valueType(a op b)); \
  } while(false)

  for (;;) { 
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");

    if (vm.stackTop == vm.stack) {
      printf("[ empty stack ]");
    }

    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }

    printf("\n");

    disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

    uint8_t instruction;

    // This process is called dispatching or instruction decoding.
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_NOT:
        push(BOOL_VAL(isFalsey(pop())));
        break;
      case OP_NEGATE: {
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }

        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      }
      case OP_NIL: push(NIL_VAL); break;
      case OP_TRUE: push(BOOL_VAL(true)); break;
      case OP_FALSE: push(BOOL_VAL(false)); break;
      case OP_EQUAL: {
        Value a = pop();
        Value b = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
      case OP_ADD: BINARY_OP(NUMBER_VAL, +); break;
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /); break;
      case OP_RETURN: {
        printValue(pop());
        printf("\n");
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  Chunk chunk;

  initChunk(&chunk);

  if (!compile(source, &chunk)) {
    freeChunk(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code;

  InterpretResult result = run();

  freeChunk(&chunk);
  return result;
}
