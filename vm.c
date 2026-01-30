#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"
#include "value.h"
#include "mem.h"
#include "object.h"

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

  vm.objects = NULL; 
  initTable(&vm.globals);
  initTable(&vm.strings);
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  freeObjects();
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

static void concatenate() {
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());

  int length = a->length + b->length;
  ObjString* result = makeString(length);
  memcpy(result->chars, a->chars, a->length);
  memcpy(result->chars + a->length, b->chars, b->length);
  result->hash = hashString(result->chars, length);
  result->chars[length] = '\0';

  ObjString* interned = tableFindString(&vm.strings, result->chars, length, result->hash);

  if (interned) {
    freeObject((Obj*) result);
    push(OBJ_VAL(interned));
    return;
  }

  internString(result);
  push(OBJ_VAL(result));
}

static InterpretResult run() {

#define READ_BYTE() (*vm.ip++)
#define READ_SHORT() \
  (vm.ip += 2, (uint16_t)((vm.ip[-2] << 8) | vm.ip[-1]))
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm.chunk->constants.values[READ_SHORT()])
#define READ_STRING() AS_STRING(READ_CONSTANT())

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
      case OP_CONSTANT_LONG: {
        Value constant = READ_CONSTANT_LONG();
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
      case OP_POP: pop(); break;
      case OP_POPN: {
        uint8_t count = READ_BYTE();
        vm.stackTop -= count;
        break;
      }
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
        push(vm.stack[slot]);
        break;
      }
      case OP_GET_LOCAL_LONG: {
        uint16_t slot = READ_SHORT();
        push(vm.stack[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        vm.stack[slot] = peek(0);
        break;
      }
      case OP_SET_LOCAL_LONG: {
        uint16_t slot = READ_SHORT();
        vm.stack[slot] = peek(0);
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;

        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);

          // Exit interpreter loop
          return INTERPRET_RUNTIME_ERROR;
        }

        push(value);
        break;
      }
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();

        // Retrieves the value of the variable on the stack with a distance of 0.
        if (tableSet(&vm.globals, name, peek(0))) {
          // The value is not removed from the stack because the assignment is an expression.
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }

        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        
        // Initializer expression is in stack at 0 distance.
        // We use peek() here to ensure that the value is reachable by the garbage collector and thus is not collected
        // in the middle of an insertion into the hash table. A hash table can trigger a dynamic allocation which can
        // trigger garbage collection.
        tableSet(&vm.globals, name, peek(0));
        pop();

        break;
      }
      case OP_EQUAL: {
        Value a = pop();
        Value b = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
      // Stack effect = -1
      case OP_ADD: {
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());

          push(NUMBER_VAL(a + b));
        } else {
          runtimeError("Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }

        break;
      }
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /); break;
      // Stack effect = 0
      case OP_PRINT: {
        printValue(pop());
        printf("\n");
        break;
      }
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        vm.ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();

        if (isFalsey(peek(0))) {
          vm.ip += offset;
        }

        break;
      }
      case OP_RETURN: {
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
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
