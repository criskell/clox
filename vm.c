#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"
#include "value.h"
#include "mem.h"
#include "object.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
  vm.openUpvalues = NULL;
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

  // Emit '\n' to `stderr` stream.
  fputs("\n", stderr);

  // Prints stacktrace.
  //
  // It walks through the call stack from the top (the most recently called function) to the bottom (the implicit top-level function).
  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;

    // `-1` is used to retrieve the instruction that just executed.
    size_t instruction = frame->ip - function->chunk.code - 1;

    fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);

    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  // It puts the machine in a consistent state.
  resetStack();
}

static void defineNative(const char* name, NativeFn function) {
  // `copyString()` and `newNative()` dynamically allocate memory, which means that when a garbage collector is present,
  // they can trigger a garbage collection.
  // Storing values ​​on the stack prevents the name and function from being collected while we are using them.
  push(OBJ_VAL(copyString(name, (int) strlen(name))));
  push(OBJ_VAL(newNative(function)));

  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);

  pop();
  pop();
}

void initVM() {
  resetStack();

  vm.objects = NULL; 
  initTable(&vm.globals);
  initTable(&vm.strings);

  defineNative("clock", clockNative);
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

static bool call(ObjClosure* closure, int argCount) {
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;

  // `-1` is to align the frame window with the beginning of the arguments including function object.
  // Otherwise, it would skip the function object.
  frame->slots = vm.stackTop - argCount - 1;
  
  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_CLOSURE:
        return call(AS_CLOSURE(callee), argCount);

      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1; // Remove arguments from stack
        push(result);
        return true;
      }

      default:
        break;
    }
  }

  runtimeError("Can only call functions and classes.");
  return false;
}

static ObjUpvalue* captureUpvalue(Value* local) {
  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;

  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue* createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }

  return createdUpvalue;
}

static void closeUpvalues(Value* last) {
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
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
  CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
  (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (frame->closure->function->chunk.constants.values[READ_SHORT()])
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

    disassembleInstruction(&frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
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
        push(frame->slots[slot]);
        break;
      }
      case OP_GET_LOCAL_LONG: {
        uint16_t slot = READ_SHORT();
        push(frame->slots[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        frame->slots[slot] = peek(0);
        break;
      }
      case OP_SET_LOCAL_LONG: {
        uint16_t slot = READ_SHORT();
        frame->slots[slot] = peek(0);
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
      case OP_GET_UPVALUE: {
        uint8_t slot = READ_BYTE();

        push(*frame->closure->upvalues[slot]->location);
        
        break;
      }
      case OP_SET_UPVALUE: {
        uint8_t slot = READ_BYTE();

        *frame->closure->upvalues[slot]->location = peek(0);

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
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();

        if (isFalsey(peek(0))) {
          frame->ip += offset;
        }

        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        
        break;
      }
      case OP_CALL: {
        int argCount = READ_BYTE();

        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }

        frame = &vm.frames[vm.frameCount - 1];

        break;
      }
      case OP_CLOSURE: {
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT()); // Load the compiled function from the constant table.
        ObjClosure* closure = newClosure(function); // Wrap that function in a new ObjClosure.
        push(OBJ_VAL(closure)); // Push the result onto the stack.
        
        for (int i = 0; i < closure->upvalueCount; i++) {
          uint8_t isLocal = READ_BYTE();
          uint8_t index = READ_BYTE();

          if (isLocal) {
            closure->upvalues[i] = captureUpvalue(frame->slots + index);
          } else {
            closure->upvalues[i] = frame->closure->upvalues[index];
          }
        }
        
        break;
      }
      case OP_CLOSE_UPVALUE: {
        closeUpvalues(vm.stackTop - 1);
        pop();
        break;
      }
      case OP_RETURN: {
        Value result = pop();

        // Pass the first stack slot owned by the function
        // To close all upvalues.
        closeUpvalues(frame->slots);

        vm.frameCount--;

        if (vm.frameCount == 0) {
          // This is the last call frame.
          pop(); // Remove the main script function from the stack.

          // Exit interpreter.
          return INTERPRET_OK;
        }

        // Discard all slots that callee was using for its parameters and local variables.
        // This includes the same slots that the caller was using to pass the arguments.
        // The top of the stack ends at the beginning of the stack window of the function that is returning.
        vm.stackTop = frame->slots;
        push(result);
      
        frame = &vm.frames[vm.frameCount - 1];

        break;
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
  ObjFunction* function = compile(source);

  if (function == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }

  // Stores the function in slot zero of the stack.
  push(OBJ_VAL(function)); // Let the garbage collector know the function has been allocated on the heap.

  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0);

  return run();
}
