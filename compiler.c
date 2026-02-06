#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

Table stringConstants;

// The purpose of this struct is to maintain the parser's state while it is reading the source code.
typedef struct {
  // Store the current token that the parser is analyzing.
  Token current;

  // Stores the last token consumed.
  Token previous;
  
  // Indicates whether an error occurred during parsing, for error reporting.
  bool hadError;

  // Indicates whether the parser is in panic mode, used to implement error recovery in the parser.
  bool panicMode;
} Parser;

// The goal is to indicate to the Pratt parser which operation should be evaluated first.
// The lower down the list, the higher the precedence.
// It indicates how far the Pratt Parser should go before returning a subexpression.
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // `=`.
  PREC_CONDITIONAL, // Ternary expression.
  PREC_OR, // `or`.
  PREC_AND, // `and`.
  PREC_EQUALITY, // `==` and `!=`.
  PREC_COMPARISON, // `<`, `>`, `>=` and `<=`.
  PREC_TERM, // `+` and `-`.
  PREC_FACTOR, // `*` and `/`.
  PREC_UNARY, // `!` and `-`.
  PREC_CALL, // `(` and `.`.
  PREC_PRIMARY
} Precedence;

// Pointer to a function that has no parameters and returns nothing (void).
typedef void (*ParseFn)(bool canAssign);

// It specifies how a token should be read during parsing.
typedef struct {
  // It's a pointer to a function that knows how to parse a prefixed expression.
  ParseFn prefix;

  // Called when the parser encounters an infix operator.
  ParseFn infix;

  // It indicates the operator precedence for this rule.
  // Defines how much the expression swallows before returning.
  Precedence precedence;

  // This is a anonymous struct.
} ParseRule;

typedef struct {
  // Local stores a copy of a token.
  // A token stores a pointer to the first character of the lexeme.
  // As long as the strings survive the compilation process, which should happen since we are compiling, the pointer inside
  // the Token should remain valid.
  Token name;
  int depth;
  bool isFinal;
  bool isCaptured;
} Local;

typedef struct {
  // Stores which local slot the upvalue is capturing.
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
  // Implements a stack of compilers.
  struct Compiler* enclosing;

  ObjFunction* function;
  FunctionType type;

  Local locals[UINT16_MAX + 1];
  int localCount;
  Upvalue upvalues[UINT8_COUNT];
  int scopeDepth;
  Table globalFinals;
} Compiler;

// A pointer to the parser's state.
Parser parser;

Compiler* current = NULL;

// A pointer to the chunk currently being compiled.
Chunk* compilingChunk;

// Returns the current chunk where the bytecode instructions are being inserted.
static Chunk* currentChunk() {
  return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
  // FIXME: Clear this flag when we reach a statement boundary.
  //        The parser has a good chance of getting lost in irrelevant details, but the user won't see the cascading errors.
  if (parser.panicMode) return;
  parser.panicMode = true;

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Do not write anything on the screen.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

// Advance to next token.
static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();

    if (parser.current.type != TOKEN_ERROR) {
#ifdef DEBUG_PRINT_TOKENS
    printf("== parser.current = %2d '%.*s'\n", parser.current.type, parser.current.length, parser.current.start);
#endif
      break;
    }

    errorAtCurrent(parser.current.start);
  }
}

// Consume a required token.
static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) {
  return parser.current.type == type;
}

static bool match(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
}

// Emits a byte in the current bytecode chunk.
static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

// Emit two bytes at once in the current bytecode chunk.
static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) error("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
  emitByte(instruction);

  // Write a placeholder operand for the jump offset.
  // Allows skipping up to 65,535 (2^16 - 1) bytes of code.
  emitByte(0xff);
  emitByte(0xff);

  // Returns the offset of the instruction issued in the code.
  return currentChunk()->count - 2;
}

static void emitShort(uint16_t value) {
  emitByte((value >> 8) & 0xff);
  emitByte(value & 0xff);
}

// Emits bytecode for a return instruction.
static void emitReturn() {
  emitByte(OP_NIL);
  emitByte(OP_RETURN);
}

// Adds a constant to the constant pool.
static uint16_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);

  if (constant > UINT16_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint16_t)constant;
}

// Emit OP_CONSTANT instruction.
static void emitConstant(Value value) {
  int constant = addConstant(currentChunk(), value);

  if (constant <= UINT8_MAX) {
    emitBytes(OP_CONSTANT, (uint8_t)constant);
  } else if (constant <= UINT16_MAX) {
    emitByte(OP_CONSTANT_LONG);
    emitShort((uint16_t)constant);
  } else {
    error("Too many constants in one chunk.");
  }
}

// Apply a named technique `backpatching`.
static void patchJump(int offset) {
  // `-2` skip over the jump offset bytes.
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();

  initTable(&compiler->globalFinals);
  
  current = compiler;

  if (type != TYPE_SCRIPT) {
    // We call `function` immediately after parsing the function name.
    // So the function name is placed in the previous token.
    //
    // We need to copy the string because the function object outlives compilation.
    // The lexeme points to the original string of source code that can be freed after compilation.
    current->function->name = copyString(parser.previous.start, parser.previous.length);
  }

  // Claims stack slot zero
  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->name.start = "";
  local->name.length = 0;
  local->isCaptured = false;
}

// End compiler and maybe print code.
static ObjFunction* endCompiler() {
  emitReturn();

  ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
  // We added this check because in syntax error mode, the compiler might continue to generate nonsensical code.
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    printf("==\n");
  } 
#endif

  freeTable(&current->globalFinals);

  current = current->enclosing;

  return function;
}

static void beginScope() {
  // Create a new scope simply by increasing the scope depth. Much faster than the jlox implementation which creates a
  // new HashMap for each scope.
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;

  int localsToPop = 0;

  // All locations with a scope depth greater than the current one will be discarded.
  // `current->localCount - 1` acts as the last existing local in the array, if `localCount` is greater than zero..
  while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
    // Remember that local variables are slots on the stack, and when local variables are removed, we must remove the slots as well.
    // One optimization is having an instruction that specifies the number of slots to remove from the stack.

    localsToPop++;

    if (current->locals[current->localCount - 1].isCaptured) {
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      emitByte(OP_POP);
    }

    // The quantity decreases because we removed a variable.
    current->localCount--;
  }

  // FIXME
  // if (localsToPop == 1) {
  //   emitByte(OP_POP);
  // } else if (localsToPop <= 255) {
  //   emitBytes(OP_POPN, localsToPop);
  // } else {
  //   while (localsToPop > 255) {
  //     emitBytes(OP_POPN, 255);
  //     localsToPop -= 255;
  //   }

  //   if (localsToPop > 0) {
  //     emitBytes(OP_POPN, (uint8_t)localsToPop);
  //   }
  // }
}

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static void declaration();
static void statement();

/**
 * Store a token in the constant pool and returns the index from it.
 *
 * It caches the identifier strings to reduce the entries in the constant pool.
 */
static uint8_t identifierConstant(Token* name) {
  // Remember that strings are already internalized.
  // We are saving space in the constant pool.
  ObjString* string = copyString(name->start, name->length);
  Value indexValue;

  if (tableGet(&stringConstants, string, &indexValue)) {
    return (uint8_t)AS_NUMBER(indexValue);
  }

  // `string` is a pointer to an object allocated on the heap.
  uint8_t index = (uint8_t)makeConstant(OBJ_VAL(string));

  // FIXME: There is a bug that reuses the index of an identifier found in another chunk of a function.
  // tableSet(&stringConstants, string, NUMBER_VAL((double)index));

  return index;
}

static bool identifiersEqual(Token* a, Token* b) {
  if (a->length != b->length) return false;

  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      
      return i;
    }
  }

  return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
  int upvalueCount = compiler->function->upvalueCount;

  for (int i = 0; i < upvalueCount; i++) {
    Upvalue* upvalue = &compiler->upvalues[i];

    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
  if (compiler->enclosing == NULL) {
    // Base case where the variable can't be resolved lexically and is treated as global.    
    // We know we've reached the outermost function without finding a local variable.
    // The variable must be global.
    return -1;
  }

  int local = resolveLocal(compiler->enclosing, name);

  if (local != -1) {
    // Base case where the local variable is found in it's enclosing compiler.
    compiler->enclosing->locals[local].isCaptured = true;

    return addUpvalue(compiler, (uint8_t)local, true);
  }

  int upvalue = resolveUpvalue(compiler, name);

  if (upvalue != -1) {
    return addUpvalue(compiler, (uint8_t)upvalue, false);
  }

  return -1;
}

static void addLocal(Token name) {
  if (current->localCount == UINT16_MAX + 1) {
    error("Too many variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  local->isFinal = false;
  local->isCaptured = false;
}

static void declareVariable() {
  if (current->scopeDepth == 0) {
    // Global variables are late bound.
    return;
  }

  Token* name = &parser.previous;

  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];

    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  // Declaring a local variable essentially adds the compiler's list of variables to the current scope.
  // The compiler needs to remember that the variable exists.
  addLocal(*name);
}

// Parses a variable's identifier and binds a constant in the pool to a string object that stores the variable's identifier.
static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  declareVariable();

  if (current->scopeDepth > 0) {
    // Dummy table index
    return 0;
  }

  // Global variables are searched by name at runtime, and using constants prevents embedding large strings
  // directly into the chunk's instruction list.
  return identifierConstant(&parser.previous);
}

// Mark latest variable as initialized.
static void markInitialized() {
  if (current->localCount == 0) {
    return;
  }

  current->locals[current->localCount - 1].depth = current->scopeDepth;
  
  // Declaring is when a variable is added to the scope, and defining is when it becomes available for use.
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    // The temporary (from initializer expression) is already the local (variable).
    // Locals are always allocated to the top of the stack.
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;

  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();

      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }

      argCount++;
    } while (match(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool canAssign) {
  // At this point, the expression on the left has already been compiled.
  // And it's located at the top of the stack.

  // If it's false, we skip the right-hand operand and leave the left-hand expression on top of the stack.
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  
  // Pops the condition value (left-hand side expression) from the stack
  emitByte(OP_POP); 

  // Evaluates the right operand.
  // It becomes the result of the entire AND expression.

  // `parsePrecedence` continues the parsing from the current token.
  // Consumes expressions whose precedence is greater than or equal to `PREC_AND`.
  // Ensures that expressions with higher precedence (comparison, equality, etc.) are correctly grouped on the right-hand side.
  // For example, given
  // `a and b == c and d`
  // We correctly group expressions following this form
  // `(a and (b == c)) and d`
  // This requires that the right-hand side of the first `and` consumes everything that has a precedence greater than to `and`.
  // but do not consume operators with lower precedence (like another `and` or `or`).
  // This `parsePrecedence` does not consume the subsequent `and`, thus maintaining left associativity.
  parsePrecedence(PREC_AND);

  patchJump(endJump);
}

// The goal is to compile a binary operator found in an expression.
static void binary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);

  // `+1` makes left-associative.
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
    case TOKEN_BANG_EQUAL:
      emitBytes(OP_EQUAL, OP_NOT);
      break;

    case TOKEN_EQUAL_EQUAL:
      emitByte(OP_EQUAL);
      break;

    case TOKEN_GREATER:
      emitByte(OP_GREATER);
      break;

    case TOKEN_GREATER_EQUAL:
      emitBytes(OP_LESS, OP_NOT);
      break;

    case TOKEN_LESS:
      emitByte(OP_LESS);
      break;

    case TOKEN_LESS_EQUAL:
      emitBytes(OP_GREATER, OP_NOT);
      break;

    case TOKEN_PLUS:
      emitByte(OP_ADD);
      break;

    case TOKEN_MINUS:
      emitByte(OP_SUBTRACT);
      break;

    case TOKEN_STAR:
      emitByte(OP_MULTIPLY);
      break;

    case TOKEN_SLASH:
      emitByte(OP_DIVIDE);
      break;

    default:
      return;
  }
}

static void call(bool canAssign) {
  uint8_t argCount = argumentList();

  emitBytes(OP_CALL, argCount);
}

static void literal(bool canAssign) {
  switch (parser.previous.type) {
    case TOKEN_FALSE:
      emitByte(OP_FALSE);
      break;

    case TOKEN_NIL:
      emitByte(OP_NIL);
      break;

    case TOKEN_TRUE:
      emitByte(OP_TRUE);
      break;

    default:
      return;
  }
}

// Implement pratt parsing algorithm.
static void parsePrecedence(Precedence precedence) {
  advance();

  ParseFn prefixRule = getRule(parser.previous.type)->prefix;

  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  // esse codigo seria executado em que parte
  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

// Compile unary expressions.
static void unary(bool canAssgin) {
  TokenType operatorType = parser.previous.type;

  // Compile operand.
  // PREC_UNARY allows nested unary expressions too.
  parsePrecedence(PREC_UNARY);

  // Emit operator instruction.
  switch (operatorType) {
    case TOKEN_BANG:
      emitByte(OP_NOT);
      break;
    
    case TOKEN_MINUS:
      emitByte(OP_NEGATE);
      break;

    default:
      return;
  }
}

// Compile number literal expressions.
static void number(bool canAssgin) {
  // `stdpod` convert a string to double.
  // `endptr` is left with unconsumed characters.
  // We assume that the token for the number literal has already been consumed and is stored in `previous`.
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssgin) {
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

// Compile OR expressions.
//
// FIXME: Jumps.
static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;

  int arg = resolveLocal(current, &name);

  if (arg != -1) {
    if (arg < UINT8_COUNT) {
      getOp = OP_GET_LOCAL;
      setOp = OP_SET_LOCAL;
    } else {
      getOp = OP_GET_LOCAL_LONG;
      setOp = OP_SET_LOCAL_LONG;
    }
  } else if ((arg = resolveUpvalue(current, &name)) != -1) {
    // `resolveUpvalue` searches for a variable declared in surrounding functions.
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE; 
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    // Check local final variables
    if ((setOp == OP_SET_LOCAL || setOp == OP_SET_LOCAL_LONG) && current->locals[arg].isFinal) {
      error("Cannot assign to final variable.");
    }

    // Check final global variables
    if (setOp == OP_SET_GLOBAL) {
      ObjString* name = AS_STRING(currentChunk()->constants.values[arg]);
      Value isFinal;

      if (tableGet(&current->globalFinals, name, &isFinal) && AS_BOOL(isFinal)) {
        error("Cannot assign to final variable.");
      }
    }

    // Compile assigned value
    expression();

    // Emit an assignment instruction
    if (setOp == OP_SET_LOCAL_LONG) {
      emitByte(setOp);
      emitShort(arg);
    } else {
      emitBytes(setOp, (uint8_t)arg); 
    }
  } else {
    if (getOp == OP_GET_LOCAL_LONG) {
      emitByte(getOp);
      emitShort(arg);
    } else {
      emitBytes(getOp, (uint8_t)arg);
    }
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

// Compile grouping expresions.
//
// Grouping expressions are called prefix expressions because we can determine the form of the expression simply by
// looking at the first token.
static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// Compile ternary expressions.
static void conditional(bool canAssign) {
  printf("> conditional\n");
  parsePrecedence(PREC_CONDITIONAL);
  printf("Parsed then branch.\n");

  consume(TOKEN_COLON, "Expect ':' after then branch of conditional operator.");
  printf("Consumed colon token.\n");

  parsePrecedence(PREC_ASSIGNMENT);
  printf("< conditional\n");
}

// Define the parser rules.
ParseRule rules[] = {
  [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
  [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
  [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
  [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
  [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
  [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
  [TOKEN_MINUS] = {unary, binary, PREC_TERM},
  [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
  [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
  [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
  [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
  [TOKEN_BANG] = {unary, NULL, PREC_NONE},
  [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
  [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
  [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
  [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
  [TOKEN_STRING] = {string, NULL, PREC_NONE},
  [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
  [TOKEN_AND] = {NULL, and_, PREC_AND},
  [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
  [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
  [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
  [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
  [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
  [TOKEN_IF] = {NULL, NULL, PREC_NONE},
  [TOKEN_NIL] = {literal, NULL, PREC_NONE},
  [TOKEN_OR] = {NULL, or_, PREC_OR},
  [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
  [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
  [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
  [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
  [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
  [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
  [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
  [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
  [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
  [TOKEN_QUESTION] = {NULL, conditional, PREC_CONDITIONAL},
};

// Get parse rule based on the token type.
//
// Returns the address of that rule.
static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

// Compile expressions.
static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler, type);
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;

      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }

      uint8_t constant = parseVariable("Expect parameter name.");
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  // Since we completely terminate the compiler at the end of the function body, there is no need to call endScope().
  ObjFunction* function = endCompiler();

  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    emitByte(compiler.upvalues[i].index);
  }
}

static void funDeclaration() {
  uint8_t global = parseVariable("Expect function name.");
  markInitialized(); // It allows the function to be referenced within its body.
  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void varDeclaration() {
  uint8_t global = parseVariable("Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    // Compile initializer expression
    expression();
  } else {
    // Desugars a uninitialized variable declaration like `var a;` into `var a = nil;`.
    emitByte(OP_NIL);
  }

  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  defineVariable(global);
}

// global is the index in the constant pool
// storing the variable name.
static void defineFinalVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    current->locals[current->localCount - 1].isFinal = true;
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static void finalDeclaration() {
  uint8_t global = parseVariable("Expect final name.");

  if (!match(TOKEN_EQUAL)) {
    error("Final variables must be initialized.");
  }

  // Compile initializer expression
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after declaration.");
  defineFinalVariable(global);

  if (current->scopeDepth == 0) {
    ObjString* name = AS_STRING(currentChunk()->constants.values[global]);
    tableSet(&current->globalFinals, name, BOOL_VAL(true));
  }
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  // an expression evaluates the expression and discards the result.
  emitByte(OP_POP); // ensure statement stack effect = 0 
}

static void forStatement() {
  // Compile initializer
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    // I call this variant because it will guarantee a stack effect of zero by emitting an OP_POP.
    // And search for a semicolon.
    // The initializer should not store anything on the stack.
    expressionStatement();
  }

  int loopStart = currentChunk()->count;

  // Compile condition expression
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Remove condition value.
  }

  // Compile increment
  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;

    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    // Move to the top of the for loop.
    // Move to right before the conditional expression.
    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);

  // Executed when the loop finishes.
  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // Remove condition value from stack.
  }

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
  // The condition value is at the top of the stack now.

  int thenJump = emitJump(OP_JUMP_IF_FALSE);

  // Everything up to `patchJump(thenJump)` will be placed in the `then` branch.
  emitByte(OP_POP); // Pop condition value in case the condition is truthy. Ensures the stack effect = 0 of statements.
  statement(); // Compile then branch

  // This jump is used if the condition is true
  int elseJump = emitJump(OP_JUMP);

  // Now that we've compiled the `then` (`statement` + unconditional jump) branch, we're patching the jump that's executed
  // if the condition is false
  patchJump(thenJump);

  // Here is the else branch.

  // This code will be placed in an `else` statement, which can be implicit
  emitByte(OP_POP); // Pop condition value in case the condition is falsy

  if (match(TOKEN_ELSE)) {
    statement();
  }

  // Patch the jump that is executed within the `then` branch, effectively skipping the `else` branch (+ `OP_POP`).
  patchJump(elseJump);

  // We guarantee that a branch is always taken and that the first instruction in each branch is to remove the value condition.
  // This happens even if we don't have an `else` statement.
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }

  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}

static void whileStatement() {
  int loopStart = currentChunk()->count;

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP);
}

static void synchronize() {
  parser.panicMode = false;

  // Implement error recovery by consuming tokens.
  while (parser.current.type != TOKEN_EOF) {
    // Look at a statement boundary like a semicolon.
    if (parser.previous.type == TOKEN_SEMICOLON) return;

    // Or look at a token that begins a statement.
    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;

      default:
        ; // Do nothing.
    }

    advance();
  }
}

static void declaration() {
  if (match(TOKEN_VAR)) {
    varDeclaration();
  } else if (match(TOKEN_FINAL)) {
    finalDeclaration();
  } else if (match(TOKEN_FUN)) {
    funDeclaration();
  } else {
    statement();
  }

  // We chose statements as the synchronization point in panic mode error recovery.
  if (parser.panicMode) synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

// Entry point of the compiler. It goes through the lexical analysis, syntactic analysis, and compilation
// to bytecode phases in the `Chunk` pointed to by `chunk`.
ObjFunction* compile(const char* source) {
  // Start the lazy lexical analysis.
  initScanner(source);

  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  parser.hadError = false;
  // Used to prevent cascading errors.
  parser.panicMode = false;

  initTable(&stringConstants);
  advance();

  while (!match(TOKEN_EOF)) {
    declaration();  
  }

  ObjFunction* function = endCompiler();
  freeTable(&stringConstants);

  // Returning NULL prevents the VM from executing potentially invalid bytecode due to syntax errors.
  return parser.hadError ? NULL : function;
}