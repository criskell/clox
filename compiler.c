#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
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
  PREC_ASSIGNMENT,
  PREC_CONDITIONAL,
  PREC_OR,
  PREC_AND,
  PREC_EQUALITY,
  PREC_COMPARISON,
  PREC_TERM,
  PREC_FACTOR,
  PREC_UNARY,
  PREC_CALL,
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
} Local;

typedef struct {
  Local locals[UINT8_COUNT];
  int localCount;
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
  return compilingChunk;
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

// Emits bytecode for a return instruction.
static void emitReturn() {
  emitByte(OP_RETURN);
}

// Adds a constant to the constant pool.
//
// FIXME: Support OP_CONSTANT_LONG.
static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);

  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

// Emit OP_CONSTANT instruction.
static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler) {
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  initTable(&compiler->globalFinals);
  current = compiler;
}

// End compiler and maybe print code.
static void endCompiler() {
  emitReturn();

#ifdef DEBUG_PRINT_CODE
  // We added this check because in syntax error mode, the compiler might continue to generate nonsensical code.
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
    printf("==\n");
  } 
#endif

  freeTable(&current->globalFinals);
}

static void beginScope() {
  // Create a new scope simply by increasing the scope depth. Much faster than the jlox implementation which creates a
  // new HashMap for each scope.
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;

  // All locations with a scope depth greater than the current one will be discarded.
  // `current->localCount - 1` acts as the last existing local in the array, if `localCount` is greater than zero..
  while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
    // Remember that local variables are slots on the stack, and when local variables are removed, we must remove the slots as well.
    // One optimization is having an instruction that specifies the number of slots to remove from the stack.
    emitByte(OP_POP);

    // The quantity decreases because we removed a variable.
    current->localCount--;
  }
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
  uint8_t index = makeConstant(OBJ_VAL(string));
  tableSet(&stringConstants, string, NUMBER_VAL((double)index));

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

static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  local->isFinal = false;
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

// Declaring is when a variable is added to the scope, and defining is when it becomes available for use.
static void markInitialized() {
  current->locals[current->localCount - 1].depth = current->scopeDepth;
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
  parsePrecedence(PREC_OR);
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);

  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    if (setOp == OP_SET_LOCAL && current->locals[arg].isFinal) {
      error("Cannot assign to final variable.");
    }

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
    emitBytes(setOp, (uint8_t)arg); 
  } else {
    emitBytes(getOp, (uint8_t)arg);
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
  [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
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
  [TOKEN_AND] = {NULL, NULL, PREC_NONE},
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

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
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
  } else {
    statement();
  }

  // We chose statements as the synchronization point in panic mode error recovery.
  if (parser.panicMode) synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
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
bool compile(const char* source, Chunk* chunk) {
  // Start the lazy lexical analysis.
  initScanner(source);

  Compiler compiler;
  initCompiler(&compiler);

  compilingChunk = chunk;
  parser.hadError = false;
  // Used to prevent cascading errors.
  parser.panicMode = false;

  initTable(&stringConstants);
  advance();

  while (!match(TOKEN_EOF)) {
    declaration();  
  }

  endCompiler();
  freeTable(&stringConstants);

  return !parser.hadError;
}