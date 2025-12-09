#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

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
typedef void (*ParseFn)();

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

// A pointer to the parser's state.
Parser parser;

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
}

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static void declaration();
static void statement();

static uint8_t identifierConstant(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  // Global variables are searched by name at runtime, and using constants prevents embedding large strings
  // directly into the chunk's instruction list.
  return identifierConstant(&parser.previous);
}

static void defineVariable(uint8_t global) {
  emitBytes(OP_DEFINE_GLOBAL, global);
}

// The goal is to compile a binary operator found in an expression.
static void binary() {
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

static void literal() {
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

  prefixRule();

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule();
  }
}

// Compile unary expressions.
static void unary() {
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
static void number() {
  // `stdpod` convert a string to double.
  // `endptr` is left with unconsumed characters.
  // We assume that the token for the number literal has already been consumed and is stored in `previous`.
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void string() {
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

// Compile OR expressions.
//
// FIXME: Jumps.
static void or_() {
  parsePrecedence(PREC_OR);
}

static void variable() {
  // TODO
  printf("variable() %d\n", parser.current.type);
}

// Compile grouping expresions.
//
// Grouping expressions are called prefix expressions because we can determine the form of the expression simply by
// looking at the first token.
static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// Compile ternary expressions.
static void conditional() {
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
  } else {
    statement();
  }

  // We chose statements as the synchronization point in panic mode error recovery.
  if (parser.panicMode) synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else {
    expressionStatement();
  }
}

// Entry point of the compiler. It goes through the lexical analysis, syntactic analysis, and compilation
// to bytecode phases in the `Chunk` pointed to by `chunk`.
bool compile(const char* source, Chunk* chunk) {
  // Start the lazy lexical analysis.
  initScanner(source);

  compilingChunk = chunk;

  parser.hadError = false;

  // Used to prevent cascading errors.
  parser.panicMode = false;

  advance();

  while (!match(TOKEN_EOF)) {
    declaration();  
  }

  endCompiler();
  return !parser.hadError;
}