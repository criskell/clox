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

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();

    if (parser.current.type != TOKEN_ERROR) break;

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
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

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void endCompiler() {
  emitReturn();

#ifdef DEBUG_PRINT_CODE
  // We added this check because in syntax error mode, the compiler might continue to generate nonsensical code.
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  } 
#endif
}

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

// The goal is to compile a binary operator found in an expression.
static void binary() {
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
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
      break;
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
  emitConstant(value);
}

// Compile grouping expresions.
//
// Grouping expressions are called prefix expressions because we can determine the form of the expression simply by
// looking at the first token.
static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
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
  [TOKEN_BANG] = {NULL, NULL, PREC_NONE},
  [TOKEN_BANG_EQUAL] = {NULL, NULL, PREC_NONE},
  [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
  [TOKEN_EQUAL_EQUAL] = {NULL, NULL, PREC_NONE},
  [TOKEN_GREATER] = {NULL, NULL, PREC_NONE},
  [TOKEN_GREATER_EQUAL] = {NULL, NULL, PREC_NONE},
  [TOKEN_LESS] = {NULL, NULL, PREC_NONE},
  [TOKEN_LESS_EQUAL] = {NULL, NULL, PREC_NONE},
  [TOKEN_IDENTIFIER] = {NULL, NULL, PREC_NONE},
  [TOKEN_STRING] = {NULL, NULL, PREC_NONE},
  [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
  [TOKEN_AND] = {NULL, NULL, PREC_NONE},
  [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
  [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
  [TOKEN_FALSE] = {NULL, NULL, PREC_NONE},
  [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
  [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
  [TOKEN_IF] = {NULL, NULL, PREC_NONE},
  [TOKEN_NIL] = {NULL, NULL, PREC_NONE},
  [TOKEN_OR] = {NULL, NULL, PREC_NONE},
  [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
  [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
  [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
  [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
  [TOKEN_TRUE] = {NULL, NULL, PREC_NONE},
  [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
  [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
  [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
  [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
};

static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

// Compile expressions.
static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");

  endCompiler();
  return !parser.hadError;
}