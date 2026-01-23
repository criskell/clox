#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
  // It marks the beginning of the lexeme.
  const char* start;

  // Marks the current character being scanned.
  const char* current;

  int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}

static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    c == '_';
}

static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}

static bool isAtEnd() {
  return *scanner.current == '\0';
}

static char advance() {
  scanner.current++;
  return scanner.current[-1];
}

static char peek() {
  return *scanner.current;
}

static char peekNext() {
  if (isAtEnd()) return '\0';
  // *(scanner.current + 1)
  return scanner.current[1];
}

static bool match(char expected) {
  if (isAtEnd()) return false;
  if (*scanner.current != expected) return false;
  scanner.current++;
  return true;
}

static Token makeToken(TokenType type) {
  Token token;

  token.type = type;
  token.start = scanner.start;
  token.length = (int) (scanner.current - scanner.start);
  token.line = scanner.line;

  return token;
}

// We must ensure that the compiler outlives the message.
// We typically call this function using C string literals that are constant and eternal.
static Token errorToken(const char* message) {
  Token token;

  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = (int) strlen(message);
  token.line = scanner.line;

  return token;
}

static void skipWhitespace() {
  for (;;) {
    char c = peek();

    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;
      case '\n':
        scanner.line++;
        advance();
        break;
      case '/':
        // We need a second lookahead character.
        if (peekNext() == '/') {
          // Do not consume "\n". Let the code that handles that character consume it.
          while (peek() != '\n' && !isAtEnd()) advance();
        } else {
          // It will not consume the first bar if it does not find the second one.
          return;
        }
        break;
      default:
        // Not white-space character
        return;
    }
  }
}

// start -> Index within the lexama where the comparison begins.
// length -> Length of the remainder of the keyword to be compared.
// rest -> Rest of the keyword.
// The type of token to be returned if it is indeed a keyword.
static TokenType checkKeyword(int start, int length, const char* rest, TokenType type) {
  // Conditions:
  // - Checks if the total length of the current lexame matches the expected length of the keyword.
  // - Compare the rest of the lexeme with the expected rest, checking if they are the same.
  if (scanner.current - scanner.start == start + length && memcmp(scanner.start + start, rest, length) == 0) {
    return type;
  }

  return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
  // Coding a trie (DFA) for keyword recognition.
  switch (scanner.start[0]) { // checking from start = 0
    case 'a':
      // checking from start = 1
      return checkKeyword(1, 2, "nd", TOKEN_AND);
    case 'c':
      return checkKeyword(1, 4, "lass", TOKEN_CLASS);
    case 'e':
      return checkKeyword(1, 3, "lse", TOKEN_ELSE);
    case 'f':
      // Check if there is a second letter.
      if (scanner.current - scanner.start > 1) {
        // Check second letter now.
        // checking from start = 1
        switch (scanner.start[1]) {
          case 'a':
            // checking from start = 2
            return checkKeyword(2, 3, "lse", TOKEN_FALSE);
          case 'u':
            return checkKeyword(2, 1, "n", TOKEN_FUN);
          case 'i':
            return checkKeyword(2, 3, "nal", TOKEN_FINAL);
          case 'o':
            return checkKeyword(2, 1, "r", TOKEN_FOR);
        }
      }

      break;
    case 'i':
      return checkKeyword(1, 1, "f", TOKEN_IF);
    case 'n':
      return checkKeyword(1, 2, "il", TOKEN_NIL);
    case 'o':
      return checkKeyword(1, 1, "r", TOKEN_OR);
    case 'p':
      return checkKeyword(1, 4, "rint", TOKEN_PRINT);
    case 'r':
      return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
    case 's':
      return checkKeyword(1, 4, "uper", TOKEN_SUPER);
    case 't':
      if (scanner.current - scanner.start > 1) {
        switch (scanner.start[1]) {
          case 'h':
            return checkKeyword(2, 2, "is", TOKEN_THIS);
          case 'r':
            return checkKeyword(2, 2, "ue", TOKEN_TRUE);
        }
      }

      break;
    case 'v':
      return checkKeyword(1, 2, "ar", TOKEN_VAR);
    case 'w':
      return checkKeyword(1, 4, "hile", TOKEN_WHILE);
  }

  return TOKEN_IDENTIFIER;
}

static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek())) advance();

  return makeToken(identifierType());
}

static Token number() {
  while (isDigit(peek())) advance();

  if (peek() == '.' && isDigit(peekNext())) {
    advance(); // Consume "."

    while (isDigit(peek())) advance();
  }

  return makeToken(TOKEN_NUMBER);
}

static Token string() {
  // Support multi-line strings.
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') scanner.line++;
    advance();
  }

  if (isAtEnd()) return errorToken("Unterminated string.");

  advance(); // Consume closing quote.
  return makeToken(TOKEN_STRING);
}

Token scanToken() {
  skipWhitespace();

  scanner.start = scanner.current;

  if (isAtEnd()) return makeToken(TOKEN_EOF);

  char c = advance();

  if (isAlpha(c)) return identifier();
  if (isDigit(c)) return number();

  switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);
    case '?': return makeToken(TOKEN_QUESTION);
    case ':': return makeToken(TOKEN_COLON);
    case '|':
      if (!match('|')) {
        return errorToken("Expected '|'.");
      }

      return makeToken(TOKEN_OR);
    case '!':
      return makeToken(
        match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG
      );
    case '=':
      return makeToken(
        match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL
      );
    case '<':
      return makeToken(
        match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS
      );
    case '>':
      return makeToken(
        match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER
      );
    case '"':
      return string();
  }

  return errorToken("Unexpected character.");
}