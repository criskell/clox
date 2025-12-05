#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

void compile(const char* source) {
  initScanner(source);

  int line = -1;

  for (;;) {
    Token token = scanToken();

    if (token.line != line) {
      printf("%4d ", token.line);
      line = token.line;
    } else {
      printf("   | ");
    }

    // "*" specifies that the precision is given in token.length. That is, it takes the `token.length` characters from `token.start`.
    // This is necessary because the lexeme does not include a NUL byte at the end.
    printf("%2d '%.*s'\n", token.type, token.length, token.start);

    if (token.type == TOKEN_EOF) break;
  }
}