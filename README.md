# clox

Lox implementation in C, created while reading CrafitingInterpreters.

## What I have praticed and learned

- C
- Lexical Analysis
- Parsing
  - Pratt parser
- Virtual Machines
  - Bytecode
  - Stack based
- Hash tables
  - Separate chaining
  - Open addressing / Closed hashing
  - Linear probing
  - Collision resolution
- String interning
- Benchmarking
- Debugging with GDB

## Running a test

Use makefile:

```bash
make
./clox tests/language/assignment.lox
```

Debugging:

```
gdb -ex run --args ./clox tests/language/local_vars_decl.lox
```
