#ifndef clox_common_h
#define clox_common_h

// Define NULL and sizes linked to sizes and offset. `size_t` means the type for object sizes.
#include <stddef.h>
// Defines the logical type bool and the values ​​true and false.
#include <stdbool.h>
// Defines integer types with exact size. For example, int8_t, uint8_t, int16_t, etc.
#include <stdint.h>

#define DEBUG_PRINT_CODE
// #define DEBUG_PRINT_TOKENS
#define DEBUG_TRACE_EXECUTION

#define UINT8_COUNT (UINT8_MAX + 1)

#endif