#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "table.h"
#include "value.h"
#include "mem.h"

#define NUM_KEYS 100000

void generateRandomKey(char *key, size_t length) {
  static const char alphabet[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

  for (size_t i = 0; i < length - 1; i++) {
    key[i] = alphabet[rand() % (sizeof(alphabet) - 1)];
  }

  key[length - 1] = '\0';
}

void benchmarkSequential() {
    Table table;
  initTable(&table);

  clock_t start = clock();

  for (int i = 0; i < NUM_KEYS; i++) {
    char key[16];
    sprintf(key, "key%d", i);

    Value value = NUMBER_VAL(i);
    tableSet(&table, copyString(key, 12), value);
  }

  clock_t end = clock();
  double timeTaken = (double)(end - start) / CLOCKS_PER_SEC;

  printf("Inserting sequential keys took %.4f seconds.\n", timeTaken);

  freeTable(&table);
}

void benchmarkRandom() {
  srand((unsigned int) time(NULL));

  Table table;
  initTable(&table);

  clock_t start = clock();

  for (int i = 0; i < NUM_KEYS; i++) {
    char key[16];

    generateRandomKey(key, 16);

    Value value = NUMBER_VAL(i);
    tableSet(&table, copyString(key, strlen(key)), value);
  }

  clock_t end = clock();

  double timeTaken = (double) (end - start) / CLOCKS_PER_SEC;

  printf("Random keys insertion took %.4f seconds.\n", timeTaken);
  freeTable(&table);
}

void benchmarkRandomWithKeyLookup() {
  srand((unsigned int) time(NULL));

  Table table;
  ObjString* keys[NUM_KEYS];
  
  for (int i = 0; i < NUM_KEYS; i++) {
    char key[16];
    generateRandomKey(key, 16);
    keys[i] = copyString(key, 16);
  }

  initTable(&table);

  for (int i = 0; i < NUM_KEYS; i++) {
    Value value = NUMBER_VAL(i);
    tableSet(&table, keys[i], value);
  }

  clock_t start = clock();

  for (int i = 0; i < NUM_KEYS; i++) {
    Value value;
    tableGet(&table, keys[i], &value);
  }

  clock_t end = clock();
  double timeTaken = (double) (end - start) / CLOCKS_PER_SEC;

  printf("Random keys lookup took %.4f seconds.\n", timeTaken);
  freeTable(&table);
}

void benchmarkSequentialLookup() {
  Table table;
  initTable(&table);

  ObjString* keys[NUM_KEYS];
  
  for (int i = 0; i < NUM_KEYS; i++) {
    char key[16];
    sprintf(key, "key%d", i);
    keys[i] = copyString(key, 16);
    
    Value value = NUMBER_VAL(i);
    tableSet(&table, keys[i], value);
  }

  clock_t start = clock();

  for (int i = 0; i < NUM_KEYS; i++) {
    Value value;
    tableGet(&table, keys[i], &value);

    if (AS_NUMBER(value) != i) {
      fprintf(stderr, "Expectation failed.");
      exit(1);
    }
  }
  
  clock_t end = clock();

  double timeTaken = (double)(end - start) / CLOCKS_PER_SEC;

  printf("Sequential lookup of sequential keys took %.4f seconds.\n", timeTaken);
  freeTable(&table);
}

void benchmarkDeletionWithReinsertion() {
  srand((unsigned int) time(NULL));

  Table table;
  ObjString* keys[NUM_KEYS];
  
  for (int i = 0; i < NUM_KEYS; i++) {
    char key[16];
    generateRandomKey(key, 16);
    keys[i] = copyString(key, 16);
  }

  initTable(&table);

  for (int i = 0; i < NUM_KEYS; i += 2) {
    Value value = NUMBER_VAL(i);
    tableSet(&table, keys[i], value);
  }

  clock_t start = clock();

  for (int i = 0; i < NUM_KEYS; i++) {
    tableDelete(&table, keys[i]);
    Value value;
    tableSet(&table, keys[i], value);
  }

  clock_t end = clock();
  double timeTaken = (double) (end - start) / CLOCKS_PER_SEC;

  printf("Deletion and reinsertion of half the keys took %.4f seconds.\n", timeTaken);
  freeTable(&table);
}

int main() {
  benchmarkSequential();
  benchmarkRandom();
  benchmarkSequentialLookup();
  benchmarkRandomWithKeyLookup();
  benchmarkDeletionWithReinsertion();

  return 0;
}