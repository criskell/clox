CC = gcc
COMMON_OBJ = chunk.o mem.o debug.o value.o vm.o compiler.o scanner.o object.o table.o
OBJ = main.o $(COMMON_OBJ)

clox: $(OBJ)
	$(CC) -o clox $(OBJ)

benchmark: benchmark.o $(COMMON_OBJ)
	$(CC) -o benchmark $^

%.o: %.c
	$(CC) -g -c $<

clean:
	rm -rf *.o clox