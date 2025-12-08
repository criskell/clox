CC = gcc
OBJ = main.o chunk.o mem.o debug.o value.o vm.o compiler.o scanner.o object.o

clox: $(OBJ)
	$(CC) -o clox $(OBJ)

%.o: %.c
	$(CC) -g -c $<

clean:
	rm -rf *.o clox