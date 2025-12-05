CC = gcc
OBJ = main.o chunk.o mem.o debug.o value.o

clox: $(OBJ)
	$(CC) -o clox $(OBJ)

%.o: %.c
	$(CC) -c $<

clean:
	rm -rf *.o clox