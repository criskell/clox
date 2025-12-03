CC = gcc
OBJ = main.o chunk.o

clox: $(OBJ)
	$(CC) -o clox $(OBJ)

%.o: %.c
	$(CC) -c $<

clean:
	rm -rf *.o clox