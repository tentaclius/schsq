BIN = schsq
CC = gcc
CFLAGS += $(shell pkg-config --cflags glib-2.0) -Wall -g
LDFLAGS += $(shell pkg-config --libs glib-2.0) -lpthread


$(BIN): scheduler.o main.c
	$(CC) $(CFLAGS) $(LDFLAGS) main.c scheduler.o -o $(BIN)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clear: clean
	rm -f $(BIN)

clean:
	rm -f *.o *.yy.c *.cc.c

re: clear $(BIN)
