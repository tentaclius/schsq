BIN = schsq
CC = gcc
CFLAGS += $(shell pkg-config --cflags glib-2.0) \
			 $(shell pkg-config --cflags guile-3.0) \
			 $(shell pkg-config --cflags alsa) \
			 -Wall -g
LDFLAGS += $(shell pkg-config --libs glib-2.0) \
			  $(shell pkg-config --libs guile-3.0) \
			  $(shell pkg-config --libs alsa) \
			  -lpthread


$(BIN): scheduler.o main.c guile.o midi.c
	$(CC) $(CFLAGS) $(LDFLAGS) main.c scheduler.o guile.o -o $(BIN)

$(BIN).so: scheduler.c scheduler.h guile.c midi.c
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $(BIN).so -fPIC scheduler.c guile.c midi.c

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clear: clean
	rm -f $(BIN) $(BIN).so

clean:
	rm -f *.o *.yy.c *.cc.c

re: clear $(BIN)
