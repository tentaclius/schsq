BIN = schsq
TARGET = $(BIN).so
CC = gcc
CFLAGS += $(shell pkg-config --cflags glib-2.0) \
			 $(shell pkg-config --cflags guile-3.0) \
			 $(shell pkg-config --cflags alsa) \
			 $(shell pkg-config --cflags liblo) \
			 -Wall -g
LDFLAGS += $(shell pkg-config --libs glib-2.0) \
			  $(shell pkg-config --libs guile-3.0) \
			  $(shell pkg-config --libs alsa) \
			  $(shell pkg-config --libs liblo) \
			  -lpthread

CLIST = scheduler.c scheduler.h guile.c midi.c osc.c

$(BIN).so: $(CLIST)
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $(BIN).so -fPIC $(CLIST)

$(BIN): scheduler.o main.c guile.o midi.c
	$(CC) $(CFLAGS) $(LDFLAGS) main.c scheduler.o guile.o -o $(BIN)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $< -o $@

clear: clean
	rm -f $(BIN) $(TARGET)

clean:
	rm -f *.o *.yy.c *.cc.c

re: clear $(TARGET)
