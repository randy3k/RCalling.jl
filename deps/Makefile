CC = gcc
CPPFLAGS = `R CMD config --cppflags`
CFLAGS = -g -Wall
LDFLAGS = `R CMD config --ldflags` -dynamiclib

SOURCES = $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)

all: $(OBJECTS) rcall.so

.c.o :
	$(CC) $(CFLAGS) $(CPPFLAGS) -fPIC -c $< -o $@

rcall.so: $(OBJECTS)
	$(CC) -shared $(OBJECTS) $(LDFLAGS) -o rcall.so

.PHONY : clean

clean:
	rm -f *.o *.so
