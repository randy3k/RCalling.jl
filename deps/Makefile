CC = gcc
CPPFLAGS = $(shell R CMD config --cppflags) -I./uv/
CPPFLAGS +=	-I$(shell julia -e 'print(abspath(joinpath(JULIA_HOME,"..","include", "julia")))')
CPPFLAGS := $(CPPFLAGS)

CFLAGS = -g -Wall
LDFLAGS = $(shell R CMD config --ldflags) -dynamiclib -ljulia
LDFLAGS += -L$(shell julia -e 'print(abspath(joinpath(JULIA_HOME, "..", "lib", "julia")))')
LDFLAGS := $(LDFLAGS)

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
