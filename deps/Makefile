CC = gcc
CPPFLAGS = $(shell R CMD config --cppflags)
CPPFLAGS := $(CPPFLAGS)

CFLAGS = -g -O2 -std=gnu99 -Wall -I./uv/
CFLAGS += -I$(shell julia -e 'print(abspath(joinpath(JULIA_HOME,"..","include", "julia")))')
CFLAGS := $(CFLAGS)

LDFLAGS = $(shell R CMD config --ldflags) -dynamiclib -ljulia
LDFLAGS += -L$(shell julia -e 'print(dirname(Sys.dlpath(dlopen("libjulia"))))')
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
