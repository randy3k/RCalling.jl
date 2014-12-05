R_HOME := $(shell R RHOME)
CC = gcc
CPPFLAGS = $(shell R CMD config --cppflags)
CPPFLAGS := $(CPPFLAGS)

CFLAGS = -g -O2 -std=gnu99 -Wall -I./uv/
CFLAGS += -I$(shell julia -e 'print(abspath(joinpath(JULIA_HOME,"..","include", "julia")))')
CFLAGS := $(CFLAGS)

RRPATH := -Wl,-rpath,$(R_HOME)/lib
RBLAS := $(shell R CMD config BLAS_LIBS)
RLAPACK := $(shell R CMD config LAPACK_LIBS)

LDFLAGS = $(shell R CMD config --ldflags) -ljulia
LDFLAGS += -L$(shell julia -e 'print(dirname(Sys.dlpath(dlopen("libjulia"))))')
LDFLAGS := $(LDFLAGS) $(RRPATH) $(RBLAS) $(RLAPACK)

SOURCES = $(wildcard *.c)
OBJECTS = $(SOURCES:.c=.o)

all: $(OBJECTS) librcall.so

.c.o :
	$(CC) $(CFLAGS) $(CPPFLAGS) -fPIC -c $< -o $@

librcall.so: $(OBJECTS)
	$(CC) -shared $(OBJECTS) $(LDFLAGS) -o librcall.so

.PHONY : clean

clean:
	rm -f *.o *.so
