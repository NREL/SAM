VPATH = ../src 

CC = gcc
CXX = g++
CFLAGS = -std=gnu++11 -I../include -Wall -O2 -g
CXXFLAGS = $(CFLAGS)

OBJECTS = \
	make_stdlib_docs.o \
	absyn.o \
	codegen.o \
	env.o \
	eval.o \
	invoke.o \
	lex.o \
	parse.o \
	stdlib.o \
	vm.o



lkdocs.exe: $(OBJECTS)
	g++   -o $@ $^ -std=gnu++11

clean:
	rm $(OBJECTS) lkdocs.exe