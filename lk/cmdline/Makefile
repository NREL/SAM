VPATH = ../src 

CC = gcc
CXX = g++
CFLAGS = -std=gnu++11 -I../include -Wall -O2 -g
CXXFLAGS = $(CFLAGS)

OBJECTS = \
	lkcmdline.o \
	absyn.o \
	codegen.o \
	env.o \
	eval.o \
	invoke.o \
	lex.o \
	parse.o \
	stdlib.o \
	vm.o



lk.exe: $(OBJECTS)
	g++  -o $@ $^ -std=gnu++11

clean:
	rm lk.exe