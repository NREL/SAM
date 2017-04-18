VPATH = ../src
CC = gcc
CXX = g++
CFLAGS = -I../include -fPIC -Wall -O2 -DLK_USE_WXWIDGETS `wx-config-3 --cflags`
CXXFLAGS = -std=c++0x $(CFLAGS)
#CXXFLAGS= -std=c++11 $(CFLAGS)
#OBJCXXFLAGS=-stdlib=libc++ -std=c++11
#LDFLAGS=-stdlib=libc++

LIBNAME = lkuxwx3
TARGET = ../$(LIBNAME).a

OBJECTS = \
	absyn.o \
	codegen.o \
	vm.o \
	env.o \
	eval.o \
	invoke.o \
	lex.o \
	parse.o \
	sqlite3.o \
	stdlib.o

$(TARGET) : $(OBJECTS)
	ar rs $@ $^ 

clean:
	rm -rf *.o $(TARGET)

