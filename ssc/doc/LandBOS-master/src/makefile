CC = gcc
CXX = clang++
CLAGS = -Wall

all: maintest.cpp LandBOS.o
	$(CXX) $(CFLAGS) -o test maintest.cpp LandBOS.cpp LandBOS.o

LandBOS.o: LandBOS.c
	$(CC) -c $<

clean:
	rm -rf test *.o