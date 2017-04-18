Windows - to build c examples using MinGW http://www.mingw.org/

ver.c:

From command line in the examples folder, issue (to build):
g++ -I..  -o ../win32/ver.exe ver.c -L../win32 -lssc
Then to run:
../Win32/ver.exe

pvwatts1ts_ex.c:

From command line in the examples folder, issue (to build):
g++ -I..  -o ../win32/pvwatts1ts_ex.exe pvwatts1ts_ex.c -L../win32 -lssc
Then to run:
../Win32/pvwatts1ts_ex.exe

pvwattsv5run.c:

From command line in the examples folder, issue (to build):
g++ -I..  -o ../win32/pvwattsv5run.exe pvwattsv5run.c -L../win32 -lssc
Then to run:
..\win32\pvwattsv5run.exe -wf=daggett.tm2
