Requirements for ssc sdk Java JNI interface and examples:

Windows 
32bit:
1. mingw32 http://sourceforge.net/projects/mingw/files/
2. install c, c++ and development tools

64bit
1. 32bit requirements as described above
2. mingw64 install http://sourceforge.net/projects/mingw-w64/files/Toolchains targetting Win64/Personal Builds/rubenvb/gcc-4.7-release/x86_64-w64-mingw32-gcc-4.7.2-release-win64_rubenvb.7z

All Platforms
Appropriate JDK from Oracle Java SE 
http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html

Note: The ssc sdk examples and Makefile built against JDK version 1.7.0_71
Different versions may require updating the CIFLAGS variable in the Makefile for your installation

To build the SSC java library and to run the examples
1. On Windows type:                                 mingw32-make
2. On OSX (10.6, 10.7 and 10.8) type:               make
3. On Linux (developed on CentOS 6.3 x64) type:     make