# OS building
MYARCH=darwin-x86_64

# Default NDK installation location for Android Studio 2.3
NDK = /Users/imacuser/Library/Android/sdk/ndk-bundle

# based on Android-23 for Samsung Galaxy 5
PLATFORMVER=android-13
GCCPREFIX=arm-linux-androideabi
GCCVER=4.9
#ARCH=armeabi-v7a

CC=${NDK}/toolchains/${GCCPREFIX}-${GCCVER}/prebuilt/${MYARCH}/bin/${GCCPREFIX}-gcc
CXX=${NDK}/toolchains/${GCCPREFIX}-${GCCVER}/prebuilt/${MYARCH}/bin/${GCCPREFIX}-g++
RANLIB=${NDK}/toolchains/${GCCPREFIX}-${GCCVER}/prebuilt/${MYARCH}/bin/${GCCPREFIX}-ranlib
AR=${NDK}/toolchains/${GCCPREFIX}-${GCCVER}/prebuilt/${MYARCH}/bin/${GCCPREFIX}-ar
ARCH=armeabi-v7a


# mixing architectures so remove object files before archiving
all:
#	make -f Makefile-shared -j8 ARCH=x86_64
#	rm *.o
	make -f Makefile-shared -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-shared -j8 ARCH=armeabi
	rm *.o
#	make -f Makefile-nlopt -j8 ARCH=x86_64
#	rm *.o
	make -f Makefile-nlopt -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-nlopt -j8 ARCH=armeabi
	rm *.o
#	make -f Makefile-lpsolve -j8 ARCH=x86_64
#	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-lpsolve -j8 ARCH=armeabi
	rm *.o
#	make -f Makefile-solarpilot -j8 ARCH=x86_64
#	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-solarpilot -j8 ARCH=armeabi
	rm *.o
#	make -f Makefile-tcs -j8 ARCH=x86_64
#	rm *.o
	make -f Makefile-tcs -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-tcs -j8 ARCH=armeabi
	rm *.o
#	make -f Makefile-ssc -j8 ARCH=x86_64
#	rm *.o
	make -f Makefile-ssc -j8 ARCH=armeabi-v7a
	rm *.o
	make -f Makefile-ssc -j8 ARCH=armeabi
	rm *.o

clean:
	make -f Makefile-shared clean
	make -f Makefile-nlopt clean
	make -f Makefile-lpsolve clean
	make -f Makefile-solarpilot clean
	make -f Makefile-tcs clean
	make -f Makefile-ssc clean

libraries:
#	lipo -create ssc_armv7s.a ssc_x86_64.a ssc_arm64.a -output ssc_ios.a
#	lipo -create ssc_armv7s.dylib ssc_x86_64.dylib ssc_arm64.dylib -output ssc_ios.dylib


	$(AR) -xv shared_$(ARCH).a
	$(AR) -xv nlopt_$(ARCH).a
	$(AR) -xv lpsolve_$(ARCH).a
	$(AR) -xv solarpilot_$(ARCH).a
	$(AR) -xv tcs_$(ARCH).a
	$(AR) -xv ssc_$(ARCH).a
	$(AR) -rc ssc$(ARCH).a *.o	
	rm *.o
#	lipo -create shared_armeabi-v7a.a shared_x86_64.a shared_armeabi.a -output shared.a	
#	lipo -create nlopt_armeabi-v7a.a nlopt_x86_64.a nlopt_armeabi.a -output nlopt.a
#	lipo -create lpsolve_armeabi-v7a.a lpsolve_x86_64.a lpsolve_armeabi.a -output lpsolve.a
#	lipo -create solarpilot_armeabi-v7a.a solarpilot_x86_64.a solarpilot_armeabi.a -output solarpilot.a
#	lipo -create tcs_armeabi-v7a.a tcs_x86_64.a tcs_armeabi.a -output tcs.a
#	lipo -create ssc_armeabi-v7a.a  ssc_x86_64.a ssc_armeabi.a -output ssc.a

#	libtool -o libssc.a shared.a nlopt.a lpsolve.a solarpilot.a tcs.a ssc.a