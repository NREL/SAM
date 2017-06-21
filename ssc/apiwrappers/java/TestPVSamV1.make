# Because %SystemRoot% is the path to the windows directory is should be set on all Windows computers but not on Linux/MacOS


ifdef SystemRoot
	RM = del /Q
	CP = copy /y
	EXT = dll
	JNILIB = SSCAPIJNI.dll
	ifneq (,$(findstring 64, $(value PROCESSOR_IDENTIFIER)))
		BITS = 64
		CCCOMP = c:/MinGW64/bin/gcc.exe
#Update based on your Java installation location		
 		CIFLAGS = -Wl,--kill-at -I"C:\Program Files\Java\jdk1.7.0_71\include"  -I"C:\Program Files\Java\jdk1.7.0_71\include\win32"
		JC = C:\Program Files\Java\jdk1.7.0_71\bin\javac.exe
		JAR = C:\Program Files\Java\jdk1.7.0_71\bin\jar.exe
		JAVA = C:\Program Files\Java\jdk1.7.0_71\bin\java.exe
		
		CP_DLL = ..\..\Win64\ssc.dll ssc.dll
		SSCLIB = ssc.dll
	else
		BITS = 32
		CCCOMP = c:/MinGW/bin/gcc.exe
#Update based on your Java installation location		
		CIFLAGS = -Wl,--kill-at -I"C:\Program Files (x86)\Java\jdk1.7.0_71\include"  -I"C:\Program Files (x86)\Java\jdk1.7.0_71\include\win32"
		JC = C:\Program Files (x86)\Java\jdk1.7.0_71\bin\javac.exe
		JAR = C:\Program Files (x86)\Java\jdk1.7.0_71\bin\jar.exe
		JAVA = C:\Program Files (x86)\Java\jdk1.7.0_71\bin\java.exe

		CP_DLL = ..\..\Win32\ssc.dll ssc.dll
		SSCLIB = ssc.dll
	endif	
	CP_H = ..\..\sscapi.h sscapi.h
	CP_TM2 = ..\..\examples\*.tm2
	CP_CSV = ..\..\examples\*.csv
	RM_JAVA = *.class SSC\\*.class *.jar 
	RM_ALL = *.dll *.class SSC\\*.class SSC\\*.dll *.jar *.h  *.tm2 *.csv

else
    PF = $(shell uname)
    ifneq (,$(findstring Darwin, $(PF)))
        VERS = $(shell sw_vers -productVersion)
#Update based on your Java installation location
        ifneq (,$(findstring 10.8, $(VERS) ))
            CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers -I/System/Library/Frameworks/JavaVM.framework/Headers
            CP_DLL = ../../osx64/ssc.dylib ./ssc.dylib
	    SSCLIB = ssc.dylib
	    RM_ALL = *.dylib *.jnilib *.class ./SSC/*.class ./SSC/*.dylib *.jar *.h  *.tm2 *.csv
	    EXT = dylib
	    JNILIB = libSSCAPIJNI.jnilib
	else
            ifneq (,$(findstring 10.7, $(VERS) ))
                CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/Current/Headers -I/System/Library/Frameworks/JavaVM.framework/Headers
                CP_DLL = ../../osx64/ssc.dylib ./ssc.dylib
	        SSCLIB = ssc.dylib
	        RM_ALL = *.dylib *.jnilib *.class ./SSC/*.class ./SSC/*.dylib *.jar *.h  *.tm2 *.csv
	        EXT = dylib
	        JNILIB = libSSCAPIJNI.jnilib
	    else
                ifneq (,$(findstring 10.6, $(VERS)))
	            CIFLAGS = -I/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Headers -I/Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers
                    CP_DLL = ../../osx64/ssc.dylib ./ssc.dylib
                    SSCLIB = ssc.dylib
                    RM_ALL = *.dylib *.jnilib *.class ./SSC/*.class ./SSC/*.dylib *.jar *.h  *.tm2 *.csv
	            EXT = dylib
	            JNILIB = libSSCAPIJNI.jnilib
	        endif
	    endif
    	endif
    else 
        ifneq (,$findstring(Linux, $(PF)))
	    CIFLAGS = -I/usr/java/jdk1.7.0_71/include -I/usr/java/jdk1.7.0_71/include/linux -fPIC 
	    CP_DLL = ../../linux64/ssc.so ./ssc.so
	    SSCLIB = ./ssc.so
	    RM_ALL = *.so *.jnilib *.class ./SSC/*.class ./SSC/*.dylib *.jar *.h  *.tm2 *.csv
	    EXT = so
	    JNILIB = libSSCAPIJNI.so
	    LIBPATH = -Djava.library.path=.
    	endif
    endif
    RM = rm -f
    CP = cp -f
    CFLAGS += -D__64BIT__
    BITS = 64l
    JC = javac
    JAR = jar
    JAVA = java
    CCCOMP = gcc
    CP_H = ../../sscapi.h ./sscapi.h
    CP_TM2 = ../../examples/*.tm2 ./
    CP_CSV = ../../examples/*.csv ./
    RM_JAVA = *.class ./SSC/*.class *.jar 
endif


ifndef CP_DLL
    $(error Please check the settings for your system. Your system may not be supported. Please contact sam.support@nrel.gov. System: $(PF) $(VERS))
endif



JFLAGS = -g
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
	TestPVSamV1.java \
	SSC/SSCAPIJNI.java \
	SSC/API.java \
	SSC/Data.java \
	SSC/Module.java \
	SSC/Entry.java \
	SSC/Info.java 
    


run: all
	$(JAVA) $(LIBPATH) -jar TestPVSamV1.jar

all: jar


java_all:  java_classes

java_classes: $(CLASSES:.java=.class)


jar: java_classes dll
	$(JAR) cvfm TestPVSamV1.jar TestPVSamV1.Manifest.txt TestPVSamV1.class SSC/*.class		

java_clean:
	$(RM) $(RM_JAVA)


# $@ matches the target, $< matches the first dependancy
dll : 
	$(CP) $(CP_H)
	$(CP) $(CP_TM2)
	$(CP) $(CP_CSV)
	$(CP) $(CP_DLL)
	$(CCCOMP) -D_JNI_IMPLEMENTATION_  $(CIFLAGS) -shared sscapi_wrap.c -o $(JNILIB) $(SSCLIB)
	
	
clean :
	$(RM) $(RM_ALL)




help:
	@echo "Please check the settings for your system. Your system may not be supported. Please contact sam.support@nrel.gov. System: $(PF) $(VERS)"

