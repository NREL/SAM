SRCDIR = ../src
VPATH = $(SRCDIR)

WEXDIR = ../../wex
LKDIR = ../../lk
SSCDIR = ../../ssc

WEXLIB = $(WEXDIR)/wexosxwx3.a
LKLIB = $(LKDIR)/lkosxwx3.a
SSCLIB = $(SSCDIR)/mac_dylib/ssc.dylib

CC = gcc
CXX = g++
CFLAGS = -g -I. -I$(SRCDIR) -I$(WEXDIR)/include -I$(LKDIR) -I$(SSCDIR) `wx-config-3 --cflags`
CXXFLAGS = $(CFLAGS)
LDFLAGS = `wx-config-3 --libs` `wx-config-3 --libs aui` `wx-config-3 --libs stc` -lm $(LKLIB) $(WEXLIB) -lstdc++ -lcurl
OBJECTS = main.o \
	equation.o \
	uiview.o \
	uiwidgets.o \
	variable.o \
	project.o \
	object.o \
	simplecurl.o \
	welcome.o \
	wxcurlbase.o \
	wxcurlftp.o \
	wxcurlhttp.o \
	wxcurlthread.o

TARGET = SAM


$(TARGET): $(OBJECTS) $(WEXLIB) 
	$(CXX) -g -o $@ $^ $(LDFLAGS)
	SetFile -a t $@
	mkdir -p $(TARGET).app
	mkdir -p $(TARGET).app/Contents
	mkdir -p $(TARGET).app/Contents/MacOS
	mkdir -p $(TARGET).app/Contents/Resources
	cp $(TARGET) ./$(TARGET).app/Contents/MacOS
	cp $(TARGET).icns ./$(TARGET).app/Contents/Resources
	cp Info-$(TARGET).plist ./$(TARGET).app/Contents/Info.plist
	echo "-n APPL????" >> ./$(TARGET).app/Contents/PkgInfo
	@mkdir -p $(TARGET).app/Contents/Frameworks
	cp $(SSCLIB) $(TARGET).app/Contents/Frameworks
	install_name_tool -id @executable_path/../Frameworks/ssc.dylib $(TARGET).app/Contents/Frameworks/ssc.dylib
	install_name_tool -change ssc.dylib @executable_path/../Frameworks/ssc.dylib $(TARGET).app/Contents/MacOS/$(TARGET)
	
DEPENDS = $(OBJECTS:.o=.d)

-include $(DEPENDS)

%.d: %.cpp
	$(CXX) -MM $(CFLAGS) $^ > $*.d
	@cp -f $*.d $*.d.tmp
	@sed -e 's/.*://' -e 's/\\$$//' < $*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $*.d
	@rm -f $*.d.tmp

clean:
	rm -rf $(OBJECTS) $(DEPENDS) $(TARGET) $(TARGET).app
