SRCDIR = ../src
VPATH = $(SRCDIR)

WEXDIR = ../../wex
LKDIR = ../../lk
SSCDIR = ../../ssc

WEXLIB = $(WEXDIR)/wexosxwx3.a
LKLIB = $(LKDIR)/lkosxwx3.a
SSCLIB = $(SSCDIR)/build_osx/ssc.dylib

# Check environment variables are set
ifeq ($(WEXDIR),)
  $(error WEXLIB is not set)
endif
ifeq ($(LKDIR),)
  $(error LKDIR is not set)
endif
ifeq ($(SSCDIR),)
  $(error SSCDIR is not set)
endif

CC = gcc -mmacosx-version-min=10.9
CXX = g++ -mmacosx-version-min=10.9
CFLAGS = -g -DLK_USE_WXWIDGETS -I. -I$(SRCDIR) -I$(WEXDIR)/include -I$(LKDIR)/include -I$(SSCDIR) `wx-config-3 --cflags`  -DWX_PRECOMP -O2 -arch x86_64  -fno-common -DWX_PRECOMP -O2 -arch x86_64  -fno-common
CXXFLAGS = $(CFLAGS) -std=gnu++11
LDFLAGS =  -arch x86_64 `wx-config-3 --libs` `wx-config-3 --libs aui` `wx-config-3 --libs stc` `wx-config-3 --libs propgrid` `wx-config-3 --libs richtext` `wx-config-3 --libs webview` -lm $(LKLIB) $(WEXLIB) -lcurl

OBJECTS = ide.o \
	lossdiag.o \
	defmgr.o \
	urdb.o \
	macro.o \
	s3engine.o \
	s3objects.o \
	s3tool.o \
	s3view.o \
	stochastic.o \
	uncertainties.o \
	urdb.o \
	nsrdb.o \
	windtoolkit.o \
	gridsupport.o \
	macro.o \
	p50p90.o \
	reports.o \
	script.o \
	graph.o \
	excelexch.o \
	simulation.o \
	library.o \
	results.o \
	ipagelist.o \
	casewin.o \
	invoke.o \
	case.o \
	main.o \
	equations.o \
	inputpage.o \
	uiobjects.o \
	lossadj.o \
	widgets.o \
	variables.o \
	project.o \
	object.o \
	variablegrid.o \
	parametric.o \
	welcome.o \
	ptlayoutctrl.o \
	troughloop.o \
	shadingfactors.o \
	materials.o \
	codegenerator.o

TARGET = SAM


$(TARGET): $(OBJECTS) $(WEXLIB) $(LKLIB) $(SSCLIB)
	$(CXX) -g -o $@ $^ $(LDFLAGS)
	SetFile -a t $@
	mkdir -p $(TARGET).app
	mkdir -p $(TARGET).app/Contents
	mkdir -p $(TARGET).app/Contents/MacOS
	mkdir -p $(TARGET).app/Contents/Resources
	mkdir -p $(TARGET).app/Contents/runtime
	python copy_runtime.py
	cp ../Sandia/osx/lhs.bin $(TARGET).app/Contents/runtime/bin
	cp ../Sandia/osx/stepwise.bin $(TARGET).app/Contents/runtime/bin
	cp $(TARGET) ./$(TARGET).app/Contents/MacOS
	cp $(TARGET).icns ./$(TARGET).app/Contents/Resources
	cp Info-$(TARGET).plist ./$(TARGET).app/Contents/Info.plist
	echo "-n APPL????" >> ./$(TARGET).app/Contents/PkgInfo
	@mkdir -p $(TARGET).app/Contents/Frameworks
	cp $(SSCLIB) $(TARGET).app/Contents/Frameworks/ssc.dylib
	install_name_tool -id @executable_path/../Frameworks/ssc.dylib $(TARGET).app/Contents/Frameworks/ssc.dylib
	install_name_tool -change ssc.dylib @executable_path/../Frameworks/ssc.dylib $(TARGET).app/Contents/MacOS/$(TARGET)

bin: $(OBJECTS) $(WEXLIB) $(LKLIB) $(SSCLIB)
	$(CXX) -g -o $(TARGET) $^ $(LDFLAGS)
	SetFile -a t $(TARGET) 
	cp $(TARGET) ./$(TARGET).app/Contents/MacOS
	install_name_tool -id @executable_path/../Frameworks/ssc.dylib $(TARGET).app/Contents/Frameworks/ssc.dylib
	install_name_tool -change ssc.dylib @executable_path/../Frameworks/ssc.dylib $(TARGET).app/Contents/MacOS/$(TARGET)

DEPENDS = $(OBJECTS:.o=.d)

-include $(DEPENDS)

%.d: %.cpp
	$(CXX) -MM $(CXXFLAGS) $^ > $*.d
	@cp -f $*.d $*.d.tmp
	@sed -e 's/.*://' -e 's/\\$$//' < $*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $*.d
	@rm -f $*.d.tmp

clean:
	rm -rf $(OBJECTS) $(TARGET) $(TARGET).app $(DEPENDS)
