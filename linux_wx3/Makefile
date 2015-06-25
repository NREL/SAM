SAMVER=2015.6.30
SAMVERDASH=$(subst .,-,$(SAMVER))
SRCDIR = ../src
VPATH = $(SRCDIR)

WEXDIR = ../../wex
LKDIR = ../../lk
SSCDIR = ../../ssc

WEXLIB = $(WEXDIR)/wexuxwx3.a
LKLIB = $(LKDIR)/lkuxwx3.a
SSCLIB = ssc.so

CC = gcc
CXX = g++
CFLAGS = -g -DLK_USE_WXWIDGETS -I. -I$(SRCDIR) -I$(WEXDIR)/include -I$(LKDIR) -I$(SSCDIR) `wx-config-3 --cflags`
CXXFLAGS = $(CFLAGS)
LDFLAGS = `wx-config-3 --libs propgrid` `wx-config-3 --libs aui` `wx-config-3 --libs stc` `wx-config-3 --libs richtext` `wx-config-3 --libs` -lm $(LKLIB) $(WEXLIB) -lstdc++ -lcurl

OBJECTS = ide.o \
	lossdiag.o \
	defmgr.o \
	urdb.o \
	macro.o \
	s3clipper.o \
	s3engine.o \
	s3objects.o \
	s3tool.o \
	s3view.o \
	stochastic.o \
	urdb.o \
	solarprospector.o \
	windtoolkit.o \
	registration.o \
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
	simplecurl.o \
	variablegrid.o \
	parametric.o \
	welcome.o \
	ptlayoutctrl.o \
	troughloop.o \
	shadingfactors.o \
	materials.o

TARGET = sam.bin


$(TARGET): $(OBJECTS) $(WEXLIB) $(LKLIB) $(SSCLIB)
	$(CXX) -g -o $@ $^ $(LDFLAGS)
	mkdir -p ../deploy/linux_64
	cp $(TARGET) ../deploy/linux_64
	cp ../../ssc/build_linux/$(SSCLIB) ../deploy/linux_64
	cp ../../lhs/src/lhs.bin ../deploy/runtime/bin
	cp ../../stepwise/stepwise.bin ../deploy/runtime/bin
	make -f Makefile-webupd
	cp webupd.bin ../deploy/linux_64

PKGFILE = sam-linux64-$(SAMVERDASH)-install.run


package:
	rm -rf ./sam.deploy sam.deploy.tar.gz
	cp -Rv ../deploy ./sam.deploy
	echo "cleaning up install package folder..."
	rm -rf ./sam.deploy/win32
	rm -rf ./sam.deploy/x64
	rm -rfv ./sam.deploy/linux_64/SAM
	rm -rfv ./sam.deploy/linux_64/SAM~
	rm -rfv `find ./sam.deploy -type d -name .svn`
	rm -rfv `find ./sam.deploy -type f -name "*.iss"`
	rm -rfv `find ./sam.deploy -type f -name "*.exe"`
	echo "stripping binaries..."
	strip -v ./sam.deploy/linux_64/sam.bin
	strip -v ./sam.deploy/linux_64/webupd.bin
	strip -v ./sam.deploy/linux_64/ssc.so
	strip -v ./sam.deploy/runtime/bin/lhs.bin
	strip -v ./sam.deploy/runtime/bin/stepwise.bin
	echo "assembling archive..."
	tar cvzf sam.deploy.tar.gz ./sam.deploy
	make installer

installer:
	echo "#!/bin/sh" > install0
	echo "SAMVER=$(SAMVER)" >> install0
	cat install0 install.sh sam.deploy.tar.gz > $(PKGFILE) 
	chmod a+x $(PKGFILE)
	
DEPENDS = $(OBJECTS:.o=.d)

-include $(DEPENDS)

%.d: %.cpp
	$(CXX) -MM $(CFLAGS) $^ > $*.d
	@cp -f $*.d $*.d.tmp
	@sed -e 's/.*://' -e 's/\\$$//' < $*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $*.d
	@rm -f $*.d.tmp

clean:
	rm -rf $(OBJECTS) $(TARGET) *.d
