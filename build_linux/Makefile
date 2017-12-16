SAMVER=Open.Source
SAMVERDASH=$(subst .,-,$(SAMVER))
SRCDIR = ../src
VPATH = $(SRCDIR)

WEXLIB = $(WEXDIR)/wexuxwx3.a
LKLIB = $(LKDIR)/lkuxwx3.a
SSCLIB = ssc.so

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

CC = gcc
CXX = g++
CFLAGS = -g -DLK_USE_WXWIDGETS -I. -I$(SRCDIR) -I$(WEXDIR)/include -I$(LKDIR)/include -I$(SSCDIR) `wx-config-3 --cflags` -Wall -Wno-unknown-pragmas -Werror
LDFLAGS = -std=c++11 $(SSCLIB) $(LKLIB) $(WEXLIB) `wx-config-3 --libs propgrid` `wx-config-3 --libs aui` `wx-config-3 --libs stc` `wx-config-3 --libs richtext` `wx-config-3 --libs` -lm -lstdc++ -lcurl
CXXFLAGS=-std=c++11 $(CFLAGS)


OBJECTS = ide.o \
	lossdiag.o \
	defmgr.o \
	s3engine.o \
	s3objects.o \
	s3tool.o \
	s3view.o \
	stochastic.o \
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

TARGET = sam.bin


$(TARGET): $(OBJECTS) $(WEXLIB) $(LKLIB) $(SSCLIB)
	$(CXX) -g -o $@ $(OBJECTS) $(LDFLAGS)
	mkdir -p ../deploy/linux_64
	cp $(TARGET) ../deploy/linux_64
	cp $(SSCDIR)/build_linux/$(SSCLIB) ../deploy/linux_64
	cp ../Sandia/linux/lhs.bin ../deploy/runtime/bin
	cp ../Sandia/linux/stepwise.bin ../deploy/runtime/bin
	rm -rf ../deploy/linux_64/GtkTheme
	cp -R ./GtkTheme ../deploy/linux_64

$(SSCLIB): $(SSCDIR)/build_linux/$(SSCLIB)
	cp $< $@

PKGFILE = sam-linux-$(SAMVERDASH).run


package:
	rm -rf ./sam.deploy sam.deploy.tar.gz
	cp -Rv ../deploy ./sam.deploy
	echo "cleaning up install package folder..."
	rm -rf ./sam.deploy/win32
	rm -rf ./sam.deploy/x64
	rm -rfv ./sam.deploy/linux_64/SAM
	rm -rfv ./sam.deploy/linux_64/SAM~
# Following leaves .svn folders in directories with spaces in names.	
#	rm -rfv `find ./sam.deploy -type d -name .svn`
	rm -rfv `find ./sam.deploy -type f -name "*.iss"`
	rm -rfv `find ./sam.deploy -type f -name "*.exe"`
	find ./sam.deploy  -name .svn -exec rm -rf {} \; 2> /dev/null; true
	echo "stripping binaries..."
# for now, don't strip debugging information even though
# the binaries are much bigger, because backtraces from crashes 
# will be empty
#
#	strip -v ./sam.deploy/linux_64/sam.bin
#	strip -v ./sam.deploy/linux_64/webupd.bin
#	strip -v ./sam.deploy/linux_64/ssc.so
#	strip -v ./sam.deploy/runtime/bin/lhs.bin
#	strip -v ./sam.deploy/runtime/bin/stepwise.bin
	echo "assembling archive..."
	tar cvzf sam.deploy.tar.gz ./sam.deploy
	make installer

installer:
	echo "#!/bin/sh" > install0
	echo "SAMVER=$(SAMVER)" >> install0
	cat install0 install.sh sam.deploy.tar.gz > $(PKGFILE) 
	chmod a+x $(PKGFILE)
	./$(PKGFILE)

DEPENDS = $(OBJECTS:.o=.d)

-include $(DEPENDS)

%.d: %.cpp
	$(CXX) -MM $(CXXFLAGS) $^ > $*.d
	@cp -f $*.d $*.d.tmp
	@sed -e 's/.*://' -e 's/\\$$//' < $*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $*.d
	@rm -f $*.d.tmp

clean:
	-rm -rf $(OBJECTS) $(SSCLIB) $(TARGET) *.d
	-rm -rf ./sam.deploy sam.deploy.tar.gz
	-rm -rf ../deploy/linux_64/GtkTheme
	-rm install0
