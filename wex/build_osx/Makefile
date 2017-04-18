VPATH = ../src/freetype/src/base ../src/freetype/src/cff ../src/freetype/src/cid ../src/freetype/src/psaux ../src/freetype/src/pshinter ../src/freetype/src/psnames ../src/freetype/src/raster ../src/freetype/src/sfnt ../src/freetype/src/smooth ../src/freetype/src/truetype ../src/freetype/src/type1 ../src/plot/qhull ../src/dview ../src/plot ../src/pdf ../src/clipper ../src

LKDIR = ../../lk
LKLIB = lkosxwx3.a

CC = gcc -mmacosx-version-min=10.9
CXX = g++ -mmacosx-version-min=10.9
CFLAGS = -fPIC -Wall -O2 `wx-config-3 --cflags` -I../include -I../src/freetype/include -DFT2_BUILD_LIBRARY -DOPJ_STATIC -DFT_CONFIG_MODULES_H=\"slimftmodules.h\" -DFT_CONFIG_OPTIONS_H=\"slimftoptions.h\" -I$(LKDIR)/include  -DWX_PRECOMP -O2 -arch x86_64  -fno-common
CXXFLAGS = $(CFLAGS) -std=gnu++11
LDFLAGS =  -arch x86_64  -framework IOKit -framework Carbon -framework Cocoa -framework AudioToolbox -framework System -framework OpenGL 

LIBNAME = wexosxwx3
TARGET = ../$(LIBNAME).a
	
OBJECTS = \
	ftbase.o \
	ftbbox.o \
	ftbitmap.o \
	ftfntfmt.o \
	ftgasp.o \
	ftglyph.o \
	ftinit.o \
	ftlcdfil.o \
	ftstroke.o \
	ftsynth.o \
	ftsystem.o \
	fttype1.o \
	cff.o \
	type1cid.o \
	psaux.o \
	pshinter.o \
	psnames.o \
	raster.o \
	sfnt.o \
	smooth.o \
	truetype.o \
	type1.o \
	clipper.o\
	gleasy.o \
	easycurl.o \
	snaplay.o \
	pagelayout.o \
	pageobjects.o \
	csv.o \
	exttree.o \
	label.o \
	exttext.o \
	radiochoice.o \
	metro.o \
	uiform.o \
	registration.o \
	tpdlg.o \
	diurnal.o \
	utils.o \
	lkscript.o \
	codeedit.o \
	extgrid.o \
	jsonreader.o \
	jsonval.o \
	jsonwriter.o \
	md5.o \
	numeric.o \
	mplcontour.o \
	plannotation.o \
	plsectorplot.o \
	plcontourplot.o \
	dvstatisticstablectrl.o \
	dvautocolourassigner.o \
	pltext.o \
	plcolourmap.o \
	dvdcctrl.o \
	dvdmapctrl.o \
	dvfilereader.o \
	dvplotctrl.o \
	dvplotctrlsettings.o \
	dvplothelper.o \
	dvpncdfctrl.o \
	dvprofilectrl.o \
	dvscatterplotctrl.o \
	dvselectionlist.o \
	dvtimeseriesctrl.o \
	dvtimeseriesdataset.o \
	ploutdev.o \
	plplot.o \
	plaxis.o \
	plbarplot.o \
	plhistplot.o \
	pllineplot.o \
	plplotctrl.o \
	plscatterplot.o \
	plwindrose.o \
	pdfannotation.o \
	pdfbarcode.o \
	pdfcffdecoder.o \
	pdfcffindex.o \
	pdfcolour.o \
	pdfdc.o \
	pdfdecode.o \
	pdfdocument.o \
	pdfencoding.o \
	pdfencrypt.o \
	pdffont.o \
	pdffontdata.o \
	pdffontdatacore.o \
	pdffontdataopentype.o \
	pdffontdatatruetype.o \
	pdffontdatatype0.o \
	pdffontdatatype1.o \
	pdffontdescription.o \
	pdffontdetails.o \
	pdffontextended.o \
	pdffontmanager.o \
	pdffontparser.o \
	pdffontparsertruetype.o \
	pdffontparsertype1.o \
	pdffontsubsetcff.o \
	pdffontsubsettruetype.o \
	pdffontvolt.o \
	pdfform.o \
	pdfgradient.o \
	pdfgraphics.o \
	pdfimage.o \
	pdfkernel.o \
	pdflayer.o \
	pdfobjects.o \
	pdfocg.o \
	pdfparser.o \
	pdfpattern.o \
	pdfprint.o \
	pdfrijndael.o \
	pdftemplate.o \
	pdfutility.o \
	pdfxml.o \
	geom.o \
	geom2.o \
	global.o \
	io.o \
	libqhull.o \
	mem.o \
	merge.o \
	poly.o \
	poly2.o \
	qset.o \
	random.o \
	rboxlib.o \
	stat.o \
	user.o \
	usermem.o \
	userprintf.o \
	userprintf_rbox.o


$(TARGET) : $(OBJECTS)
	ar rs $@ $^

DEPENDS = $(OBJECTS:.o=.d)

-include $(DEPENDS)

%.d: %.cpp
	$(CXX) -MM $(CXXFLAGS) $^ > $*.d
	@cp -f $*.d $*.d.tmp
	@sed -e 's/.*://' -e 's/\\$$//' < $*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> $*.d
	@rm -f $*.d.tmp

clean:
	rm -rf $(OBJECTS) $(DEPENDS) $(TARGET)

