CFLAGS=-O2
LFLAGS=-static-libgfortran -static-libgcc -static-libstdc++ -o
IVFC=gfortran
# Because %SystemRoot% is the path to the windows directory is should be set on all Windows computers but not on Linux/MacOS
ifdef SystemRoot
	TARGET=lhs.exe
	CP_TR_1=..\..\SAMnt\deploy\runtime\bin
	CP_TR_2=..\..\SAMnt\deploy\runtime\bin
	CP_BR_1=..\..\..\2015.1.30\SAMnt\deploy\runtime\bin
	CP_BR_2=..\..\..\2015.1.30\SAMnt\deploy\runtime\bin
	CP=copy
	RM=del /q
else
	TARGET=lhs.bin
	CP_TR_1=../../SAMnt/osx_wx3
	CP_TR_2=../../SAMnt/osx_wx3/SAM.app/Contents/runtime/bin
	CP_BR_1=../../../2015.1.30/SAMnt/osx_wx3
	CP_BR_2=../../../2015.1.30/SAMnt/osx_wx3/SAM.app/Contents/runtime/bin
	CP=cp
	RM=rm -f
endif
OBJ=\
	KILLFILE.o\
	Parms.o\
	Cparam.o\
	CSAMP.o\
	CWORKX.o\
	CWORKC.o\
	MISCMOD.o\
	CRANK.o\
	BETAFN.o\
	BETAIC.o\
	BETALN.o\
	Banner.o\
	Beta.o\
	Binom.o\
	CCMATR.o\
	CHLSKY.o\
	CORCAL.o\
	COROUT.o\
	Chkdat.o\
	Chkdim.o\
	Chkemp.o\
	Chkstr.o\
	Chkzro.o\
	Cmcrd.o\
	Cumulc.o\
	Cumuld.o\
	DATOUT.o\
	DISTNM.o\
	DMFSD.o\
	DSINV.o\
	Datsqz.o\
	ENTRPF.o\
	ERRGET.o\
	ERSTGT.o\
	ERXSET.o\
	Entrpy.o\
	Errchk.o\
	Expon.o\
	FACTOR.o\
	FACTR2.o\
	FILELENG.o\
	FILEOC.o\
	FINDIT.o\
	FINVNOR.o\
	FLNAME.o\
	GAMMAB.o\
	Gamma.o\
	Gammam.o\
	Geom.o\
	HISTO.o\
	HPSRT.o\
	HSTOUT.o\
	HYPGEO.o\
	Hgeom.o\
	IGAUS1.o\
	IGAUSF.o\
	IMTQL2.o\
	INTERP.o\
	INTRPD.o\
	Igaus.o\
	LJUST.o\
	Lhs.o\
	Lhsdrv.o\
	Lread.o\
	MATINV.o\
	MIX.o\
	Nbinom.o\
	Newcrd.o\
	Normal.o\
	OUTDAT.o\
	Outcrd.o\
	PMTRX.o\
	Pareto.o\
	Poison.o\
	Posdef.o\
	RANKER.o\
	RIERFC1.o\
	RMCNP.o\
	RMCNPI.o\
	RNUM1.o\
	Rdpar.o\
	Rdpar2.o\
	Read.o\
	SETDEF.o\
	SIFT.o\
	Samout.o\
	Sspev.o\
	TQLRAT.o\
	TRBAK3.o\
	TRED3.o\
	Table.o\
	Triang.o\
	UNIFRM.o\
	VIF.o\
	WRTCRD.o\
	Weibul.o\
	Wrtpar.o

%.o : %.F
	$(IVFC) $(CFLAGS) -c $<

%.o : %.for
	$(IVFC) $(CFLAGS) -c $<

%.o : %.FOR
	$(IVFC) $(CFLAGS) -c $<

$(TARGET) : $(OBJ)
	$(IVFC) $(LFLAGS) $@ $^

copy_trunk:
	$(CP) $(TARGET) $(CP_TR_1)
	$(CP) $(TARGET) $(CP_TR_2)

copy_branch:
	$(CP) $(TARGET) $(CP_BR_1)
	$(CP) $(TARGET) $(CP_BR_2)

clean:
	$(RM) *.o $(TARGET) *.mod

