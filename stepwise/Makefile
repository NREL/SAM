# Because %SystemRoot% is the path to the windows directory is should be set on all Windows computers but not on Linux/MacOS
ifdef SystemRoot
	TARGET=stepwise.exe
	CP_TR_1=..\SAMnt\deploy\runtime\bin
	CP_TR_2=..\SAMnt\deploy\runtime\bin
	CP_BR_1=..\..\2015.1.30\SAMnt\deploy\runtime\bin
	CP_BR_2=..\..\2015.1.30\SAMnt\deploy\runtime\bin
	CP=copy
	RM=del /q
else
	TARGET=stepwise.bin
	CP_TR_1=../SAMnt/osx_wx3
	CP_TR_2=../SAMnt/osx_wx3/SAM.app/Contents/runtime/bin
	CP_BR_1=../../2015.1.30/SAMnt/osx_wx3
	CP_BR_2=../../2015.1.30/SAMnt/osx_wx3/SAM.app/Contents/runtime/bin
	CP=cp
	RM=rm -f
endif
CFLAGS=-O2
IVFC=gfortran
LFLAGS=-static-libgfortran -static-libgcc -static-libstdc++ -o

OBJ = \
	CAMLIB_FEELEM.o \
	CAMLIB_FEIJK.o \
	CAMLIB_FEMINMAX.o \
	CAMLIB_FENENUM.o \
	CAMLIB_FF0LIN.o \
	CAMLIB_FF0TKN.o \
	CAMLIB_FF0TKNK.o \
	CAMLIB_FF9DBL.o \
	CAMLIB_FFGETCMT.o \
	CAMLIB_FFGETLIN.o \
	CAMLIB_FFKEYPRM.o \
	CAMLIB_FFPAREQN.o \
	CAMLIB_FFPARSE.o \
	CAMLIB_FFRDFLDS.o \
	CAMLIB_FFRDKEYS.o \
	CAMLIB_FFRDLINE.o \
	CAMLIB_FILCMDLIN.o \
	CAMLIB_FILDFNAM.o \
	CAMLIB_FILECHO.o \
	CAMLIB_FILECHO2.o \
	CAMLIB_FILGTNAMS.o \
	CAMLIB_FILOPEN.o \
	CAMLIB_FILPARSE.o \
	CAMLIB_FILRDNAMS.o \
	CAMLIB_FILWRCMD.o \
	CAMLIB_FILWRNAMS.o \
	CAMLIB_IQAERRUNI.o \
	CAMLIB_ISTRFIND.o \
	CAMLIB_ISTRLEN.o \
	CAMLIB_QA0INIT.o \
	CAMLIB_QAABORT.o \
	CAMLIB_QABANNER.o \
	CAMLIB_QABATCH.o \
	CAMLIB_QABIGLET.o \
	CAMLIB_QACANCEL.o \
	CAMLIB_QACPUS.o \
	CAMLIB_QADOEDIS.o \
	CAMLIB_QAFETCH.o \
	CAMLIB_QAHELP.o \
	CAMLIB_QAMAXERR.o \
	CAMLIB_QAMEMERR.o \
	CAMLIB_QAMESSAG.o \
	CAMLIB_QAPAGE.o \
	CAMLIB_QAPRTSTR.o \
	CAMLIB_QASETUP.o \
	CAMLIB_STRCMPRS.o \
	CAMLIB_STRLIMIT.o \
	CAMLIB_STRPACK.o \
	CAMLIB_STRUPCASE.o \
	SUPLIB_EXABORT.o \
	SUPLIB_EXCANCEL.o \
	SUPLIB_EXCMDLIN.o \
	SUPLIB_EXCPUS.o \
	SUPLIB_EXDATE.o \
	SUPLIB_EXEXEINFO.o \
	SUPLIB_EXFILDAT.o \
	SUPLIB_EXFILFLD.o \
	SUPLIB_EXFOPEN.o \
	SUPLIB_EXHELP.o \
	SUPLIB_EXLOGICAL.o \
	SUPLIB_EXMEMY.o \
	SUPLIB_EXOVERWR.o \
	SUPLIB_EXPARM.o \
	SUPLIB_EXREAD.o \
	SUPLIB_EXSPAWN.o \
	SUPLIB_EXSYMBOL.o \
	SUPLIB_EXTIME.o \
	SUPLIB_IXLCHR.o \
	SUPLIB_IXLNUM.o \
	SUPLIB_MDINIT.o \
	SUPLIB_XMCCOMP.o \
	SUPLIB_XMCDEL.o \
	SUPLIB_XMCFIND.o \
	SUPLIB_XMCGET.o \
	SUPLIB_XMCLONG.o \
	SUPLIB_XMCMEMY.o \
	SUPLIB_XMCNSRT.o \
	SUPLIB_XMCPRNT.o \
	SUPLIB_XMCRSRV.o \
	SUPLIB_XMDCOMP.o \
	SUPLIB_XMDDEL.o \
	SUPLIB_XMDEROR.o \
	SUPLIB_XMDEXEC.o \
	SUPLIB_XMDFIND.o \
	SUPLIB_XMDGET.o \
	SUPLIB_XMDGIVE.o \
	SUPLIB_XMDLIST.o \
	SUPLIB_XMDLONG.o \
	SUPLIB_XMDLOOK.o \
	SUPLIB_XMDNSRT.o \
	SUPLIB_XMDPRNT.o \
	SUPLIB_XMDRSRV.o \
	SUPLIB_XMSHFTC.o \
	SUPLIB_XMSHFTI.o \
	SUPLIB_XMSRCHC.o \
	SUPLIB_XMSRCHI.o \
	SUPLIB_XMVTABLE.o \
	SUPLIB_XXUPPER.o \
	main_stepwise.o \
	stp___default_lib.o \
	stp___ff_rtns.o \
	stp___ffmisc_rtns.o \
	stp___ffxvar.o \
	stp___ranker.o \
	stp___trn_lib.o \
	stp_algama.o \
	stp_calcconfintv.o \
	stp_calcor.o \
	stp_calcpress.o \
	stp_calcwtvar.o \
	stp_chkeqnsyn.o \
	stp_chkunilab.o \
	stp_ckinvsym.o \
	stp_dbl2sng.o \
	stp_dcdft.o \
	stp_delvar.o \
	stp_dumpvar.o \
	stp_fsub.o \
	stp_invertsym.o \
	stp_listplot.o \
	stp_loceqnvar.o \
	stp_plot.o \
	stp_postfixeqn.o \
	stp_prtsymmat.o \
	stp_rankalldata.o \
	stp_rddatval.o \
	stp_readuserinput.o \
	stp_regranal.o \
	stp_regrvar.o \
	stp_rnksrt.o \
	stp_scatter.o \
	stp_sng2dbl.o \
	stp_std01.o \
	stp_stepadd.o \
	stp_stepdel.o \
	stp_stepforce.o \
	stp_stepinit.o \
	stp_steppick.o \
	stp_stepstep.o \
	stp_tinv.o \
	stp_transform.o

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

