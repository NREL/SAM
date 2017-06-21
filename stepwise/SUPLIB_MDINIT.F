      SUBROUTINE MDINIT (USERV)
C
C     THIS SUBPROGRAM PROVIDES DATA MANAGEMENT WITH DYNAMIC MEMORY
C     ALLOCATION CAPABILITIES.  THE ENTIRE PACKAGE IS WRITTEN IN
C     STANDARD FORTRAN-77 WITH THE EXCEPTION OF TWO SYSTEM DEPENDENT
C     SUBPROGRAMS:  IXLNUM AND EXMEMY.
C
C     AUTHOR:  W. C. MILLS-CURRAN, SANDIA NATIONAL LABORATORIES,
C     DIV. 1523.
C     DATE:  FEBRUARY, 1986.
C
C     DOCUMENTATION:  SAND86-0911
C
C     REVISED 28-OCT-1988
C     Modified to allow memory for character variables.  The error
C     handling was also improved to allow better user control.
C
C ****************************************************************************
C
C                   ISSUED BY SANDIA NATIONAL LABORATORIES,
C                          A PRIME CONTRACTOR TO THE
C                      UNITED STATES DEPARTMENT OF ENERGY
C
C ****************************************************************************
C
C THIS CODE WAS PREPARED IN THE COURSE OF WORK SPONSORED BY THE UNITED STATES
C GOVERNMENT.  NEITHER THE UNITED STATES, NOR THE UNITED STATES DEPARTMENT OF
C ENERGY, NOR THE UNITED STATES NUCLEAR REGULATORY COMMISSION, NOR ANY OF
C THEIR EMPLOYEES, NOR ANY OF THEIR CONTRACTORS, SUBCONTRACTORS, OR THEIR
C EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
C LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS OR USEFULNESS OF
C ANY INFORMATION, APPARATUS, PRODUCT OR PROCESS DISCLOSED, OR REPRESENTS THAT
C ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
C
C ****************************************************************************
C
C     ENTRY POINTS:
C
C     MDINIT (USERV)
C     MCINIT (UCHAR)
C     Initializes the data manager and calculates the offset of the
C     user's base vector.
C
C     MDCOMP
C     MCCOMP
C     Compresses all storage to consolidate the voids within a
C     memory block.
C
C     MDDEBG (UNIT)
C     MCDEBG (UNIT)
C     Turns on immediate error output at the time an error is triggered.
C
C     MDDEL (NAME1)
C     MCDEL (NAME1)
C     "Deletes" (frees the storage of) an array.
C
C     MDEFIX (ENUM, COUNT)
C     MCEFIX (ENUM, COUNT)
C     Resets the error counter for a particular error flag.
C
C     MDEROR (UNIT)
C     MCEROR (UNIT)
C     Prints an error summary.
C
C     MDERPT (ENUM, COUNT)
C     MCERPT (ENUM, COUNT)
C     Returns the error count for a particular error flag.
C
C     MDFILL (TFILL)
C     MCFILL (NAME1)
C     Turns on data initialization to a specific value.
C
C     MDFIND (NAME1, NEWLOC, NEWLEN)
C     MCFIND (NAME1, NEWLOC, NEWLEN)
C     Reports the location and length of an array.
C
C     MDFOFF ()
C     MCFOFF ()
C     Turns off data initialization.
C
C     MDGET (MNGET)
C     MCGET (MNGET)
C     Requests a contiguous block of storage.
C
C     MDGIVE
C     MCGIVE
C     Returns storage to the system under certain conditions.
C
C     MDLAST (ENUM)
C     MCLAST (ENUM)
C     Returns the flag number of the last error.
C
C     MDLIST (UNIT)
C     MCLIST (UNIT)
C     Prints a storage summary.
C
C     MDLONG (NAME1, NEWLOC, NEWLEN)
C     MCLONG (NAME1, NEWLOC, NEWLEN)
C     Changes the length of an array.
C
C     MDNAME (NAME1, NAME2)
C     MCNAME (NAME1, TNAME2)
C     Changes the name of an array.
C
C     MDPRNT (NAME1, UNIT, NAME2)
C     MCPRNT (NAME1, UNIT, NEWLEN)
C     Prints an array.
C
C     MDRSRV (NAME1, NEWLOC, NEWLEN)
C     MCRSRV (NAME1, NEWLOC, NEWLEN)
C     Reserves space for an array and reports the storage location.
C
C     MDSTAT (MNERRS, MNUSED)
C     MCSTAT (MNERRS, MNUSED)
C     Reports on accumulated errors and space used.
C
C     Associated subroutines:
C
C     IXLCHR            XMDLIST            XMCLONG
C     MDINIT            XMDLONG            XMCMEMY
C     MEMTEST           XMDLOOK            XMCNSRT
C     XMDCOMP           XMDNSRT            XMCPRNT
C     XMDDEL            XMDPRNT            XMCRSRV
C     XMDEROR           XMDRSRV            XMSHFTC
C     XMDEXEC           XMCCOMP            XMSHFTI
C     XMDFIND           XMCDEL             XMSRCHC
C     XMDGET            XMCFIND            XMSRCHI
C     XMDGIVE           XMCGET             XMVTABLE
C
C
      IMPLICIT INTEGER (A-Z)
C
C     Dictionary and tables lengths.
C
C     LDICT     Maximum number of dictionary entrys/allocated arrays.
C
C     LVOID     Maximum number of voids.
C
C     These parameters may be changed independently.
C
      PARAMETER (LPDICT=128, LPVOID=128)
      CHARACTER*8 DICT(LPDICT, 2)
      DIMENSION DPOINT(LPDICT, 2, 3)
C                              *  *> 1: LOCATION
C                              *     2: NUMERIC STORAGE UNITS
C                              *     3: CHARACTER STORAGE UNITS
C                              *        (-1 FOR NUMERIC)
C                              *> 1: NUMERIC
C                                 2: CHARACTER FOR NON MIXED MODE
      DIMENSION VOID(LPVOID, 2, 2)
C                            *  *> 1: LOCATION
C                            *     2: SIZE (IN NUMERIC UNITS FOR MIXED MODE)
C                            *> 1: NUMERIC
C                               2: CHARACTER FOR NON MIXED MODE
C
C     The strategy for the above arrays is to fix the second dimension
C     at 1 for mixed mode, but let the first dimension "overrun" so
C     that the storage is usable.  In nonmixed mode, the first dimension
C     will not overrun and the character stuff will use a value of 2 for
C     for the second dimension.  The mixed vs. nonmixed mode is
C     indicated by the variable CHRCOL. (1 = mixed, 2 = nonmixed)
C
      INCLUDE 'SUPLIB_MEMORY_PARAMS.H'
      DIMENSION ERRVEC(NERVEC)
C
C     Internal base vectors.
C     All pointers in the tables are relative to these base vectors, and
C     an offset to the user's base vector is maintained to give the
C     user the correct pointer to the user's base vector.  They are
C     equivalenced to allow printing as real or integer (see mdprnt).
C
      DIMENSION MYV(1)
      REAL RMYV(1)
      EQUIVALENCE (MYV,RMYV)
      PARAMETER (MAXSIZ=256)
      CHARACTER*1 MYCHAR(MAXSIZ), CFDATA
      COMMON /XXMEMCOM/ MYCHAR
C
      LOGICAL INIT, CINIT, DEFER, DEFROK, FILL, CFILL
C
C     User's base vector.  This is an argument in the entry point
C     MDINIT.
C
      DIMENSION USERV(1)
      CHARACTER*1 UCHAR(1)
C
      PARAMETER (NCOLP=132)
      CHARACTER*8  NAME1, NAME2, NAMET
      CHARACTER*(*)  TNAME1, TNAME2
      DIMENSION NVOIDS(2), NNAMES(2)
      SAVE
C
C     Data initialization
C
      DATA NVOIDS /2*0/, NNAMES /2*0/
      DATA ERRVEC /NERVEC*0/
      DATA INIT /.FALSE./, CINIT /.FALSE./,
     &   DEFER /.FALSE./, DEFROK /.FALSE./,
     &   FILL /.FALSE./, CFILL /.FALSE./
      DATA EUNIT /0/
      DATA LERROR /0/
      DATA CHRCOL /1/
C
      LDICT = 2 * LPDICT
      LVOID = 2 * LPVOID
C
C     Get the offset between "USERV" and "MYV".  All internal
C     information is in terms of "MYV" since most operations are done
C     with this base address.
C

C     even if myv and userv are 1 dimension array, compiler does not want 
C     array as replacement parameter for scalar. Added then (1) to them
C     C.S. 05/25/2010
      MYLOC = IXLNUM (MYV(1))
      ULOC = IXLNUM (USERV(1))
      OFFSET = MYLOC - ULOC
C
C     Check to see if deferred mode will be allowed.  This requires
C     a functioning IXLNUM.
      IF (IXLNUM(NVOIDS(1)) .NE. IXLNUM(NVOIDS(2))) DEFROK = .TRUE.
C
C     Was MDINIT previously called?
C
      IF (INIT) THEN
         LASTER = REINIT
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDINIT',
     &      'Second call to MDINIT not allowed'
      ELSE
         LASTER = SUCESS
      END IF
      INIT = .TRUE.
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) LERROR = LASTER
      RETURN
C
C***********************************************************************
C
      ENTRY MCINIT (UCHAR)
C
C***********************************************************************
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCINIT',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      MYCLOC = IXLCHR (MYCHAR(1))
      UCLOC = IXLCHR (UCHAR(1))
      TEMP = IXLCHR (MYCHAR(2))
      IF (MYCLOC .NE. TEMP) THEN
C
C        Mixed mode - character and numeric data intermixed.
C
         CALL EXPARM (NAMET, NAMET, I, CHRNUM, I, I)
         COFFST = CHRNUM * MYLOC - UCLOC
      ELSE
C
C        Split mode - character and numeric data separate.
C
         COFFST = 0
         CHRCOL = 2
         CHRNUM = 1
         IF (NNAMES(1) .GT. LPDICT) THEN
            LASTER = DFULL
         ELSE IF (NVOIDS(1) .GT. LPVOID) THEN
            LASTER = VFULL
         ELSE
            LDICT = LPDICT
            LVOID = LPVOID
            LASTER = SUCESS
         END IF
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCINIT'
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      ELSE
         CINIT = .TRUE.
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCCOMP
C
C***********************************************************************
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCCOMP',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C***********************************************************************
C
      ENTRY MDCOMP
C
C***********************************************************************
C
C     This section compresses all void space to the end of each block.
C     No attempt is made to shift storage between blocks.
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDCOMP',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      CALL XMDCOMP (MYV, VOID, LVOID,
     &   NVOIDS, DPOINT, LDICT, NNAMES, CHRCOL, LASTER)
C
      IF (LASTER .EQ. SUCESS .AND. CHRCOL .EQ. 2) THEN
         CALL XMCCOMP (MYCHAR, VOID, LVOID,
     &      NVOIDS, DPOINT, LDICT, NNAMES, CHRCOL, LASTER)
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MD/MCCOMP'
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
C
      RETURN
C
C***********************************************************************
C
      ENTRY MCDEBG (UNIT)
      ENTRY MDDEBG (UNIT)
C
C***********************************************************************
C
      IF (UNIT .GE. 1) THEN
         EUNIT = UNIT
      ELSE
         EUNIT = 0
      END IF
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MDDEL (TNAME1)
C
C***********************************************************************
C
C     This entry point deletes an entry from the dictionary and
C     inserts an entry in the void table.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDDEL',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDDEL',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      CALL XMDDEL (NAME1, DICT, DPOINT, LDICT, NNAMES, VOID,
     &   LVOID, NVOIDS, CHRCOL, LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDDEL', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCDEL (TNAME1)
C
C***********************************************************************
C
C     This entry point deletes an entry from the dictionary and
C     inserts an entry in the void table.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCDEL',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCDEL',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
      CALL XMCDEL (NAME1, DICT, DPOINT, LDICT, NNAMES, VOID,
     &   LVOID, NVOIDS, CHRCOL, LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCDEL', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCEFIX (ENUM, COUNT)
      ENTRY MDEFIX (ENUM, COUNT)
C
C***********************************************************************
C
C     This resets the number of occurances of a particular error.
C     If the requested error number (enum) is out of range, an error
C     is flagged.
C
      IF (ENUM .GE. 1 .AND. ENUM .LE. NERVEC) THEN
         ERRVEC(ENUM) = COUNT
         LASTER = SUCESS
      ELSE
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDEFIX',
     &      'Illegal error flag number given'
         LASTER = BADCOD
      END IF
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) LERROR = LASTER
      RETURN
C
C***********************************************************************
C
      ENTRY MCEROR (UNIT)
      ENTRY MDEROR (UNIT)
C
C***********************************************************************
C
C     This section prints the error codes.
C
      CALL XMDEROR (UNIT, LASTER, ERRVEC, .TRUE.)
capgNote: This routine does not affect error status
capg      LASTER = SUCESS
capg      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MCERPT (ENUM, COUNT)
      ENTRY MDERPT (ENUM, COUNT)
C
C***********************************************************************
C
C     This reports the number of occurances of a particular error.
C     If the requested error number (ENUM) is out of range, a -1
C     is returned.
C
      IF (ENUM .GE. 1 .AND. ENUM .LE. NERVEC) THEN
         COUNT = ERRVEC(ENUM)
      ELSE
         COUNT = -1
      END IF
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MDFILL (TFILL)
C
C***********************************************************************
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDFILL',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      FDATA = TFILL
      FILL = .TRUE.
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MCFILL (TNAME1)
C
C***********************************************************************
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCFILL',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
      IF (LEN(TNAME1) .LT. 1) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCFILL',
     &      'Bad character string given'
         RETURN
      END IF
      CFDATA = TNAME1(1:1)
      CFILL = .TRUE.
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MDFIND (TNAME1, NEWLOC, NEWLEN)
C
C***********************************************************************
C
C     This section finds a name in the dictionary and returns the
C     location and length of that vector.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDFIND',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDFIND',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
C     Find the name in the dictionary.
C
      CALL XMDFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
C
      IF (LASTER .EQ. SUCESS) THEN
C
C        Entry was found in dictionary.
C
         IF (DPOINT(ROW,1,2) .LT. 0) THEN
            DPOINT(ROW,1,1) = IXLNUM(NEWLOC)
            NEWLOC = - ULOC
         ELSE
            NEWLOC = DPOINT(ROW,1,1) + OFFSET
         END IF
         NEWLEN = ABS(DPOINT(ROW,1,2))
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDFIND', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCFIND (TNAME1, NEWLOC, NEWLEN)
C
C***********************************************************************
C
C     This section finds a name in the dictionary and returns the
C     location and length of that vector.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCFIND',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCFIND',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C     Find the name in the dictionary.
C
      CALL XMCFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
C
      IF (LASTER .EQ. SUCESS) THEN
C
C        Entry was found in dictionary.
C
         IF (DPOINT(ROW,CHRCOL,2) .LT. 0) THEN
            DPOINT(ROW,CHRCOL,1) = IXLNUM(NEWLOC)
            NEWLOC = - UCLOC
         ELSE IF (DPOINT(ROW,CHRCOL,3) .EQ. 0) THEN
            NEWLOC = 1
         ELSE
            NEWLOC = (DPOINT(ROW,1,1) - 1) * CHRNUM + 1 + COFFST
         END IF
         NEWLEN = DPOINT(ROW,1,3)
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCFIND', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MDFOFF ()
C
C***********************************************************************
C
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDFOFF',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      FILL = .FALSE.
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MCFOFF ()
C
C***********************************************************************
C
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCFOFF',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
      CFILL = .FALSE.
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MDGET (MNGET)
C
C***********************************************************************
C
C     This section processes a request for a contiguous
C     chunk of memory.
C     If a void of sufficient size is available, no action is taken,
C     otherwise a call to the system for the memory is made.
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDGET',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      CALL XMDGET (MYLOC, MNGET, VOID, LVOID, NVOIDS,
     &   CHRCOL, LASTER, VROW)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDGET'
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCGET (MNGET)
C
C***********************************************************************
C
C     This section processes a request for a contiguous
C     chunk of memory.
C     If a void of sufficient size is available, no action is taken,
C     otherwise a call to the system for the memory is made.
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCGET',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
      IF (CHRCOL .EQ. 2) THEN
         CALL XMCGET (MYCLOC, MNGET, VOID, LVOID, NVOIDS,
     &      CHRCOL, MAXSIZ, LASTER, VROW)
      ELSE
         INTLEN = (MNGET + CHRNUM - 1) / CHRNUM
         CALL XMDGET (MYLOC, INTLEN, VOID, LVOID, NVOIDS,
     &      CHRCOL, LASTER, VROW)
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCGET'
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCGIVE
C
C***********************************************************************
C
C     This section returns unused space at the end of all storage
C     blocks to the system.  It does not return character memory
C     in split mode.
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCGIVE',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C***********************************************************************
C
      ENTRY MDGIVE
C
C***********************************************************************
C
C     This section returns unused space at the end of all storage
C     blocks to the system.
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDGIVE',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
C *** MCGIVE and MDGIVE have not effect.  XMDGIVE generates an error.
C#????      CALL XMDGIVE (MYLOC, DPOINT, LDICT, NNAMES, VOID, LVOID,
C#????     &   NVOIDS, CHRCOL, LASTER)
C#????C
C#????      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
C#????      IF (LASTER .NE. SUCESS) THEN
C#????         LERROR = LASTER
C#????         IF (EUNIT .GT. 0) THEN
C#????            WRITE (EUNIT, 10010) 'MDGIVE'
C#????            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
C#????         END IF
C#????      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCLAST (ENUM)
      ENTRY MDLAST (ENUM)
C
C***********************************************************************
C
      ENUM = LERROR
capgNote: This routine does not affect error status
capg      LASTER = SUCESS
capg      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MCLIST (UNIT)
C
C***********************************************************************
C
C     This section lists the internal tables of the data manager.
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCLIST',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C***********************************************************************
C
      ENTRY MDLIST (UNIT)
C
C***********************************************************************
C
C     This section lists the internal tables of the data manager.
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDLIST',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      CALL XMDLIST (UNIT, OFFSET, DICT, DPOINT, LDICT, NNAMES,
     &   VOID, LVOID, NVOIDS, CHRCOL)
C
      LASTER = SUCESS
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      RETURN
C
C***********************************************************************
C
      ENTRY MDLONG (TNAME1, NEWLOC, NEWLEN)
C
C***********************************************************************
C
C     This section expands or shortens a vector.
C     Any nonnegative value of newlen is permissible.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDLONG',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDLONG',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
C     Is the new length sensible?
C
      IF (NEWLEN .LT. 0) THEN
         LASTER = BADLEN
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDLONG',
     &      'Negative length given for name ', NAME1
         RETURN
      END IF
C
      CALL XMDLONG (NAME1, NEWLEN, NEWLOC, MYV, MYCHAR, MYLOC,
     &   MYCLOC, UCLOC, COFFST, OFFSET,
     &   DICT, DPOINT, LDICT, NNAMES, VOID, LVOID, NVOIDS,
     &   FILL, FDATA, CFILL, CFDATA, CHRNUM, CHRCOL, LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDLONG', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCLONG (TNAME1, NEWLOC, NEWLEN)
C
C***********************************************************************
C
C     This section expands or shortens a vector.
C     Any nonnegative value of newlen is permissible.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCLONG',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCLONG',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C     Is the new length sensible?
C
      IF (NEWLEN .LT. 0) THEN
         LASTER = BADLEN
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCLONG',
     &      'Negative length given for name ', NAME1
         RETURN
      END IF
C
      CALL XMCLONG (NAME1, NEWLEN, NEWLOC, MYV, MYCHAR, MYLOC,
     &   MYCLOC, UCLOC, COFFST, OFFSET,
     &   DICT, DPOINT, LDICT, NNAMES, VOID, LVOID, NVOIDS,
     &   FILL, FDATA, CFILL, CFDATA, CHRNUM, CHRCOL, MAXSIZ, LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCLONG', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MDNAME (TNAME1, TNAME2)
C
C***********************************************************************
C
C     This section renames an array from NAME1 to NAME2.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDNAME',
     &      'From name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
      NAMET = TNAME2
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDNAME',
     &      'To name is blank, from name ', NAME1
         RETURN
      END IF
      NAME2 = NAMET(LEFT:RIGHT)
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDNAME',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
C     Find NAME1 in dictionary.
C
      CALL XMDFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
      IF (LASTER .EQ. SUCESS) THEN
C
         LOC = DPOINT(ROW,1,1)
         LEN1 = DPOINT(ROW,1,2)
C
C        Delete dictionary entry.
C
         CALL XMSHFTC (DICT, CHRCOL*LDICT, ROW+1, NNAMES(1), 1)
         CALL XMSHFTI (DPOINT, LDICT*CHRCOL, 3, ROW+1, NNAMES(1), 1)
         NNAMES(1) = NNAMES(1) - 1
C
C        insert NAME2 with old pointer and length.
C
         CALL XMDNSRT (NAME2, LOC, LEN1, DICT, DPOINT, LDICT,
     &      NNAMES, CHRCOL, LASTER)
         IF (LASTER .NE. SUCESS) THEN
            CALL XMDNSRT (NAME1, LOC, LEN1, DICT, DPOINT, LDICT,
     &         NNAMES, CHRCOL, TMP)
         END IF
C
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDNAME', 'from '//NAME1//' to '//NAME2
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MCNAME (TNAME1, TNAME2)
C
C***********************************************************************
C
C     This section renames an array from NAME1 to NAME2.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCNAME',
     &      'From name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
      NAMET = TNAME2
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCNAME',
     &      'To name is blank, from name ', NAME1
         RETURN
      END IF
      NAME2 = NAMET(LEFT:RIGHT)
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCNAME',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C     Find NAME1 in dictionary.
C
      CALL XMCFIND (NAME1, DICT, DPOINT, LDICT, NNAMES,
     &   CHRCOL, LASTER, ROW)
      IF (LASTER .EQ. SUCESS) THEN
C
         LOC = DPOINT(ROW,CHRCOL,1)
         LEN1 = DPOINT(ROW,CHRCOL,2)
         LEN2 = DPOINT(ROW,CHRCOL,3)
C
C        Delete dictionary entry.
C
         CALL XMSHFTC (DICT(1,CHRCOL), CHRCOL*LDICT, ROW+1,
     &      NNAMES(CHRCOL), 1)
         CALL XMSHFTI (DPOINT(1,CHRCOL,1), LDICT*CHRCOL, 3, ROW+1,
     &      NNAMES(CHRCOL), 1)
         NNAMES(CHRCOL) = NNAMES(CHRCOL) - 1
C
C        insert NAME2 with old pointer and length.
C
         CALL XMCNSRT (NAME2, LOC, LEN1, LEN2, DICT, DPOINT, LDICT,
     &      NNAMES, CHRCOL, LASTER)
         IF (LASTER .NE. SUCESS) THEN
            CALL XMCNSRT (NAME1, LOC, LEN1, LEN2, DICT, DPOINT, LDICT,
     &         NNAMES, CHRCOL, TMP)
         END IF
C
      END IF
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCNAME', 'from '//NAME1//' to '//NAME2
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
      RETURN
C
C***********************************************************************
C
      ENTRY MDPRNT (TNAME1, UNIT, TNAME2)
C
C***********************************************************************
C
C     This section prints a named vector as integer or real, as
C     indicated by NAME2.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDPRNT',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
      NAMET = TNAME2
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDPRNT',
     &      'Data type is blank for name', NAME1
         RETURN
      END IF
      NAME2 = NAMET(LEFT:RIGHT)
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDPRNT',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
      CALL XMDPRNT (NAME1, UNIT, NAME2, MYV, RMYV, OFFSET,
     &   DICT, DPOINT, LDICT, NNAMES, CHRCOL, NCOLP, LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDPRNT'
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
C
      RETURN
C
C***********************************************************************
C
      ENTRY MCPRNT (TNAME1, UNIT, NEWLEN)
C
C***********************************************************************
C
C     This section prints a named vector as character.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCPRNT',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCPRNT',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
      CALL XMCPRNT (NAME1, UNIT, MYCHAR, COFFST, COFFST+UCLOC-MYCLOC,
     &   DICT, DPOINT, LDICT, NNAMES, CHRNUM,
     &   CHRCOL, NCOLP, NEWLEN, LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCPRNT'
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
C
      RETURN
C
C***********************************************************************
C
      ENTRY MDRSRV (TNAME1, NEWLOC, NEWLEN)
C
C***********************************************************************
C
C     This section reserves space for an array.  If a void is not
C     available, a system call will be made for the space.  Any
C     nonnegative request is permissible.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDRSRV',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MDINIT previously called?
C
      IF (.NOT. INIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDRSRV',
     &      'MDINIT has not been called'
         RETURN
      END IF
C
C     Check for sensible length.
C
      IF (NEWLEN .LT. 0) THEN
         LASTER = BADLEN
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MDRSRV',
     &      'Negative length given for name ', NAME1
         RETURN
      END IF
C
      CALL XMDRSRV (MYV, NAME1, NEWLEN, NEWLOC, MYLOC, OFFSET,
     &   VOID, LVOID, NVOIDS,
     &   DICT, DPOINT, LDICT, NNAMES, CHRCOL, DEFER, FILL, FDATA,
     &   LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MDRSRV', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
C
      RETURN
C
C***********************************************************************
C
      ENTRY MCRSRV (TNAME1, NEWLOC, NEWLEN)
C
C***********************************************************************
C
C     This section reserves space for an array.  If a void is not
C     available, a system call will be made for the space.  Any
C     nonnegative request is permissible.
C
      NAMET = TNAME1
      CALL XXUPPER (NAMET, LEFT, RIGHT)
      IF (LEFT .GT. RIGHT) THEN
         LASTER = BADNAM
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCRSRV',
     &      'Name is blank'
         RETURN
      END IF
      NAME1 = NAMET(LEFT:RIGHT)
C
C     Was MCINIT previously called?
C
      IF (.NOT. CINIT) THEN
         LASTER = NOINIT
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCRSRV',
     &      'MCINIT has not been called'
         RETURN
      END IF
C
C     Check for sensible length.
C
      IF (NEWLEN .LT. 0) THEN
         LASTER = BADLEN
         ERRVEC(LASTER) = ERRVEC(LASTER) + 1
         LERROR = LASTER
         IF (EUNIT .GT. 0) WRITE (EUNIT, 10000) 'MCRSRV',
     &      'Negative length given for name ', NAME1
         RETURN
      END IF
C
      CALL XMCRSRV (MYCHAR, NAME1, NEWLEN, NEWLOC, MYLOC, MYCLOC,
     &   UCLOC, OFFSET, COFFST,
     &   VOID, LVOID, NVOIDS,
     &   DICT, DPOINT, LDICT, NNAMES, CHRCOL, CHRNUM, DEFER,
     &   CFILL, CFDATA, MAXSIZ,
     &   LASTER)
C
      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      IF (LASTER .NE. SUCESS) THEN
         LERROR = LASTER
         IF (EUNIT .GT. 0) THEN
            WRITE (EUNIT, 10010) 'MCRSRV', 'name '//NAME1
            CALL XMDEROR (EUNIT, LASTER, ERRVEC, .FALSE.)
         END IF
      END IF
C
      RETURN
C
C***********************************************************************
C
      ENTRY MDSTAT (MNERRS, MNUSED)
C
C***********************************************************************
C
C     This section returns the total number of errors and the total
C     amount of numeric storage assigned to arrays.
C
capgNote: This routine does not affect error status
capg      LASTER = SUCESS
capg      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      MNERRS = 0
      DO 100 I = 2, NERVEC
         MNERRS = MNERRS + ERRVEC(I)
  100 CONTINUE
      MNUSED = 0
      DO 110 I = 1, NNAMES(1)
         IF (DPOINT(I,1,3) .EQ. -1) THEN
            MNUSED = MNUSED + ABS(DPOINT(I,1,2))
         END IF
  110 CONTINUE
      RETURN
C
C***********************************************************************
C
      ENTRY MCSTAT (MNERRS, MNUSED)
C
C***********************************************************************
C
C     This section returns the total number of errors and the total
C     amount of character storage assigned to arrays.
C
capgNote: This routine does not affect error status
capg      LASTER = SUCESS
capg      ERRVEC(LASTER) = ERRVEC(LASTER) + 1
      MNERRS = 0
      DO 120 I = 2, NERVEC
         MNERRS = MNERRS + ERRVEC(I)
  120 CONTINUE
      MNUSED = 0
      DO 130 I = 1, NNAMES(CHRCOL)
         MNUSED = MNUSED + MAX (0, DPOINT(I,CHRCOL,3))
  130 CONTINUE
      RETURN
C
10000  FORMAT (/1X, '%%% CAMSUPES_LIB error in ', A, :, ' %%% ', 5A)
10010  FORMAT (/1X, '%%% CAMSUPES_LIB error in ', A,
     &   ' %%% See error table below', :, ', ', 5A)
      END
