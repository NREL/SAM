C     Last change:  GDW   1 Aug 100    9:33 am
C**********************************************************
C     -- THIS FILE IS AN INCLUDE BLOCK THAT CONTAINS ALL OF
C     -- THE COMMON PARAMETERS THAT ARE USED IN LHS.
C
c      PARAMETER (NMAX=20001, MAXNNV=200000)
c      PARAMETER (NVAR=401, NINTMX=401, NCVAR=801)
c      PARAMETER (LENT=125, LENC=80, NAMLEN=16)
c      PARAMETER (MAXTB=5001)

c above dimensions changed 12-2-96                 gdw
C      PARAMETER (NMAX=24576, MAXNNV=200000)
C      PARAMETER (NVAR=1024, NINTMX=64, NCVAR=1024)
C      PARAMETER (LENT=125, LENC=80, NAMLEN=16)
C      PARAMETER (MAXTB=5001)
c
c   description of parameters:
c       NMAX   - Maximum number of observations
c       MAXNNV - Maximum number of variables * number of samples       
c       NVAR   - Maximum number of variables
c       NINTMX - Maximum number of sub-intervals for any uniform* and
c                loguniform* distributions
c       NCVAR  - Maximum number of correlations (pairs)
c       LENT   - Maximum length of title card
c       LENC   - Maximum length of a single (one card) of input
c       NAMLEN - Maximum length of a name
c       MAXTB  - Maximum number of entries that can be in a table 
C
C     -- END OF PARAMETER DECLARATION
C**********************************************************
C
      MODULE PARMS
C
C       These are the elements of the old include file
C
c        dimensions changed 12-2-96                 gdw
        INTEGER :: NMAX=24576, MAXNNV=200000
        INTEGER :: NVAR=1024, NINTMX=64, NCVAR=1024
        INTEGER, PARAMETER :: LENT=125, LENC=256, NAMLEN=16
        INTEGER :: MAXTB=5001
C
C        IPrint is used to control the amount of printing
C           0 = Nothing to the screen
C           1 = Normal
C           2 = Debug printout (someday)
        INTEGER :: IPrint = 1
C
C        ISamW controls the width of the Sample Output File
C           0 = Narrow -- Single Column
C           1 = Normal (compiler default -- 80 characters - I think)
C           2 = Very wide (for input into spreadsheets)
        INTEGER :: ISamW = 1
C
C       Now here is the initialization routine
      CONTAINS
C
      SUBROUTINE PRAMS_INIT()
C
C       Define these variables to be part of a namelist, then
C       provide default values for them.
C
        NAMELIST /LHS/ NMAX, MAXNNV, NVAR, NINTMX, NCVAR,
     1                 MAXTB, IPrint, ISamW
        LOGICAL :: YESNO
C
        NMAX=24576
        MAXNNV=200000
        NVAR=512
        NINTMX=64
        NCVAR=1024
        MAXTB=5001
        IPrint = 1
        ISamW = 1
C
C       Now open the initialization file, read the namelist variables,
C       and close the namelist file to get any changes to these default
C       values.
C
        INQUIRE (FILE="SIPRA.INI", EXIST=YESNO)
C
        IF ( YESNO ) THEN
C
           OPEN (19, FILE="SIPRA.INI", ERR=200, ACTION="READ")
           READ (19, NML=LHS, ERR=100, END=100)
           CLOSE (19)
C
        ELSE
C
           GOTO 200
C
        END IF
C
        RETURN
C
C  = = = = = = = = =  ERROR HANDLING SECTION  = = = = = = = = = = = = =
C
C       An error condition occurred while reading the file.  Close it,
C       write an error message, and go on using the defaults.
C
 100    WRITE (*,*)
        WRITE (*,*) "**** Error reading file SIPRA.INI. ****"
        WRITE (*,*) "****   Default dimensions used.    ****"
        WRITE (*,*)
C
        CLOSE (19)
C
        RETURN
C
C       An error condition occurred while opening the file.
C       Write an error message, and go on using the defaults.
C
 200    WRITE (*,*)
        WRITE (*,*) "**** Error opening file SIPRA.INI. ****"
        WRITE (*,*) "****   Default dimensions used.    ****"
        WRITE (*,*)
C
        RETURN
C
      END SUBROUTINE
C
      SUBROUTINE PRAMS_CLOSE()
C
C       Nothing to deallocate.
C
        RETURN
C
      END SUBROUTINE
C
      END MODULE
