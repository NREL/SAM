C=======================================================================
      SUBROUTINE EXFOPEN (IUNIT, INOUT, FORMT, INFILE, IERROR)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXFOPEN *** (CAMSUPES_LIB) Open a file
C   --   Written by Amy Gilkey - revised 10/20/98
C   --
C   --EXFOPEN opens an input or output file.  The file is rewound after open.
C   --
C   --Parameters:
C   --   IUNIT - IN integer - the file unit number
C   --   INOUT - IN char*(*) - the file status:
C   --      (only the first character is checked; case is unimportant)
C   --      'INPUT' for an input file (status = 'old')
C   --      'OUTPUT' for an output file (status = 'new')
C   --      'SCRATCH' for a scratch file (status = 'scratch')
C   --   FORMT - IN char*(*) - the file form:
C   --      (only the first character is checked; case is unimportant)
C   --      'FORMATTED' for a formatted file
C   --      'UNFORMATTED' for an unformatted file
C   --      'LISTING' for a formatted file with no FORTRAN carriage control
C   --   INFILE - IN char*(*) - the file name
C   --   IERROR - OUT integer - the returned OPEN error status; 0 if no error

C   --Machine Dependencies:
C   --   Function LNBLNK (string length).
C   --   The READONLY option.

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER IUNIT
      CHARACTER*(*) INOUT
      CHARACTER*(*) FORMT
      CHARACTER*(*) INFILE
      INTEGER IERROR

C   --Definitions for functions called:
      INTEGER LNBLNK

      INTEGER LNAM
      INTEGER LFORM
      CHARACTER*1 FSTAT
      CHARACTER*1 FFORM
      CHARACTER*128 FILNAM
      CHARACTER*11 FORM
      INTEGER IDUM1, IDUM2

      FSTAT = INOUT(1:1)
      CALL XXUPPER (FSTAT, IDUM1, IDUM2)
      FFORM = FORMT(1:1)
      CALL XXUPPER (FFORM, IDUM1, IDUM2)

      FILNAM = INFILE
      LNAM = LNBLNK (FILNAM)

      IF (FFORM .EQ. 'U') THEN
         FORM = 'UNFORMATTED'
      ELSE IF (FFORM .EQ. 'F') THEN
         FORM = 'FORMATTED'
      ELSE
         FORM = 'FORMATTED'
      END IF
      LFORM = LNBLNK (FORM)

      IF (FSTAT .EQ. 'I') THEN
         OPEN (UNIT=IUNIT, FILE=FILNAM(:LNAM), FORM=FORM(:LFORM),
     &      STATUS='OLD', IOSTAT=IERROR, ERR=100)

      ELSE IF (FSTAT .EQ. 'O') THEN
C      --Keyword RECL is probably ignored
         IF (FFORM .EQ. 'U') THEN
            OPEN (UNIT=IUNIT, FILE=FILNAM(:LNAM), FORM=FORM(:LFORM),
     &         STATUS='UNKNOWN', IOSTAT=IERROR, ERR=100)
         ELSE IF (FFORM .EQ. 'L') THEN
            OPEN (UNIT=IUNIT, FILE=FILNAM(:LNAM), FORM=FORM(:LFORM),
     &         RECL=16384,
     &         STATUS='UNKNOWN', IOSTAT=IERROR, ERR=100)
         ELSE
            OPEN (UNIT=IUNIT, FILE=FILNAM(:LNAM), FORM=FORM(:LFORM),
     &         RECL=16384,
     &         STATUS='UNKNOWN', IOSTAT=IERROR, ERR=100)
         END IF

      ELSE IF (FSTAT .EQ. 'S') THEN
C      --Keyword RECL is probably ignored
         IF (FFORM .EQ. 'U') THEN
            OPEN (UNIT=IUNIT, FILE=FILNAM(:LNAM), FORM=FORM(:LFORM),
     &         STATUS='SCRATCH', IOSTAT=IERROR, ERR=100)
         ELSE
            OPEN (UNIT=IUNIT, FILE=FILNAM(:LNAM), FORM=FORM(:LFORM),
     &         RECL=16384,
     &         STATUS='SCRATCH', IOSTAT=IERROR, ERR=100)
         END IF

      ELSE
         IERROR = -999
         GOTO 100
      END IF

      REWIND (IUNIT)

  100 CONTINUE
      RETURN
      END
