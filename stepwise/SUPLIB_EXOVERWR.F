C=======================================================================
      SUBROUTINE EXOVERWR (OPTION, LINE)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXOVERWR *** (CAMSUPES_LIB) Overwrite line
C   --   Written by Amy Gilkey - revised 06/01/95
C   --
C   --EXOVERWR writes over a line that has just been written to the standard
C   --output.
C   --
C   --Parameters:
C   --   OPTION - IN char*(*) - first character only, case unimportant:
C   --      ' ' = overwrite line
C   --      'INIT' = start line to be overwritten and overwrite line
C   --      'BLANK' = blank line which has been previously overwritten
C   --         (up to 128 characters) and overwrite line
C   --   LINE - IN char*(*) - the line to be written over the previous line

C   --Machine Dependencies:
C   --   Function LNBLNK (string length).
C   --   Machine-specific FORMAT.

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) OPTION
      CHARACTER*(*) LINE

      INTEGER LNBLNK

      INTEGER LSTR
      INTEGER IDUM
      CHARACTER OPT
      CHARACTER*128 BLANK

      INTEGER MAXLEN
      SAVE MAXLEN
      DATA MAXLEN /0/

C   --Get the option
      OPT = OPTION
      CALL XXUPPER (OPT, IDUM, LSTR)

      IF (OPT .EQ. 'I') THEN
C      --Start line
         MAXLEN = 0
         WRITE (*, *)

      ELSE IF (OPT .EQ. 'B') THEN
         IF (MAXLEN .GT. 0) THEN
C         --Overwrite previously written line with blanks
            BLANK = ' '
            WRITE (*, 10000, IOSTAT=IDUM) BLANK(1:MAXLEN)
            MAXLEN = 0
         END IF
      END IF

C   --Overwrite line
      LSTR = LNBLNK (LINE)
      IF (LSTR .GT. 0) THEN
         MAXLEN = MAX (MAXLEN, LSTR)
         WRITE (*, 10000, IOSTAT=IDUM) LINE(1:LSTR)
10000     FORMAT ('+', A)
      END IF

      RETURN
      END
