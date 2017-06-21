C=======================================================================
      SUBROUTINE FILECHO2 (IUNITI, IUNITO, FLAG)
C=======================================================================

C   --*** FILECHO2 *** (CAMCON_LIB) Read formatted file and echo to output
C   --   Modified by Amy Gilkey - revised 02/27/93
C   --   Written by Jonathan S. Rath - 11/10/88
C   --
C   --FILECHO2 reads a formatted ASCII input file and echoes the file line
C   --by line to a formatted output file.  The echo is exact - NO extra
C   --lines or characters or FORTRAN carriage control are added.
C   --
C   --Limitations:
C   --   Writes only the first 128 characters from an input file record.
C   --
C   --Parameters:
C   --   IUNITI - IN integer - the input file unit number
C   --   IUNITO - IN integer - the output file unit number
C   --   FLAG - IN char*(*) - unused
C   --      (only the first character is checked; case is unimportant)
C   --      ' '      to simply echo the file

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER IUNITI, IUNITO
      CHARACTER*(*) FLAG

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER NLINES
      INTEGER IDUM
      CHARACTER*128 LINE

      NLINES = 0

  100 CONTINUE
C   --Read a line
      READ (IUNITI, '(A)', ERR=110, END=110) LINE
C   --Write the line
      NLINES = NLINES + 1
      WRITE (IUNITO, 10000, IOSTAT=IDUM) LINE(:ISTRLEN(LINE))
      GOTO 100

  110 CONTINUE
C   --Rewind file before returning
      REWIND (IUNITI, IOSTAT=IDUM)

      RETURN
10000  FORMAT (A)
      END
