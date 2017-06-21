C=======================================================================
      SUBROUTINE FILECHO (IUNITI, IUNITO, FLAG)
C=======================================================================

C   --*** FILECHO *** (CAMCON_LIB) Read formatted file and write listing
C   --   Modified by Amy Gilkey - revised 08/17/98
C   --   Written by Jonathan S. Rath - 11/10/88
C   --
C   --FILECHO reads a formatted ASCII input file and echoes the file line
C   --by line to a formatted listing file.  The listing may optionally
C   --include line numbers and paging.  FORTRAN carriage control is added
C   --to the start of the line.  One blank line precedes the listing and
C   --follows any page break.  Pages (if any) are 60 lines long, with no
C   --paging before the first page.
C   --
C   --Limitations:
C   --   Writes only the first 128 characters from an input file record.
C   --
C   --Parameters:
C   --   IUNITI - IN integer - the input file unit number
C   --   IUNITO - IN integer - the output file unit number
C   --   FLAG - IN char*(*) - the type indicator:
C   --      (only the first character is checked; case is unimportant)
C   --      ' '      to simply echo the file
C   --      'NUMBER' to echo the file with line numbers
C   --      'PAGE'   to echo the file with line numbers and headers and paging

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER IUNITI, IUNITO
      CHARACTER*(*) FLAG

C   --Definitions for functions called:
      INTEGER ISTRLEN

      LOGICAL NUMBER, PAGE
      INTEGER NLINES
      INTEGER LSTPAG
      INTEGER IDUM
      CHARACTER*128 LINE
      CHARACTER*1 TMPFLG

      TMPFLG = FLAG(1:1)
      CALL STRUPCASE (TMPFLG)
      NUMBER = (TMPFLG .EQ. 'N') .OR. (TMPFLG .EQ. 'P')
      PAGE = (TMPFLG .EQ. 'P')

C   --Zero out the line numbers
      NLINES = 0
      LSTPAG = NLINES

C   --Write header
      WRITE (IUNITO, *, ERR=110)

  100 CONTINUE
C   --Read a line
      READ (IUNITI, '(A)', ERR=110, END=110) LINE

      IF (PAGE) THEN
C      --Page if needed
         IF (NLINES-LSTPAG .GE. 60) THEN
            IF (NLINES .GT. 0) THEN
               LSTPAG = NLINES
               WRITE (IUNITO, 10000, IOSTAT=IDUM)
               WRITE (IUNITO, *, IOSTAT=IDUM)
            END IF
         END IF
      END IF

C   --Write the line
      NLINES = NLINES + 1
      IF (NUMBER) THEN
         WRITE (IUNITO, 10010, IOSTAT=IDUM) NLINES, LINE(:ISTRLEN(LINE))
      ELSE
         WRITE (IUNITO, 10020, IOSTAT=IDUM) LINE(:ISTRLEN(LINE))
      END IF
      GOTO 100

  110 CONTINUE
C   --Rewind file before returning
      REWIND (IUNITI, IOSTAT=IDUM)

      RETURN
10000  FORMAT ('1')
10010  FORMAT (1X, I5, 2X, A)
10020  FORMAT (1X, A)
      END
