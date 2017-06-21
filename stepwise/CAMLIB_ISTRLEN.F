C=======================================================================
      INTEGER FUNCTION ISTRLEN (STRING)
C=======================================================================

C   --*** ISTRLEN *** (CAMCON_LIB) Return string length
C   --   Written by Amy Gilkey - revised 03/26/93
C   --
C   --ISTRLEN returns the length of a given string.  There can be blanks
C   --embedded within the string.
C   --
C   --Parameters:
C   --   STRING - IN char*(*) - the string

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STRING

      INTEGER N

      ISTRLEN = LEN (STRING)
C   --Check for invalid string
      IF (ISTRLEN .LE. 0) RETURN

      IF (STRING(ISTRLEN:ISTRLEN) .EQ. ' ') THEN
         N = INDEX (STRING, ' ')
  100    CONTINUE
         IF (STRING(N:) .NE. ' ') THEN
            N = N + INDEX (STRING(N+1:), ' ')
            GOTO 100
         END IF
         ISTRLEN = N-1
      END IF

      RETURN
      END
