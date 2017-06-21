C=======================================================================
      INTEGER FUNCTION ISTRFIND (STR, LENLST, STRLST)
C=======================================================================

C   --*** ISTRFIND *** (CAMCON_LIB) Find string in list
C   --   Written by Amy Gilkey - revised 03/26/93
C   --
C   --ISTRFIND returns the index of the given string in a list of strings.
C   --If the string is not in the list, ISTRFIND returns 0.
C   --
C   --Parameters:
C   --   STR - IN char*(*) - the string to be searched for
C   --   LENLST - IN integer - the number of strings in the list
C   --   STRLST - IN char*(*) array - the list of strings to be searched

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STR
      INTEGER LENLST
      CHARACTER*(*) STRLST(LENLST)

      DO 100 ISTRFIND = 1, LENLST
         IF (STR .EQ. STRLST(ISTRFIND)) GOTO 110
  100 CONTINUE
      ISTRFIND = 0

  110 CONTINUE
      RETURN
      END
