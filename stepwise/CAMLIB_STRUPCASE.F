C=======================================================================
      SUBROUTINE STRUPCASE (STRING)
C=======================================================================

C   --*** STRUPCASE *** (CAMCON_LIB) Convert string to upper-case
C   --   Written by Amy Gilkey - revised 02/27/93
C   --
C   --STRUPCASE converts the passed string to upper-case letters.
C   --
C   --Parameters:
C   --   STRING - IN/OUT char*(*) - the input string; the returned upper-case
C   --      string

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STRING

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER I, K

      CHARACTER*26 UPPER, LOWER
      SAVE UPPER, LOWER

      DATA UPPER / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      DATA LOWER / 'abcdefghijklmnopqrstuvwxyz' /

      DO 100 I = 1, ISTRLEN (STRING)
         K = INDEX (LOWER, STRING(I:I))
         IF (K .GE. 1) STRING(I:I) = UPPER(K:K)
  100 CONTINUE

      RETURN
      END
