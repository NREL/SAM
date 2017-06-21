C=======================================================================
      SUBROUTINE STRPACK (STRING, LSTR)
C=======================================================================

C   --*** STRPACK *** (CAMCON_LIB) Remove all blanks from string
C   --   Written by Amy Gilkey - revised 03/26/93
C   --
C   --STRPACK left justifies a string and removes all embedded blanks.
C   --
C   --Parameters:
C   --   STRING - IN/OUT char*(*) - the string, returned packed, may be up
C   --      up to 132 characters long
C   --   LSTR - OUT integer - the length of the packed string
C
C   --Routines Called:
C   --   ISTRLEN - (CAMCON_LIB) Find string length

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STRING
      INTEGER LSTR

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER IBLK
      CHARACTER*128 TMPSTR

      LSTR = ISTRLEN (STRING)
      IF (LSTR .LE. 1) RETURN

      IBLK = INDEX (STRING, ' ')
  100 CONTINUE
      IF ((IBLK .GT. 0) .AND. (IBLK .LT. LSTR)) THEN
         TMPSTR = STRING(IBLK+1:LSTR)
         STRING(IBLK:LSTR) = TMPSTR
         LSTR = LSTR - 1
         IBLK = INDEX (STRING, ' ')
         GOTO 100
      END IF

      RETURN
      END
