C=======================================================================
      SUBROUTINE EXCMDLIN (LINE)
C=======================================================================
C     FORTRAN EXTENSION LIBRARY - UNIX

C   --*** EXCMDLIN *** (CAMSUPES_LIB) Get command line
C   --   Written by Amy Gilkey - revised 09/02/94
C   --
C   --EXCMDLIN reads the arguments from the command line and appends
C   --them into a single line.
C   --
C   --Parameters:
C   --   LINE - OUT char*(*) - the returned command line
C
C   --Machine Dependencies:
C   --   IARGC and GETARG and LNBLNK are called.

C      USE DFLIB
      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER LINE*(*)
     
      INTEGER IARGC, LNBLNK

      CHARACTER STR*132
      INTEGER NNARGS, I, L

      NNARGS = IARGC ()

      IF (NNARGS .GT. 0) THEN

         LINE = ' '

         DO 100 I = 1, NNARGS
            CALL GETARG (I, STR)
            IF (I .LE. 1) THEN
               LINE = STR
            ELSE
               L = LNBLNK (LINE)
               LINE(L+1:) = ' ' // STR
            END IF
  100    CONTINUE

      ELSE
         LINE = ' '
      END IF

      RETURN
      END
