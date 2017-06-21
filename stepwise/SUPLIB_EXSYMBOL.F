C=======================================================================
      SUBROUTINE EXSYMBOL (SYMNAM, SYMVAL, LENSYM)
C=======================================================================
C     FORTRAN Extension Library - UNIX 

C   --*** EXSYMBOL *** (CAMSUPES_LIB) Get system symbol
C   --   Written by Amy Gilkey - revised 09/22/94
C   --
C   --EXSYMBOL gets the value of a system symbol.
C   --
C   --Parameters:
C   --   SYMNAM - IN char*(*) - the symbol name
C   --   SYMVAL - OUT char*(*) - the returned symbol value
C   --   LENSYM - OUT integer - the length of the returned symbol value;
C   --      <0 if not defined.

C   --Machine Dependencies:
C   --   GETENV gets value of environment variable.
C   --   LNBLNK returns string length.

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) SYMNAM
      CHARACTER*(*) SYMVAL
      INTEGER LENSYM

      INTEGER LNBLNK

      CALL GETENV (SYMNAM, SYMVAL)
      LENSYM = LNBLNK (SYMVAL)
      IF (LENSYM .LE. 0) LENSYM = -1

      RETURN
      END
