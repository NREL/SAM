C=======================================================================
      SUBROUTINE EXABORT
C=======================================================================
C     FORTRAN EXTENSION LIBRARY - UNIX

C   --*** EXABORT *** (CAMSUPES_LIB) Abort program
C   --   Written by Amy Gilkey - revised 07/18/95
C   --
C   --EXABORT aborts the program.

C   --Machine Dependencies:
C   --   Calls ABORT

      IMPLICIT NONE

      CALL ABORT
      END
