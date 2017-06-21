C=======================================================================
      LOGICAL FUNCTION EXCANCEL (RESET)
C=======================================================================
C     FORTRAN EXTENSION LIBRARY - UNIX

C   --*** EXCANCEL *** Cancel function
C   --   Written by Colin Selleck - revised 06/01/95
C   --
C   --EXCANCEL checks whether the cancel flag has been set and returns it.

C   --Machine Dependencies:
C   --   Always returns false.

      IMPLICIT NONE

      LOGICAL RESET

      EXCANCEL = .FALSE.

      RETURN
      END
