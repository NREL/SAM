C=======================================================================
      SUBROUTINE QAABORT (ERRMSG)
C=======================================================================

C   --*** QAABORT *** (CAMCON_LIB) Abort program
C   --   Written by Amy Gilkey - revised 10/17/95
C   --
C   --QAABORT prints a fatal error message to the terminal and to the
C   --diagnostics file (if defined).  It then aborts the program.
C   --
C   --Parameters:
C   --   ERRMSG - IN char*(*) - the error message

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) ERRMSG

C   --Definitions for functions called:
      INTEGER IQAERRUNI

      IF (ERRMSG .NE. ' ') THEN
         CALL QAMESSAG (0, '+FATAL', ERRMSG)
         IF (IQAERRUNI (-1) .GT. 0) THEN
            CALL QAMESSAG (0, 'CONT',
     &         '(see output file for more information)')
            CALL QAMESSAG (-1, '+FATAL', ERRMSG)
         END IF
      END IF

      CALL QAMESSAG (0, ' ', '%%% Program is aborting %%%')

      CALL EXABORT

      END
