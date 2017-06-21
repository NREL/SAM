C=======================================================================
      LOGICAL FUNCTION QABATCH ()
C=======================================================================

C   --*** QABATCH *** (CAMCON_LIB) Return batch versus interactive flag
C   --   Written by Amy Gilkey - revised 05/02/93
C   --
C   --QABATCH returns true if the program is in batch mode and false if it is
C   --in interactive mode.  A spawned process is treated as batch.
C
C   --Routines Called:
C   --   EXPARM - (CAMSUPES_LIB) Get batch vs. interactive flag

      IMPLICIT NONE

      INTEGER IMODE
      INTEGER IDUM
      CHARACTER CDUM8*8

      LOGICAL FIRST, SVBATC
      SAVE FIRST, SVBATC

      DATA FIRST / .TRUE. /

      IF (FIRST) THEN
         CALL EXPARM (CDUM8, CDUM8, IMODE, IDUM, IDUM, IDUM)
         SVBATC = (IMODE .NE. 1)
         FIRST = .FALSE.
      END IF

      QABATCH = SVBATC

      RETURN
      END
