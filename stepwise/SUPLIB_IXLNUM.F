      INTEGER FUNCTION IXLNUM (NUMVAR)
C
************************************************************************
C
C     FORTRAN Extension Library - UNIX
C
C     DESCRIPTION:
C     This function returns the absolute location of a numeric variable.
C     This location must be measured in numeric storage units.
C
C     FORMAL PARAMETERS:
C     NUMVAR    INTEGER         Numeric Variable
C
C     INTRINSICS CALLED:
C     LOC       INTEGER         Address of a variable
C
************************************************************************
C
      IMPLICIT NONE

      INTEGER NUMVAR

      INTEGER ILOC

C   --Combining these two statements did not seem to work
      ILOC = LOC (NUMVAR)
      IXLNUM = ILOC / 4

      END
