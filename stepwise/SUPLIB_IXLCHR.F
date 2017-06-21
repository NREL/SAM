      INTEGER FUNCTION IXLCHR (CHRVAR)
C
************************************************************************
C
C     FORTRAN Extension Library - UNIX
C
C     DESCRIPTION:
C     This function returns the absolute location of a character variable.
C     This location must be measured in character storage units.
C
C     FORMAL PARAMETERS:
C     CHRVAR    CHARACTER       Character Variable
C
C     INTRINSICS CALLED:
C     LOC       INTEGER         Address of a variable
C
************************************************************************
C
      IMPLICIT NONE
C
      CHARACTER*(*) CHRVAR
C
      IXLCHR = LOC (CHRVAR)
C
      END
