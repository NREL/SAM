C     Last change:  SLD   5 Dec 96    3:32 pm
C****************************************************************
C SUBROUTINE RMCNPI PROVIDED INITIAL CONTROL FOR THE PSEUDO-RANDOM
C NUMBER SEQUENCE FOR THE MCNP RANDOM NUMBER GENERATOR.
C
      SUBROUTINE RMCNPI(ISEED)
C
C     TAKEN FROM MCNP, AUGUST 1990
C
C     Changes for DLL compatibility
      USE FIRSTS, ONLY: ISARG
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/MCNPRN/RANS,RANB,RANI,RANJ,NRN
C
      PARAMETER (P=16777216.,Q=1./P,FB=4867484.,FS=10256733.)
C
C     Changes for DLL compatibility - now uses module here.
C     DATA ISARG / 0 /
C
      IF(ISARG.GT.0) RETURN
      ISARG = 1
      NRN=0
C     -- SET COUNTER TO ISEED
      NRN = ISEED
c      R = DFLOAT(ISEED)
      R = DBLE(ISEED)
      RANI=AINT(R*Q)
      RANJ=R-RANI*P
      RANB=RANI
      RANS=RANJ
      RETURN
C
      END
