C****************************************************************
C FUNCTION HYPGEO IS USED IN GENERATING A BETA DISTRIBUTION
C
      FUNCTION HYPGEO(A,B,C,X,SUM)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     FUNCTION HYPGEO SUMS THE SERIES FOR F(A,B,C,X) WHEN SUM=1. OR
C     F(A,B,C,X)-1. WHEN SUM=0. WHERE F(A,B,C,X) IS THE GAUSS
C     HYPERGEOMETRIC FUNCTION.
C     UPDATED, JANUARY, 1975
c
c     2-14-96 changes:
c     changes made to replace call to function R1MACH for value R1MACH(4)
c     with calls to LF90 routines DIGITS and RADIX
c     R1MACH(4) = B**(1-T), the largest relative spacing
c     value of R1MACH(4) was set = 1.19E-07 for IBM-compatible PCs in
c     function R1MACH
      Real RARG, DGTS, R1MACH4
      Data RARG / 0.0 /
c
      KMAX=1000
c
      DGTS = DIGITS(RARG)                 ! convert integer result to real
      R1MACH4 = RADIX(RARG)**(1.0-DGTS)   ! convert integer result to real
c      TOL=MAX(1.0E-14,R1MACH(4))         ! changed 2-14-96
      TOL=MAX(1.0E-14,R1MACH4)
c
      M=2.-SUM
      FSUM=X*A*(B/C)
      TK=FSUM
      TLIM=TOL
      IF(M.EQ.2) TLIM=TLIM*ABS(FSUM)
      AK=1.
      DO 10 K=2,KMAX
      TK=TK*((A+AK)/(AK+1.))*((B+AK)/(C+AK))*X
      FSUM=FSUM+TK
      IF(ABS(TK).LE.TLIM) GO TO 20
      AK=AK+1.
   10 CONTINUE
      CALL ERRCHK(35,'IN HYPGEO, NO CONVERGENCE OF SERIES')
      If(KLLERR) Return
      RETURN
   20 HYPGEO=FSUM+SUM
      RETURN
      END
