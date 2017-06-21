C****************************************************************
C SUBROUTINE FINVNOR IS USED IN GENERATING THE NORMAL AND
C LOGNORMAL DISTRIBUTIONS
C
      FUNCTION FINVNOR (X)
      IF (X-0.5) 20,10,30
   10 FINVNOR=0.0
      RETURN
   20 F=-1.0
      Y=X
      GO TO 40
   30 F=1.0
      Y=1.0-X
   40 FINVNOR=SQRT(2.0)*F*RIERFC1(2.*Y)
      RETURN
      END
