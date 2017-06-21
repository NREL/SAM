C****************************************************************
C SUBROUTINE TO GENERATE GAMMA DEVIATES WHEN THE SHAPE PARAMETER
C ALPHA < 1
C REFERENCE: D. J. BEST (1983). "A NOTE OF GAMMA VARIATE GENERATORS
C            WITH SHAPE PARAMETER LESS THAN UNITY," COMPUTING, 30, 185-188.
C
C FORMERLY NAMED BEST IN RON'S PROGRAM
C
      SUBROUTINE GAMMAB(ALPHA,X,Z,B)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
      DOUBLE PRECISION X,Z,B,P,Y
C
    1 P = B*RNUM1()
      US = RNUM1()
C
      IF (P.LE.1.) THEN
         X = Z*P**(1./ALPHA)
         IF (US .LE. (2.-X)/(2.+X)) RETURN
         IF (US .GT. EXP(-X)) GO TO 1
      ELSE
         X = -LOG(Z*(B-P)/ALPHA)
         Y = X/Z
         IF (US*(ALPHA + Y - ALPHA*Y) .LT. 1.) RETURN
         IF (US .GT. Y**(ALPHA-1.)) GO TO 1
      END IF
C
      RETURN
      END
