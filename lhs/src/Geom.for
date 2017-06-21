C     Last change:  S    17 Feb 98    1:23 pm
C**********************************************************
C SUBROUTINE GEOM GENERATES A GEOMETRIC DISTRIBUTION
C
      SUBROUTINE GEOM(J)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C     INCLUDE 'CWORKX.INC'                                              GDW-96  
      USE CWORKX                        
C
C      This statement removed to make modules work - gdw-96
C      DIMENSION XTABLE(MAXTB,2)
C      EQUIVALENCE (XTABLE(1,1),XX(1))
      DOUBLE PRECISION PDT,PINPUT,PINPL,PCOMPL,STOTAL
C
      LOC(I,J) = (J-1)*N+I
C
C     --- READ THE INPUT PARAMETER AND GENERATE A DISCRETE CUMULATIVE
C     --- DISTRIBUTION BASED ON THAT PARAMETER
C
      READ (8) AAA
C
      II=0
      TOTAL=0.0
      STOTAL = 0.0D0
      DELTA=1.0/(MAXTB-1)
      TLIMIT=1.0-DELTA-DELTA
      PINPUT=AAA
      PINPL=LOG(PINPUT)
      PCOMPL=LOG(1.0D0-PINPUT)
C
      DO 100 I=0, 9999999
         PDT= I*PCOMPL + PINPL
         PDT= EXP(PDT)
         STOTAL = STOTAL + PDT
         IF (STOTAL .GE. DELTA) THEN
            TOTAL=TOTAL+STOTAL
            II=II+1
            XTABLE(II,1)=I
            XTABLE(II,2)=TOTAL
            IF (TOTAL .GT. TLIMIT) GO TO 101
            STOTAL=0.0D0
         END IF
  100 CONTINUE
C
C     --- IF PROGRAM GETS HERE, THE CREATION OF THE CUMULATIVE FUNCTION
C     --- WAS UNSUCCESSFUL, SO WRITE A MESSAGE AND STOP.
      WRITE (6,999) 'CREATION OF A GEOMETRIC DISTRIBUTION WAS ',
     1 'NOT SUCCESSFUL.  THE INPUT PARAMETER WAS TOO SMALL.'
      WRITE (99,999) 'CREATION OF A GEOMETRIC DISTRIBUTION WAS ',
     1 'NOT SUCCESSFUL.  THE INPUT PARAMETER WAS TOO SMALL.'
  999 FORMAT ('1',5X,A,A)
      KLLERR = .TRUE.
      RETURN
C
  101 XTABLE(II,2)=1.0
C
      PROBINC=1.0/N
      IF (IRS .NE. 0) PROBINC=1.0
      STRTPT = 0.0
      IMIN = 1
C
      DO I = 1, N
         PROB = PROBINC * RNUM1() + STRTPT
         CALL INTRPD(PROB, BX, XTABLE, MAXTB, IMIN, II)
         If(KLLERR) Return
         X(LOC(I,J)) = BX
         IF (IRS .EQ. 0) THEN
            STRTPT = REAL(I) / REAL(N)
         ELSE
            IMIN = 1
         END IF
      END DO
C
      RETURN
C
      END
