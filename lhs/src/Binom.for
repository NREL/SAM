C     Last change:  S    26 Jan 98   10:33 am
C**********************************************************
C SUBROUTINE BINOM GENERATES A BINOMIAL DISTRIBUTION
C
      SUBROUTINE BINOM(J)
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
C      These statements removed to make modules work - GDW-96
C      DIMENSION XTABLE(MAXTB,2)
C      EQUIVALENCE (XTABLE(1,1),XX(1))
      DOUBLE PRECISION PDT,PINPUT,PINPL,PCOMPL,STOTAL
      DOUBLE PRECISION FACTOR,FACTR2
C
      LOC(I,J) = (J-1)*N+I
C
C     --- READ THE INPUT PARAMETER AND GENERATE A DISCRETE CUMULATIVE
C     --- DISTRIBUTION BASED ON THAT PARAMETER
C
      READ (8) AAA, NTOT
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
      DO 100 I=0, NTOT
         ITMP=NTOT-I
         PDT= FACTOR(I,NTOT) - FACTR2(1,ITMP) + I*PINPL + ITMP*PCOMPL
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
