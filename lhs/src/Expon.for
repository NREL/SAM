C     Last change:  S    26 Jan 98   10:37 am
C****************************************************************
C SUBROUTINE EXPON GENERATES EXPONENTIAL DISTRIBUTIONS
C WITH PARAMETER LAMBDA
C
      SUBROUTINE EXPON(J,IDT)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C
      LOC(I,J)=(J-1)*N+I
C
      IF (IDT .EQ. 25) THEN
C        -- TRUNCATED EXPONENTIAL
         READ (8) RLAMBD, A, B
      ELSE IF (IDT .EQ. 26) THEN
C        -- BOUNDED EXPONENTIAL - COMPUTE QUANTILES FOR A AND B
         READ (8) RLAMBD, A, B
         A = 1.0 - EXP(-RLAMBD*A)
         B = 1.0 - EXP(-RLAMBD*B)
      ELSE
C        -- UNBOUNDED EXPONENTIAL - QUANTILES ARE 0.0 AND 1.0
         READ (8) RLAMBD
         A = 0.0
         B = 1.0
      END IF
C
      PROBINC = (B - A) / FLOAT(N)
      IF (IRS .EQ. 1) PROBINC = B - A
      STRTPT = A
      DO I = 1, N
         R=PROBINC*RNUM1()+STRTPT
         X(LOC(I,J))=-LOG(1.0-R)/RLAMBD
         IF (IRS == 0) STRTPT = REAL(I) / REAL(N)
      END DO
C
      RETURN
      END
