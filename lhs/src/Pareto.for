C     Last change:  S    26 Jan 98   10:40 am
C****************************************************************
C SUBROUTINE PARETO GENERATES PARETO DISTRIBUTIONS
C WITH PARAMETERS ALPHA > 2 AND BETA > 0
C
      SUBROUTINE PARETO(J)
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
      PROBINC=1./FLOAT(N)
      IF (IRS .EQ. 1) PROBINC=1.0
      READ (8) ALPHA,BETA
      STRTPT=0.
      DO I = 1, N
         R = PROBINC*RNUM1()+STRTPT
         RES = BETA/(1.-R)**(1./ALPHA)
         X(LOC(I,J)) = MAX( RES, 1.0E-10)
         IF (IRS == 0) STRTPT = REAL(I) / REAL(N)
      END DO
C
      RETURN
      END
