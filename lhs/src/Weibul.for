C     Last change:  S    26 Jan 98   10:38 am
C****************************************************************
C SUBROUTINE WEIBUL GENERATES WEIBULL DISTRIBUTIONS
C WITH PARAMETERS ALPHA AND BETA
C
      SUBROUTINE WEIBUL(J)
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
      A = 1./ALPHA
      STRTPT=0.
      DO I = 1, N
         R=PROBINC*RNUM1()+STRTPT
         RES = ( (-LOG(1.-R)) ** A ) * BETA
         X(LOC(I,J)) = MAX( RES, 1E-10)
         IF (IRS == 0) STRTPT = REAL(I) / REAL(N)
      END DO
C
      RETURN
      END
