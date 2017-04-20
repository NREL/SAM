C****************************************************************
C SUBROUTINE CHLSKY COMPUTES THE CHOLESKY FACTORIZATION OF A
C CORRELATION MATRIX STORED IN LOWER TRIANGULAR SYMMETRIC FORM
C
      SUBROUTINE CHLSKY
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C     INCLUDE 'CWORKC.INC'                                              GDW-96  
      USE CWORKC                        
C
      LOC1(I,J)=J+(I*I-I)/2
C
      NVX=(NV*(NV+1))/2
      DO 70 I1=1,NVX
         Q(I1)=0.0
   70 CONTINUE
      I1=1
      DO 80 I2=1,NV
         Q(LOC1(I2,I1))=CORR(LOC1(I2,I1))
   80 CONTINUE
C
      DO 200 I1=2, NV
         I1MIN=I1-1
         DO 100 I2=1,I1MIN
            Q(LOC1(I1,I1))=Q(LOC1(I1,I1))+Q(LOC1(I1,I2))**2
  100    CONTINUE
         Q(LOC1(I1,I1))=SQRT(1.0-Q(LOC1(I1,I1)))
         IF (I1.LT.NV) THEN
            IPLUS=I1+1
            DO 120 I2=IPLUS,NV
               DO 110 K=1,I1MIN
                  Q(LOC1(I2,I1))=Q(LOC1(I2,I1))+
     1                    Q(LOC1(I2,K))*Q(LOC1(I1,K))
  110          CONTINUE
               Q(LOC1(I2,I1))=(CORR(LOC1(I2,I1))-Q(LOC1(I2,I1)))/
     1                    Q(LOC1(I1,I1))
  120       CONTINUE
         END IF
  200 CONTINUE
C
      RETURN
      END
