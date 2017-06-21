C****************************************************************
C SUBROUTINE MATINV IS USED TO INVERT A LOWER TRIANGULAR MATRIX
C
      SUBROUTINE MATINV
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CWORKC.INC'                                              GDW-96  
      USE CWORKC                        
C
      LOC1(I,J)=J+(I*I-I)/2
C
      DO 10 I=1,NV
         Q(LOC1(I,I))=1.0/Q(LOC1(I,I))
   10 CONTINUE
C
      DO 100 K=NV, 2, -1
         DO 90 J=K-1, 1, -1
            TEMP=0.0
            JPLUS=J+1
            DO 40 I=JPLUS,K
               TEMP=TEMP+Q(LOC1(K,I))*Q(LOC1(I,J))
   40       CONTINUE
            Q(LOC1(K,J))=-TEMP*Q(LOC1(J,J))
   90    CONTINUE
  100 CONTINUE
C
      RETURN
      END
