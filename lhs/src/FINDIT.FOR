C     Last change:  SLD   5 Dec 96    8:16 am
C****************************************************************
C SUBROUTINE FINDIT IS USED IN THE POSITIVE DEFINITE CHECK
C OF THE CORRELATION MATRIX
C
      SUBROUTINE FINDIT(NP,M,EIG,ICONV)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C
C     These statements removed to make modules work - GDW-96
C     COMMON/PDMAT/Z(NVAR,NVAR),D(NVAR)
      USE PDMAT
C
C
      LOC(I,J)=(J-1)*N+I
      NEV = 0
      DO 9 I = 1,NP
         IF (D(I) .LT. 0.0) NEV=NEV+1
    9 CONTINUE
C
      IF (NEV .EQ. 0) THEN
C
         ICONV=1
C
      ELSE
C
         DO 11 I = 1,NEV
            D(I)=EIG
   11    CONTINUE
C
         L1=NEV+1
         L2=NEV+NEV
         DO 12 I = L1,L2
            IF(D(I).LT.EIG)D(I)=EIG
   12    CONTINUE
C
         DO 4 I = 1,M
         DO 4 J = 1,M
            X(LOC(I,J)) = 0.0
    4    CONTINUE
C
         DO 5 I = 1,NP
         DO 5 J = 1,NP
         DO 5 K = 1,NP
            X(LOC(I,J)) = X(LOC(I,J)) + Z(I,K)*D(K)*Z(J,K)
    5    CONTINUE
C
         DO 30 I = 1,NP
            X(LOC(I,I)) = 1.0
   30    CONTINUE
C
         KI = 0
         DO 10 I = 1,NP
         DO 10 J = 1,I
            KI = KI + 1
            CORR(KI) = X(LOC(I,J))
   10    CONTINUE
C
      ENDIF
C
      RETURN
      END
