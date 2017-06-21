C     Last change:  SLD  28 Feb 97    2:23 pm
C****************************************************************
C SUBROUTINE CORCAL COMPUTES A CORRELATION MATRIX FROM THE SAMPLE
C
      SUBROUTINE CORCAL
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                   
c
      USE LOCALVARS, ONLY: XM, SSQ
C
C     To make compatible with modules - gdw-96
c     Moved to the LOCALVARS module
C      DIMENSION XM(NVAR),SSQ(NVAR)
C      REAL, ALLOCATABLE :: XM(:), SSQ(:)
C
      LOC(I,J)=(J-1)*N+I
      LOC1(I,J)=J+(I*I-I)/2
C
c     Added for modules
c     Moved to the LOCALVARS module
C      ALLOCATE( XM(NVAR), SSQ(NVAR) )
C
      IF(NV.EQ.1)THEN
        CORR(1)=1.0
c       Added for modules
c     Moved to the LOCALVARS module
C        DEALLOCATE( XM, SSQ )
        RETURN
      ENDIF
      DO 10 I=1,NV
        XM(I)=0.0
        SSQ(I)=0.0
   10 CONTINUE
      K=(NV*(NV+1))/2
      DO 20 I=1,K
         CORR(I)=0.0
   20 CONTINUE
      FN=N
C
C     COMPUTE THE MEAN
C
      DO 30 J=1,NV
         DO 30 K=1,N
            XM(J)=XM(J)+X(LOC(K,J))
   30 CONTINUE
      DO 40 J=1,NV
         XM(J)=XM(J)/FN
   40 CONTINUE
C
C     SUBTRACT THE MEAN FROM ALL OF THE ELEMENTS OF THE MATRIX
C     AND COMPUTE THE SUM OF SQUARES
C
      DO 50 J=1,NV
         DO 50 K=1,N
           X(LOC(K,J))=X(LOC(K,J))-XM(J)
           SSQ(J)=SSQ(J)+X(LOC(K,J))*X(LOC(K,J))
   50 CONTINUE
C
C     COMPUTE THE CORRELATION
C
      DO 70 I=2,NV
         IL1=I-1
         DO 70 J=1,IL1
            DO 60 K=1,N
               CORR(LOC1(I,J))=CORR(LOC1(I,J))+X(LOC(K,I))*X(LOC(K,J))
   60       CONTINUE
   70 CONTINUE
C
      DO 80 I=2,NV
         IL1=I-1
         DO 80 J=1,IL1
            IF (CORR(LOC1(I,J)) .NE. 0.0)
     1         CORR(LOC1(I,J))=CORR(LOC1(I,J))/SQRT(SSQ(I)*SSQ(J))
   80 CONTINUE
C
      DO 90 I=1,NV
   90    CORR(LOC1(I,I))=1.0
C
c     Added for modules
c     Moved to the LOCALVARS module
c      DEALLOCATE( XM, SSQ )
C      
      RETURN
      END
