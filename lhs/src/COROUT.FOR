C****************************************************************
C SUBROUTINE COROUT WILL OUTPUT THE RAW AND RANK CORRELATION
C MATRICES OF THE SAMPLE IF REQUESTED
C
      SUBROUTINE COROUT
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
C     INCLUDE 'CRANK.INC'                                               GDW-96  
      USE CRANK                         
C
      LOC(I,J)=(J-1)*N+I
C
      NNV=N*NV
      DO 10 I=1, NNV
         X(I)=XSAVE(I)
   10 CONTINUE
      DO 600 I=1,NV
  600 LCM(I)=I
      CALL CORCAL
      If(KLLERR) Return
      CALL PMTRX(NV,1)
      If(KLLERR) Return
c      IF(N.GT.NV.AND.ICM.EQ.0)CALL VIF
      IF(N.GT.NV.AND.ICM.EQ.0) THEN
         CALL VIF
         If(KLLERR) Return
      ENDIF
      DO 620 J=1,NV
        DO 610 I=1,N
  610   XV(I)=X(LOC(I,J))
        CALL RANKER
        If(KLLERR) Return
        DO 620 I=1,N
        X(LOC(I,J))=RXV(I)
  620 CONTINUE
      CALL CORCAL
      If(KLLERR) Return
      CALL PMTRX(NV,2)
      If(KLLERR) Return
      IF(N.GT.NV.AND.ICM.EQ.0)CALL VIF
      RETURN
      END
