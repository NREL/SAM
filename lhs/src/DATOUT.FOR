C****************************************************************
C SUBROUTINE DATOUT WILL OUTPUT THE SAMPLE AND ITS RANKS IF
C REQUESTED
C
      SUBROUTINE DATOUT
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CRANK.INC'                                               GDW-96  
      USE CRANK                         
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C
      LOC(I,J)=(J-1)*N+I
C
      NNV=N*NV
      DO 10 I=1, NNV
         X(I)=XSAVE(I)
   10 CONTINUE
      CALL OUTDAT(0)
      If(KLLERR) Return
      DO 620 J=1,NV
        DO 610 I=1,N
  610   XV(I)=X(LOC(I,J))
        CALL RANKER
        If(KLLERR) Return
        DO 620 I=1,N
        X(LOC(I,J))=RXV(I)
  620 CONTINUE
      CALL OUTDAT(1)
      If(KLLERR) Return
      RETURN
      END
