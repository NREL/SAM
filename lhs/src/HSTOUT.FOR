C****************************************************************
C SUBROUTINE HSTOUT IS USED TO GENERATE HISTOGRAMS OF THE
C VARIABLES
C
      SUBROUTINE HSTOUT
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C     INCLUDE 'CRANK.INC'                                               GDW-96  
      USE CRANK                         
C     INCLUDE 'DISTNM.INC'                                              GDW-96  
      USE DISTNM                        
C
      LOC(I,J)=(J-1)*N+I
C
      NNV=N*NV
      DO 10 I=1, NNV
         X(I)=XSAVE(I)
   10 CONTINUE
      DO 590 I=1,NV
         IDT=IDIST(I)
         WRITE(6,9001)TITLE
         WRITE(6,9002)I,DIST(IDSST(IDT):IDSEND(IDT))
         DO 530 J=1,N
            XV(J)=X(LOC(J,I))
  530    CONTINUE
         CALL HISTO
         If(KLLERR) Return
  590 CONTINUE
C
      RETURN
C
 9001 FORMAT('1',3X,A)
 9002 FORMAT('0','  HISTOGRAM FOR VARIABLE NO.',I3,5X,
     1        A,'DISTRIBUTION')
C
      END
