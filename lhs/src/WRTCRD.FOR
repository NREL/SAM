C****************************************************************
C SUBROUTINE WRTCRD PROCESSES THE DISTRIBUTION PARAMETER
C STATEMENTS
C
      SUBROUTINE WRTCRD(ITYPE,LABEL)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C
      CHARACTER LABEL*(*),LABVAR*(LENC),BLANK
      PARAMETER (BLANK=' ')
C
      NV=NV+1
      IDIST(NV)=ITYPE
      NC=LEN(LABEL)
      IC=0
      LABVAR=BLANK
  100 CONTINUE
      IC=IC+1
      IF(IC.GT.NC)GO TO 200
      IF(LABEL(IC:IC).EQ.BLANK)GO TO 100
      LABVAR=LABEL(IC:NC)
  200 CONTINUE
      WRITE(9)LABVAR
      RETURN
      END
