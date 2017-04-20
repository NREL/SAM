C     Last change:  S    17 Feb 98    1:15 pm
C****************************************************************
C SUBROUTINE OUTCRD PROCESSES THE OUTPUT PARAMETER OPTIONS
C
      SUBROUTINE OUTCRD(CARD)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C
      CHARACTER CARD*(LENC),PDATA*4,PHIST*4,PCORR*4,BLANK
      PARAMETER (PDATA='DATA',PHIST='HIST',PCORR='CORR',BLANK=' ')
C
      IC=7
   10 CONTINUE
      IC=IC+1
      IF(IC.GT.LENC)GO TO 20
      IF(CARD(IC:IC).EQ.BLANK)GO TO 10
      IE=IC+3
      IF(CARD(IC:IE).EQ.PDATA)THEN
        IDATA=1
        IC=IE+1
        GO TO 10
      ELSE IF(CARD(IC:IE).EQ.PHIST)THEN
        IHIST=1
        IC=IE+1
        GO TO 10
      ELSE IF(CARD(IC:IE).EQ.PCORR)THEN
        ICORR=1
        IC=IE+1
        GO TO 10
      ELSE
        WRITE(6,9001)CARD
        WRITE(99,9001)CARD
        KLLERR = .TRUE.
        RETURN
      ENDIF
   20 CONTINUE
      RETURN
 9001 FORMAT('1',5X,'THE FOLLOWING OUTPUT OPTION CARD REQUESTED ',
     1       'AN UNDEFINED OUTPUT OPTION',/,6X,'PLEASE CHECK THE ',
     2       'USER MANUAL FOR THE CORRECT OUTPUT OPTION CARD ',
     3       'SYNTAX',//,3X,'***',A,'***')
      END
