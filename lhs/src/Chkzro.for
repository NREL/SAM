C     Last change:  S    17 Feb 98    1:16 pm
C****************************************************************
C SUBROUTINE CHKZRO CHECKS TO MAKE SURE THAT THE MINIMUM
C REQUIREMENTS FOR A SAMPLE HAVE BEEN MET
C
      SUBROUTINE CHKZRO(N,NV,IRSET)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
      IF(N.EQ.0)THEN
        WRITE(6,9001)
        WRITE(99,9001)
        KLLERR = .TRUE.
        RETURN
c      -- it is now legal to have zero variables if all information
c      -- is constants, so remove this check and make modifications
c      -- in the main program to bypass sampling.
c      ELSE IF(NV.EQ.0)THEN
c        WRITE(6,9002)
c        STOP 'CHKZRO'
      ELSE IF(IRSET.EQ.0)THEN
        WRITE(6,9003)
        WRITE(99,9003)
        KLLERR = .TRUE.
        RETURN
      ENDIF
      RETURN
 9001 FORMAT('1',5X,'THE NUMBER OF OBSERVATIONS HAS NOT BEEN ',
     1       'SPECIFIED')
 9002 FORMAT('1',5X,'NO VARIABLES HAVE BEEN SPECIFIED')
 9003 FORMAT('1',5X,'A RANDOM SEED HAS NOT BEEN SPECIFIED')
      END
