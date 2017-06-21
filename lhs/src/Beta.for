C     Last change:  S    26 Jan 98   10:14 am
C****************************************************************
C SUBROUTINE BETA IS USED TO GENERATE A BETA DISTRIBUTION ON THE
C INTERVAL (A,B) AND WITH PARAMETERS P AND Q
C
      SUBROUTINE BETA(J)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C     INCLUDE 'CWORKX.INC'                                              GDW-96  
      USE CWORKX                        
C
      COMMON /PQ/P,Q,NZ
C      This statement removed to make modules work - gdw-96
C      DIMENSION XTABLE(MAXTB,2)
C      EQUIVALENCE (XTABLE(1,1), XX(1))
      EXTERNAL BETAFN
C
      LOC(I,J) = (J-1)*N+I
C
      CALL ERXSET(10,0)
      If(KLLERR) Return
      PROBINC = 1./FLOAT(N)
      IF(IRS.NE.0)PROBINC=1.0
      READ (8)A,B,P,Q
      STRTPT = 0.
      ISIZE=100
      CALL TABLE(BETAFN,XTABLE,MAXTB,ISIZE)
      If(KLLERR) Return
      IMIN=1
      DO I = 1, N
         PROB = PROBINC*RNUM1()+ STRTPT
         CALL INTERP(PROB,BX,XTABLE,MAXTB,IMIN,ISIZE,0)
         If(KLLERR) Return
         X(LOC(I,J)) = A + (B-A)*BX
         IF (IRS .EQ. 0) THEN
            STRTPT = REAL(I) / REAL(N)
         ELSE
            IMIN=1
         END IF
      END DO
C
      RETURN
      END
