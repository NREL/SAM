C     Last change:  S    26 Jan 98   10:24 am
C****************************************************************
C SUBROUTINE CUMULD IS USED TO GENERATE USER-SPECIFIED CUMULATIVE
C DISCRETE DISTRIBUTION
C
      SUBROUTINE CUMULD(J)
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
C      These statements removed to make modules work - GDW-96
C      DIMENSION XTABLE(MAXTB,2)
C      EQUIVALENCE (XTABLE(1,1), XX(1))
C
      LOC(I,J) = (J-1)*N+I
C
      PROBINC = 1./FLOAT(N)
      IF (IRS .NE. 0) PROBINC=1.0
      READ (8) NP
      READ (8) (XTABLE(III,1),XTABLE(III,2),III=1,NP)
      STRTPT = 0.
      IMIN=1
      DO I = 1, N
         PROB = PROBINC*RNUM1()+ STRTPT
         CALL INTRPD(PROB,BX,XTABLE,MAXTB,IMIN,NP)
         If(KLLERR) Return
         X(LOC(I,J)) = BX
         IF(IRS.EQ.0) THEN
            STRTPT = REAL(I) / REAL(N)
         ELSE
            IMIN=1
         END IF
      END DO
C
      RETURN
      END
