C     Last change:  S    17 Feb 98    1:12 pm
C****************************************************************
C SUBROUTINE POSDEF IS USED IN THE POSITIVE DEFINITE CHECK
C OF THE CORRELATION MATRIX
C
      SUBROUTINE POSDEF(ITEST)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C
C     These statements removed to make modules work - GDW-96
C     COMMON/PDMAT/Z(NVAR,NVAR),D(NVAR)
      USE PDMAT
c
      USE LOCALVARS, ONLY: WK
C
C     DIMENSION WK((NVAR*(NVAR+1))/2)
c     Moved to the LOCALVARS module
C      REAL, ALLOCATABLE :: WK(:)
C     NN = THE DIMENSIONS OF Z,  I.E.  Z(NN,NN).
C     DATA NN/NVAR/
C
C
C     EIG = THE VALUE THAT THE NEGATIVE EIGENVALUES ARE SET EQUAL TO.
      DATA EIG/0.001/
C
C       These statements added to make modules work - GDW-96
C       Allocate and initialize this work array
c       Moved to the LOCALVARS module
C        ALLOCATE( WK((NVAR*(NVAR+1))/2) )
C        WK = 0.0
C       NN = THE DIMENSIONS OF Z,  I.E.  Z(NN,NN).
        NN = NVAR
C
C     M = MAXIMUM NUMBER OF ITERATIONS ALLOWED.
      M=20
C
      NP=NCM
c      NKX=(NP*(NP+1))/2       -----  removed 1-12-96, not used
      ITEST=0
      ICONV=0
  100 CONTINUE
      ITEST=ITEST+1
      IF(ITEST.GT.M)THEN
        WRITE(6,1000)
        WRITE(99,1000)
        KLLERR = .TRUE.
        RETURN
      ENDIF
      REWIND 3
      WRITE(3)CORR
      CALL SSPEV(CORR,NP,D,Z,NN,WK,1,INFO)
      If(KLLERR) Then
         Return
      END If
      CALL FINDIT(NP,NN,EIG,ICONV)
      If(KLLERR) Return
      IF(ICONV.EQ.0)GO TO 100
      REWIND 3
      READ(3)CORR
C
c      Moved to the LOCALVARS module
C      DEALLOCATE( WK )
C
      RETURN
 1000 FORMAT(1H1,'THE INPUT RANK CORRELATION MATRIX IS NOT POSITIVE ',
     1       'DEFINITE.',/,' AN ITERATIVE PROCEDURE HAS FAILED TO ',
     2       'PRODUCE A POSITIVE DEFINITE MATRIX AFTER 20 ITERATIONS.',
     3       /,' THEREFORE, THE PROGRAM HAS BEEN TERMINATED.',/,' THE',
     4       ' USER NEEDS TO REEVALUATE THE RELATIONSHIP OF THE ',
     5       'CORRELATED VARIABLES IN THE MATRIX.')
      END
C****************************************************************
C FUNCTION PYTHAG IS USED IN THE POSTIVE DEFINITE CHECK OF THE
C CORRELATION MATRIX
C
      REAL FUNCTION PYTHAG(A,B)
C***BEGIN PROLOGUE  PYTHAG
C***REFER TO  EISDOC
C     FINDS SQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW
C***ROUTINES CALLED    (NONE)
C***END PROLOGUE  PYTHAG
      REAL A,B
C
      REAL P,Q,R,S,T
C***FIRST EXECUTABLE STATEMENT  PYTHAG
      P = MAX(ABS(A),ABS(B))
      Q = MIN(ABS(A),ABS(B))
      IF (Q .EQ. 0.0E0) GO TO 20
   10 CONTINUE
         R = (Q/P)**2
         T = 4.0E0 + R
         IF (T .EQ. 4.0E0) GO TO 20
         S = R/T
         P = P + 2.0E0*P*S
         Q = Q*S
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
