C     Last change:  S    17 Feb 98    1:14 pm
C****************************************************************
C SUBROUTINE SSPEV IS USED IN THE POSITIVE DEFINITE CHECK OF THE
C CORRELATION MATRIX
C
      SUBROUTINE SSPEV(A,N,E,V,LDV,WORK,JOB,INFO)
C***BEGIN PROLOGUE  SSPEV
C***DATE WRITTEN   800808   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D4A1
C***KEYWORDS  EISPACK,EIGENVALUES,EIGENVECTORS,SYMMETRIC,REAL,PACKED
C***AUTHOR  KAHANER, K. K., (NBS)

C           MOLER, C. B., (U. OF NEW MEXICO)
C           STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  TO COMPUTE THE EIGENVALUES AND, OPTIONALLY, THE EIGEN-
C            VECTORS OF A REAL SYMMETRIC MATRIX STORED IN PACKED FORM.
C***DESCRIPTION
C     LICEPACK.  THIS VERSION DATED 08/08/80.
C     DAVID KAHANER, CLEVE MOLER, PETE STEWART
C          N.B.S.       U.N.M.     N.B.S./U.MD.
C
C     ABSTRACT
C      SSPEV COMPUTES THE EIGENVALUES AND, OPTIONALLY, THE EIGENVECTORS
C      OF A REAL SYMMETRIC MATRIX STORED IN PACKED FORM.
C
C     CALL SEQUENCE PARAMETERS-
C       (THE VALUES OF PARAMETERS MARKED WITH * (STAR) WILL BE  CHANGED
C         BY SSPEV.)
C
C        A*      REAL(N*(N+1)/2)
C                REAL SYMMETRIC PACKED INPUT MATRIX.  CONTAINS UPPER
C                TRIANGLE AND DIAGONAL OF A, BY COLUMN (ELEMENTS
C                11, 12, 22, 13, 23, 33, ...).
C
C        N       INTEGER
C                SET BY THE USER TO
C                THE ORDER OF THE MATRIX A.
C
C        E*      REAL(N)
C                ON RETURN FROM SSPEV, E CONTAINS THE EIGENVALUES OF A.
C                SEE ALSO INFO BELOW.
C
C        V*      REAL(LDV,N)
C                ON RETURN FROM SSPEV, IF THE USER HAS SET JOB
C                = 0        V IS NOT REFERENCED.
C                = NONZERO  THE N EIGENVECTORS OF A ARE STORED IN THE
C                FIRST N COLUMNS OF V.  SEE ALSO INFO BELOW.
C
C        LDV     INTEGER
C                SET BY THE USER TO
C                THE LEADING DIMENSION OF THE ARRAY V IF JOB IS ALSO
C                SET NONZERO.  IN THAT CASE, N MUST BE .LE. LDV.
C                IF JOB IS SET TO ZERO, LDV IS NOT REFERENCED.
C
C        WORK*   REAL(2N)
C                TEMPORARY STORAGE VECTOR.  CONTENTS CHANGED BY SSPEV.
C
C        JOB     INTEGER
C                SET BY THE USER TO
C                = 0        EIGENVALUES ONLY TO BE CALCULATED BY SSPEV.
C                           NEITHER V NOR LDV ARE REFERENCED.
C                = NONZERO  EIGENVALUES AND VECTORS TO BE CALCULATED.
C                           IN THIS CASE, A & V MUST BE DISTINCT ARRAYS.
C                           ALSO, IF LDA .GT. LDV, SSPEV CHANGES ALL THE
C                           ELEMENTS OF A THRU COLUMN N.  IF LDA < LDV,
C                           SSPEV CHANGES ALL THE ELEMENTS OF V THROUGH
C                           COLUMN N.  IF LDA=LDV, ONLY A(I,J) AND V(I,
C                           J) FOR I,J = 1,...,N ARE CHANGED BY SSPEV.
C
C       INFO*   INTEGER
C               ON RETURN FROM SSPEV, THE VALUE OF INFO IS
C               = 0 FOR NORMAL RETURN.
C               = K IF THE EIGENVALUE ITERATION FAILS TO CONVERGE.
C                   EIGENVALUES AND VECTORS 1 THROUGH K-1 ARE CORRECT.
C
C
C     ERROR MESSAGES-
C          NO. 1   RECOVERABLE  N IS GREATER THAN LDV AND JOB IS NONZERO
C          NO. 2   RECOVERABLE  N IS LESS THAN ONE
C
C     SUBROUTINES USED
C
C      EISPACK- IMTQL2, TQLRAT, TRBAK3, TRED3
C      SLATEC- XERROR
C***REFERENCES  (NONE)
C***ROUTINES CALLED  IMTQL2,TQLRAT,TRBAK3,TRED3,XERROR
C***END PROLOGUE  SSPEV
c
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      

      INTEGER I,INFO,J,K,LDV,M,N
      REAL A((N*(N+1))/2),E(N),V(LDV,N),WORK(1)
C***FIRST EXECUTABLE STATEMENT  SSPEV
       NV=(N*(N+1))/2
C      -- CALLS TO XERROR REPLACED 11/20/91 - GDW
       IF(N .GT. LDV) THEN
          WRITE (*,*) '***** FATAL ERROR IN SSPEV -- ',
     1                'N IS GREATER THAN LDV *****'
c****** add prints to error file and message file
          WRITE (6,*) '***** FATAL ERROR IN SSPEV -- ',
     1                'N IS GREATER THAN LDV *****'
          WRITE (99,*) '***** FATAL ERROR IN SSPEV -- ',
     1                'N IS GREATER THAN LDV *****'
c******
          KLLERR = .TRUE.
          RETURN
       ELSE IF(N .LT. 1) THEN
          WRITE (*,*) '***** FATAL ERROR IN SSPEV -- ',
     1                'N IS LESS THAN 1 *****'
c****** add prints to error file and message file
          WRITE (6,*) '***** FATAL ERROR IN SSPEV -- ',
     1                'N IS LESS THAN 1 *****'
          WRITE (99,*) '***** FATAL ERROR IN SSPEV -- ',
     1                'N IS LESS THAN 1 *****'
c******
          KLLERR = .TRUE.
          RETURN
       END IF
C
C       CHECK N=1 CASE
C
      E(1) = A(1)
      INFO = 0
      IF(N .EQ. 1) RETURN
C
      IF(JOB.NE.0) GO TO 20
C
C     EIGENVALUES ONLY
C
      CALL TRED3(N,NV,A,E,WORK(1),WORK(N+1))
      If(KLLERR) Return
      CALL TQLRAT(N,E,WORK(N+1),INFO)
      If(KLLERR) Return
      RETURN
C
C     EIGENVALUES AND EIGENVECTORS
C
   20 CALL TRED3(N,NV,A,E,WORK(1),WORK(1))
      If(KLLERR) Return
      DO 30 I = 1, N
        DO 25 J = 1, N
   25     V(I,J) = 0.
   30   V(I,I) = 1.
      CALL IMTQL2(LDV,N,E,WORK,V,INFO)
      If(KLLERR) Return
      M = N
      IF(INFO .NE. 0) M = INFO - 1
      CALL TRBAK3(LDV,N,NV,A,M,V)
      If(KLLERR) Return
      RETURN
      END
