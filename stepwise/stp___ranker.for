C=======================================================================
      SUBROUTINE RANKER (N, X, RANK, IWORK)
C=======================================================================

C   --*** RANKER *** Assign ranks to array
C   --   Modified by Amy Gilkey - revised 02/19/96
C   --
C   --RANKER assigns ranks to an array based on an ascending sort.  Equal
C   --values are assigned an average of the corresponding ranks.
C   --
C   --Parameters:
C   --   N - IN - the number of values in X
C   --   X - IN - the values to be ranked 
C   --   RANK - OUT - the returned ranks for X
C   --   IWORK - SCRATCH - size = N

      IMPLICIT NONE

      INTEGER N
      REAL X(N)
      REAL RANK(N)
      INTEGER IWORK(N)

      INTEGER LOCINT

      LOGICAL SORTIT
      INTEGER I, II, J, K
      INTEGER NTIES
      REAL AVG
      REAL XHOLD
      REAL RHOLD

      DO 110 I = 1, N
         IWORK(I) = I
  110 CONTINUE
C   --Call sort routine heepa
      CALL HEEPA (N, X, IWORK)

      DO 120 I = 1, N
         RANK(I) = FLOAT(I)
  120 CONTINUE
C   --Find ties
      II = 1
  130 CONTINUE
      IF (II .LT. N) THEN
         DO 140 I = II, N-1
            IF (X(I) .EQ. X(I+1)) GOTO 150
  140    CONTINUE
         GOTO 200
  150    CONTINUE
C      --Count ties
         DO 160 II = I+2,  N
            IF (X(I) .NE. X(II)) GOTO 170
  160    CONTINUE
C      --Average tied ranks
  170    CONTINUE
         AVG = 0.0
         DO 180 J = I, II-1
            AVG = AVG + RANK(J)
  180    CONTINUE
         NTIES = II - I
         AVG = AVG / FLOAT(NTIES)
         DO 190 J = I, II-1
            RANK(J) = AVG
  190    CONTINUE
         GOTO 130
      END IF
  200 CONTINUE

C   --Reorder array to original order
         DO 210 I = N, 2, -1
            K = LOCINT (I, I, IWORK)
            XHOLD = X(K)
            RHOLD = RANK(K)
            X(K) = X(I)
            RANK(K) = RANK(I)
            IWORK(K) = IWORK(I)
            X(I) = XHOLD
            RANK(I) = RHOLD
            IWORK(I) = I
  210    CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE HEEPA (N, X, IX)
C=======================================================================

C   --*** HEEPA *** Sort array with indices
C   --   Modified by Amy Gilkey - revised 11/26/90
C   --
C   --HEEPA performs an ascending sort carrying corresponding integer values.
C   --
C   --Parameters:
C   --   N - IN - the number of values in the array
C   --   X - IN/OUT - the array to be sorted
C   --   IX - IN/OUT - the integer array corresponding to X

      IMPLICIT NONE

      INTEGER N
      REAL X(N)
      INTEGER IX(N)

      INTEGER L, IR, I, J
      INTEGER IXHOLD
      REAL XHOLD

      L = N/2 + 1
      IR = N
  100 CONTINUE
      IF (L .LE. 1) GOTO 140
      L = L - 1
      XHOLD = X(L)
      IXHOLD = IX(L)
  110 CONTINUE
      J = L
  120 CONTINUE
      I = J
      J = 2 * J
      IF (J .GT. IR) GOTO 130
      IF (J .LT. IR) THEN
         IF (X(J) .LT. X(J+1)) J = J + 1
      END IF
      IF (XHOLD .GE. X(J)) GOTO 130
      X(I) = X(J)
      IX(I) = IX(J)
      GOTO 120
  130 CONTINUE
      X(I) = XHOLD
      IX(I) = IXHOLD
      GOTO 100
  140 CONTINUE
      XHOLD = X(IR)
      IXHOLD = IX(IR)
      X(IR) = X(1)
      IX(IR) = IX(1)
      IR = IR - 1
      IF (IR .GT. 1) GOTO 110
      X(1) = XHOLD
      IX(1) = IXHOLD

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP___RANKER.FOR
C *2     3-MAR-1996 11:37:03 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:25 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP___RANKER.FOR
