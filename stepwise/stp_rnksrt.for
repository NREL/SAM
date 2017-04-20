C=======================================================================
      SUBROUTINE RNKSRT (N, X, RANK)
C=======================================================================

C   --*** RNKSRT *** Assign ranks to array and sort
C   --   Modified by Amy Gilkey - revised 02/19/96
C   --
C   --RNKSRT assigns ranks to an array based on an ascending sort.  Equal
C   --values are assigned an average of the corresponding ranks.
C   --RNKSRT returns the sorted array.
C   --
C   --Parameters:
C   --   N - IN - the number of values in X
C   --   X - IN/OUT - the values to be ranked, returned sorted 
C   --   RANK - OUT - the returned ranks for X

      IMPLICIT NONE

      INTEGER N
      DOUBLE PRECISION X(N)
      DOUBLE PRECISION RANK(N)

      INTEGER I, II, J
      INTEGER NTIES
      DOUBLE PRECISION AVG

C   --Call sort routine heepa
      CALL HEEPD (N, X)

      DO 120 I = 1, N
         RANK(I) = DFLOAT(I)
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
         AVG = AVG / DFLOAT(NTIES)
         DO 190 J = I, II-1
            RANK(J) = AVG
  190    CONTINUE
         GOTO 130
      END IF
  200 CONTINUE

      RETURN
      END

C=======================================================================
      SUBROUTINE HEEPD (N, X)
C=======================================================================

C   --*** HEEPD *** Sort array 
C   --   Modified by Amy Gilkey - revised 02/19/96
C   --
C   --HEEPD performs an ascending sort.
C   --
C   --Parameters:
C   --   N - IN - the number of values in the array
C   --   X - IN/OUT - the array to be sorted

      IMPLICIT NONE

      INTEGER N
      DOUBLE PRECISION X(N)

      INTEGER L, IR, I, J
      DOUBLE PRECISION XHOLD

      L = N/2 + 1
      IR = N
  100 CONTINUE
      IF (L .LE. 1) GOTO 140
      L = L - 1
      XHOLD = X(L)
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
      GOTO 120
  130 CONTINUE
      X(I) = XHOLD
      GOTO 100
  140 CONTINUE
      XHOLD = X(IR)
      X(IR) = X(1)
      IR = IR - 1
      IF (IR .GT. 1) GOTO 110
      X(1) = XHOLD

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_RNKSRT.FOR
C *1     3-MAR-1996 11:40:35 APGILKE "Convert to double precision"
C CMS REPLACEMENT HISTORY, Element STP_RNKSRT.FOR
