C=======================================================================
      SUBROUTINE INVERTSYM (N, A, INVERR)
C=======================================================================

C   --*** INVERTSYM *** (STEPWISE) Invert symmetrically stored matrix
C   --   Modified by Amy Gilkey - revised 10/08/91
C   --
C   --INVERTSYM inverts a symmetrically stored matrix in place.
C   --
C   --Parameters:
C   --   N - IN - the number of rows/columns in the matrix
C   --   A - IN/OUT - the symmetrically stored matrix to be inverted
C   --   INVERR - OUT - 0 if able to invert the matrix without problems;
C   --      <0 if warning; >0 if error

      IMPLICIT NONE

      INTEGER N
      DOUBLE PRECISION A(*)
      INTEGER INVERR

      INTEGER IV1, IV2, IV3
      DOUBLE PRECISION DIN
      DOUBLE PRECISION WORK

      INTEGER IXSYM
      INTEGER I, J
      IXSYM(I,J) = (I*I-I)/2 + J

C   --Call CKINVSYM to perform error checking on matrix to be inverted
      CALL CKINVSYM (N, A, INVERR)

      DO 120 IV1 = N, 1, -1
         DIN = 1.0 / A(IXSYM(IV1,IV1))
         A(IXSYM(IV1,IV1)) = DIN
         DO 110 IV2 = N, IV1+1, -1
            WORK = 0.0
            DO 100 IV3 = IV1+1, IV2
               WORK = WORK + A(IXSYM(IV2,IV3))*A(IXSYM(IV3,IV1))
  100       CONTINUE
            A(IXSYM(IV2,IV1)) = -WORK * DIN
  110    CONTINUE
  120 CONTINUE

      DO 150 IV1 = 1, N
         DO 140 IV2 = IV1, N
            WORK = 0.0
            DO 130 IV3 = IV2, N
               WORK = WORK + A(IXSYM(IV3,IV2))*A(IXSYM(IV3,IV1))
  130       CONTINUE
            A(IXSYM(IV2,IV1)) = WORK
  140    CONTINUE
  150 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_INVERTSYM.FOR
C *2     3-MAR-1996 11:36:11 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:04 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_INVERTSYM.FOR
