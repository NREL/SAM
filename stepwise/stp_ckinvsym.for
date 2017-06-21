C=======================================================================
      SUBROUTINE CKINVSYM (N, A, INVERR)
C=======================================================================

C   --*** CKINVSYM *** (STEPWISE) Check if can invert sym-stored matrix
C   --   Modified by Amy Gilkey - revised 10/08/91
C   --
C   --CKINVSYM checks if a symmetrically stored matrix can be inverted.
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
      INTEGER IDUM
      DOUBLE PRECISION TOL
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DPIV
      CHARACTER*128 TXTMSG

      INTEGER IXSYM
      INTEGER I, J
      IXSYM(I,J) = (I*I-I)/2 + J

      INVERR = 0

      DO 120 IV1 = 1, N
         TOL = ABS (0.01 * A(IXSYM(IV1,IV1)))
         DO 110 IV2 = IV1, N
            DSUM = 0.0
            DO 100 IV3 = IV1-1, 1, -1
               DSUM = DSUM + A(IXSYM(IV1,IV3))*A(IXSYM(IV2,IV3))
  100       CONTINUE
            DSUM = A(IXSYM(IV2,IV1)) - DSUM
            IF (IV2 .EQ. IV1) THEN
               IF (DSUM .LE. TOL) THEN
                  IF (DSUM .LE. 0.0) THEN
                     WRITE (TXTMSG, 10000, IOSTAT=IDUM)
     &                  'Matrix is singular at row', IV1
10000                 FORMAT (A, I5)
                     CALL QAMESSAG (-1, '+ERROR', TXTMSG)
                     DSUM = TOL / 1000.0
                     INVERR = 1
                  ELSE
                     WRITE (TXTMSG, 10000, IOSTAT=IDUM)
     &                  'Matrix inversion rounding error in row', IV1-1
                     CALL QAMESSAG (-1, '+WARNING', TXTMSG)
                     IF (INVERR .EQ. 0) INVERR = -1
                  END IF
               END IF
               DPIV = SQRT (DSUM)
               A(IXSYM(IV2,IV1)) = DPIV
               DPIV = 1.0 / DPIV
            ELSE
               A(IXSYM(IV2,IV1)) = DSUM * DPIV
            END IF
  110    CONTINUE
  120 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_CKINVSYM.FOR
C *2     3-MAR-1996 11:35:54 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:02 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_CKINVSYM.FOR
