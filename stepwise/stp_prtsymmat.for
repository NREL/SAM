C=======================================================================
      SUBROUTINE PRTSYMMAT (IUNOUT, FMTTYP, HEAD, NVAR, IXVAR,
     &   LABEL, ARY)
C=======================================================================

C   --*** PRTSYMMAT *** (STEPWISE) Print a half-stored matrix
C   --   Modified by Amy Gilkey - revised 10/11/92
C   --
C   --PRTSYMMAT prints the lower half of a half-stored matrix.
C   --
C   --Parameters:
C   --   IUNOUT - IN - the listing file unit number
C   --   FMTTYP - IN - 'F' for F format, 'E' for E format
C   --   HEAD - IN - the name of the table
C   --   NVAR - IN - the number of variables in matrix
C   --   IXVAR - IN - the indices of the selected variables
C   --   LABEL - IN - the names of all variables
C   --   ARY - IN - the half-stored matrix

      IMPLICIT NONE

      INTEGER IUNOUT
      CHARACTER*1 FMTTYP
      CHARACTER*(*) HEAD
      INTEGER NVAR
      INTEGER IXVAR(*)
      CHARACTER*8 LABEL(*)
      DOUBLE PRECISION ARY(*)

      INTEGER IBEG, IEND
      INTEGER IDUM
      CHARACTER*30 FMT

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D

      IF (FMTTYP .NE. 'F') THEN
         FMT = '(1X,A8,1P10E11.3)'
      ELSE
         FMT = '(1X,A8,10F11.4)'
      END IF

      CALL QAMESSAG (IUNOUT, '+CENTER', HEAD)

      DO 110 IBEG = 1, NVAR, 10
         IEND = MIN (NVAR, IBEG+10-1)
         WRITE (IUNOUT, *)
         DO 100 I = IBEG, NVAR
            WRITE (IUNOUT, FMT) LABEL(IXVAR(I)),
     &         (ARY(IXSYM(I,J)), J=IBEG,MIN(I,IEND))
  100    CONTINUE
         WRITE (IUNOUT, 10000, IOSTAT=IDUM)
     &      (LABEL(IXVAR(I)), I=IBEG,IEND)
  110 CONTINUE

      RETURN
10000  FORMAT (1X, 8X, 11 (3X, A8))
      END
C CMS REPLACEMENT HISTORY, Element STP_PRTSYMMAT.FOR
C *2     3-MAR-1996 11:36:14 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:07 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_PRTSYMMAT.FOR
