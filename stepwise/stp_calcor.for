C=======================================================================
      SUBROUTINE CALCOR (IUNOUT, NUMOBS, NMODVR, IXMODL, LABEL,
     &   XYRNK, WTVAR, WTS,
     &   VMEAN, STDEV, SSCP, DIAG, CORR)
C=======================================================================

C   --*** CALCOR *** (STEPWISE) Calculate means, correlation matrix, etc.
C   --   Modified by Amy Gilkey - revised 10/11/92
C   --
C   --CALCOR calculates the means and standard deviation for all the
C   --independent and dependent variables in the model.  It also calculates
C   --the correlation matrix for all the variables in the model.
C   --
C   --Parameters:
C   --   IUNOUT - IN - the listing file unit number
C   --   NUMOBS - IN - the number of observations
C   --   NMODVR - IN - the number of selected independent and dependent
C   --      variables
C   --   IXMODL - IN - the indices of the selected independent and dependent
C   --      variables
C   --   LABEL - IN - the variable labels
C   --   XYRNK - IN - the variable data (rank if 'RANK')
C   --   WTVAR - IN - true iff a weighted regression analyis is needed
C   --   WTS - IN - the weights for the regression (if WTVAR)
C   --   VMEAN - OUT - the mean for each selected variable
C   --   STDEV - OUT - the standard deviation for each selected variable
C   --   SSCP - OUT - the sum of the squares and cross product for all
C   --      selected variables
C   --   DIAG - OUT - the cross product ? for all selected variables
C   --   CORR - OUT - the correlation matrix for all selected variables

      IMPLICIT NONE

      INCLUDE 'stp_title_common.inc'
      INCLUDE 'stp_print_options_common.inc'

      INTEGER IUNOUT
      INTEGER NUMOBS
      INTEGER NMODVR
      INTEGER IXMODL(*)
      CHARACTER*8 LABEL(*)
      DOUBLE PRECISION XYRNK(NUMOBS,*)
      LOGICAL WTVAR
      DOUBLE PRECISION WTS(NUMOBS)
      DOUBLE PRECISION VMEAN(NMODVR)
      DOUBLE PRECISION STDEV(NMODVR)
      DOUBLE PRECISION SSCP(*)
      DOUBLE PRECISION DIAG(NMODVR)
      DOUBLE PRECISION CORR(*)

      INTEGER IV
      INTEGER IOBS
      INTEGER IV1, IV2
      INTEGER IDUM
      DOUBLE PRECISION XSUM
      DOUBLE PRECISION XNUM
      DOUBLE PRECISION SS
      DOUBLE PRECISION B1, B2
      DOUBLE PRECISION SQROBS
      DOUBLE PRECISION VAR
      DOUBLE PRECISION STERR
      DOUBLE PRECISION CV
      DOUBLE PRECISION B

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D


C   --Calculate means

      DO 110 IV = 1, NMODVR
         XSUM = 0.0
         DO 100 IOBS = 1, NUMOBS
            XNUM = XYRNK(IOBS,IXMODL(IV))
            IF (WTVAR) THEN
               XSUM = XSUM + WTS(IOBS)*XNUM
            ELSE
               XSUM = XSUM + XNUM
            END IF
  100    CONTINUE
         VMEAN(IV) = XSUM / DFLOAT(NUMOBS)
  110 CONTINUE

C   --Calculate sum of squares and cross products

      DO 140 IV1 = 1, NMODVR
         DO 130 IV2 = 1, IV1
            SS = 0.0
            DO 120 IOBS = 1, NUMOBS
               B1 = XYRNK(IOBS,IXMODL(IV1)) - VMEAN(IV1)
               B2 = XYRNK(IOBS,IXMODL(IV2)) - VMEAN(IV2)
               IF (WTVAR) THEN
                  SS = SS + WTS(IOBS)*B1*B2
               ELSE
                  SS = SS + B1*B2
               END IF
  120       CONTINUE
            SSCP(IXSYM(IV1,IV2)) = SS
  130    CONTINUE
  140 CONTINUE

C   --Calculate variance, standard deviation, standard error, and
C   --coefficient of variation

      SQROBS = SQRT (DFLOAT(NUMOBS))

      IF (PRMEAN) THEN
         CALL QAPAGE (IUNOUT, 'PAGE')
         CALL QAMESSAG (IUNOUT, 'CENTER', TITLE)
         WRITE (IUNOUT, 10000, IOSTAT=IDUM)
      END IF
      DO 150 IV1 = 1, NMODVR
         DIAG(IV1) = SSCP(IXSYM(IV1,IV1))
         VAR = DIAG(IV1) / DFLOAT(NUMOBS-1)
         STDEV(IV1) = SQRT (VAR)
         STERR = STDEV(IV1) / SQROBS
         IF (VMEAN(IV1) .NE. 0.0) THEN
            CV = 100.0 * STDEV(IV1) / VMEAN(IV1)
         ELSE
            CV = 0.0
         END IF
         IF (PRMEAN) THEN
            WRITE (IUNOUT, 10010, IOSTAT=IDUM) LABEL(IXMODL(IV1)),
     &         VMEAN(IV1), VAR, STDEV(IV1), STERR, CV
         END IF
  150 CONTINUE

      IF (PRSSCP) THEN
         CALL QAPAGE (IUNOUT, 'PAGE')
         CALL QAMESSAG (IUNOUT, 'CENTER', TITLE)
         CALL PRTSYMMAT (IUNOUT, 'E', 'SUM OF SQUARES MATRIX',
     &      NMODVR, IXMODL, LABEL, SSCP)
      END IF

C   --Calculate correlation matrix

      DO 170 IV1 = 1, NMODVR
         DO 160 IV2 = 1, IV1-1
            B = DIAG(IV1) * DIAG(IV2)
            IF (B .NE. 0.0) THEN
               CORR(IXSYM(IV1,IV2)) = SSCP(IXSYM(IV1,IV2)) / SQRT (B)
            ELSE
               CORR(IXSYM(IV1,IV2)) = 0.0
            END IF
  160    CONTINUE
         B = DIAG(IV1)
         IF (B .NE. 0.0) THEN
            CORR(IXSYM(IV1,IV1)) = SSCP(IXSYM(IV1,IV1)) / B
         ELSE
            CORR(IXSYM(IV1,IV1)) = 0.0
         END IF
  170 CONTINUE

      IF (PRCORR) THEN
         CALL QAPAGE (IUNOUT, 'PAGE')
         CALL QAMESSAG (IUNOUT, 'CENTER', TITLE)
         CALL PRTSYMMAT (IUNOUT, 'F', 'CORRELATION MATRIX',
     &      NMODVR, IXMODL, LABEL, CORR)
      END IF

      RETURN

10000  FORMAT (/,
     &   1X, 'VARIABLE', 3X, '    MEAN   ', 3X, '  VARIANCE ',
     &   3X, '  STD DEV  ', 3X, '  STD ERR  ', 3X, ' COEFFofVAR', /,
     &   1X, '--------', 3X, ' ----------', 3X, ' ----------',
     &   3X, ' ----------', 3X, ' ----------', 3X, ' ----------')
10010  FORMAT (1X, A8, 1P5E14.4)
      END
C CMS REPLACEMENT HISTORY, Element STP_CALCOR.FOR
C *2     3-MAR-1996 11:35:44 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:23:59 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_CALCOR.FOR
