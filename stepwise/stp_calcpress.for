C=======================================================================
      SUBROUTINE CALCPRESS (NUMOBS, NUMVAR, NIV, RESI,
     &   VMEAN, STDEV, XINVER,
     &   IXIV, XRNK, WTVAR, WTS, PRESS, YDUM)
C=======================================================================

C   --*** CALCPRESS *** (STEPWISE) Calculate predicted error sum of squares
C   --
C   --   Modified by Cédric Sallaberry - 07/27/2010
C   --   add NUMVAR - IN - number of variables
C   --
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --CALCPRESS calculates the predicted error sum of squares.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the number of observations
C   --   NIV - IN - the number of selected independent variables
C   --   RESI - IN - the residuals
C   --   VMEAN - IN - the mean for each selected variable
C   --   STDEV - IN - the standard deviation for each selected variable
C   --   XINVER - IN - the inverse of the correlation matrix for all
C   --      selected variables
C   --   IXIV - IN - the indices of the selected independent variables
C   --   XRNK - IN - the variable data (rank if 'RANK', standard 0-1 if 'STD01')
C   --   WTVAR - IN - true iff a weighted regression analyis is needed
C   --   WTS - IN - the weights for the regression (if WTVAR)
C   --   PRESS - OUT - the returned predicted error sum of squares
C   --   YDUM - SCRATCH - size = NIV

      IMPLICIT NONE

      INTEGER NUMOBS
!     add type for NUMVAR
      INTEGER NUMVAR
      INTEGER NIV
      DOUBLE PRECISION RESI(NUMOBS)
      DOUBLE PRECISION VMEAN(NIV)
      DOUBLE PRECISION STDEV(NIV)
      DOUBLE PRECISION XINVER(*)
      INTEGER IXIV(NIV)
!     change XRNK size from (NUMOBS,NIV) to (NUMOBS,NUMVAR)      
      DOUBLE PRECISION XRNK(NUMOBS,NUMVAR)
      LOGICAL WTVAR
      DOUBLE PRECISION WTS(NUMOBS)
      DOUBLE PRECISION PRESS
      DOUBLE PRECISION YDUM(NIV)


      INTEGER IOBS
      INTEGER IV
      INTEGER IV1, IV2
      DOUBLE PRECISION SQROBS
      DOUBLE PRECISION QI
      DOUBLE PRECISION SUMQ
      DOUBLE PRECISION W12
      DOUBLE PRECISION YDUM1

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D

      PRESS = 0.0
      SQROBS = SQRT (DFLOAT(NUMOBS-1))
      DO 130 IOBS = 1, NUMOBS
         DO 100 IV = 1, NIV
            YDUM(IV) = (XRNK(IOBS,IXIV(IV)) - VMEAN(IV))
     &         / (STDEV(IV) * SQROBS)
  100    CONTINUE
         QI = 0.0
         DO 120 IV2 = 1, NIV
            SUMQ = 0.0
            DO 110 IV1 = 1, NIV
               IF (IV2 .LE. IV1) THEN
                  W12 = XINVER(IXSYM(IV1,IV2))
               ELSE
                  W12 = XINVER(IXSYM(IV2,IV1))
               END IF
               SUMQ = SUMQ + YDUM(IV1)*W12
  110       CONTINUE
            QI = QI + SUMQ*YDUM(IV2)
  120    CONTINUE
         QI = QI + 1.0/DFLOAT(NUMOBS)
         YDUM1 = RESI(IOBS) / (1.0-QI)
         IF (WTVAR) THEN
            PRESS = PRESS + WTS(IOBS)*YDUM1*YDUM1
         ELSE
            PRESS = PRESS + YDUM1*YDUM1
         END IF
  130 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_CALCPRESS.FOR
C *2     3-MAR-1996 11:35:48 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:23:59 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_CALCPRESS.FOR
