C=======================================================================
      SUBROUTINE DELVAR (NVAR, IVAR,
     &   IXIVO, VMEANO, STDEVO, DIAGO, CORRO, CORDVO,
     &   NIV, IXIV, VMEAN, STDEV, DIAG, CORR, CORDV)
C=======================================================================

C   --*** DELVAR *** (STEPWISE) Copy selected variables into new arrays
C   --   Modified by Amy Gilkey - revised 10/14/92
C   --
C   --DELVAR copies the values for selected variables into new arrays.
C   --The values include the variable index, the mean, the standard
C   --deviation, and the correlation values.
C   --
C   --Parameters:
C   --   NVAR - IN - the number of selected independent variables
C   --   IVAR - IN - the IXIVO index of the selected independent variables
C   --   IXIVO - IN - the indices of the selected independent variables
C   --   VMEANO - IN - the mean for each selected independent variable
C   --      (ORIGINAL selection)
C   --   STDEVO - IN - the standard deviation for each selected independent
C   --      variable (ORIGINAL selection)
C   --   DIAGO - IN - the cross product ? for all selected independent variables
C   --      (ORIGINAL selection)
C   --   CORRO - IN - the correlation matrix for all selected independent
C   --      variables (ORIGINAL selection)
C   --   CORDVO - IN - the correlation for all selected independent variables
C   --      with the dependent variable (ORIGINAL selection)
C   --   NIV - OUT - the number of selected independent variables
C   --   IXIV - OUT - the indices of the selected independent variables
C   --   VMEAN - OUT - the mean for each selected independent variable
C   --   STDEV - OUT - the standard deviation for each selected independent
C   --      variable
C   --   DIAG - OUT - the cross product ? for all selected independent variables
C   --   CORR - OUT - the correlation matrix for all selected independent
C   --      variables
C   --   CORDV - OUT - the correlation for all selected independent variables
C   --      with the dependent variable

      IMPLICIT NONE

      INTEGER NVAR
      INTEGER IVAR(*)
      INTEGER IXIVO(*)
      DOUBLE PRECISION VMEANO(*)
      DOUBLE PRECISION STDEVO(*)
      DOUBLE PRECISION DIAGO(*)
      DOUBLE PRECISION CORRO(*)
      DOUBLE PRECISION CORDVO(*)
      INTEGER IXIV(*)
      DOUBLE PRECISION VMEAN(*)
      DOUBLE PRECISION STDEV(*)
      DOUBLE PRECISION DIAG(*)
      DOUBLE PRECISION CORR(*)
      DOUBLE PRECISION CORDV(*)

      INTEGER NIV
      INTEGER IV, IV1, IV2
      INTEGER I1, I2

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D

      NIV = NVAR

      DO 100 IV = 1, NIV
         IXIV(IV) = IXIVO(IVAR(IV))
         VMEAN(IV) = VMEANO(IVAR(IV))
         STDEV(IV) = STDEVO(IVAR(IV))
         DIAG(IV) = DIAGO(IVAR(IV))
  100 CONTINUE

      DO 120 IV1 = 1, NIV
         DO 110 IV2 = 1, IV1
            I1 = MAX (IVAR(IV1), IVAR(IV2))
            I2 = MIN (IVAR(IV1), IVAR(IV2))
            CORR(IXSYM(IV1,IV2)) = CORRO(IXSYM(I1,I2))
  110    CONTINUE
  120 CONTINUE
      DO 130 IV = 1, NIV
         CORDV(IV) = CORDVO(IVAR(IV))
  130 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_DELVAR.FOR
C *2     3-MAR-1996 11:36:04 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:03 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_DELVAR.FOR
