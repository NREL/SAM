C=======================================================================
      SUBROUTINE REGRANAL (
     &  IUNOUT, NUMOBS, NUMVAR, NIV, NDV, IXMODL,
     &  LABEL, XYRAW, XYRNK, WTVAR, WTS,
     &  VMEAN, STDEV, DIAG, CORR, INPUT_NAMES, 
     &  SRC_VALUES, COEFF_DETERM, NBSTEPS )
C=======================================================================

C   --*** REGRANAL *** (STEPWISE) Perform regression analysis on all variables
C   --   Modified by Cédric Sallaberry - 07/27/2010
C   --   add NUMVAR - IN - number of variables
C
C   --   Modified by Amy Gilkey - revised 11/18/96
C   --
C   --REGRANAL performs regression analysis on each selected dependent variable
C   --with all selected independent variables.
C   --
C   --Parameters:
C   --   IUNOUT - IN - the listing file unit number
C   --   NUMOBS - IN - the number of observations
C   --   NIV - IN - the number of selected independent variables
C   --   NDV - IN - the number of selected dependent variables
C   --   IXMODL - IN - the indices of the selected independent variables
C   --   LABEL - IN - the variable labels
C   --   XYRAW - IN - the raw variable data
C   --   XYRNK - IN - the variable data (rank if 'RANK',
C   --      standard 0-1 if 'STD01')
C   --   WTVAR - IN - true iff a weighted regression analyis is needed
C   --   WTS - IN - the weights for the regression (if WTVAR)
C   --   VMEAN - IN - the mean for each selected variable
C   --   STDEV - IN - the standard deviation for each selected variable
C   --   DIAG - IN - the cross product ? for all selected variables
C   --   CORR - IN - the correlation matrix for all selected variables

      IMPLICIT NONE

      INCLUDE 'stp_title_common.inc'
      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_print_options_common.inc'

      INTEGER IUNOUT
      INTEGER NUMOBS
!     add type of NUMVAR      
      INTEGER NUMVAR
      INTEGER NIV, NDV
      INTEGER IXMODL(*)
      CHARACTER*8 LABEL(*)
      DOUBLE PRECISION XYRAW(NUMOBS,*)
!     redefine XYRNK size from (NUMOBS,*) to (NUMOBS, NUMVAR)      
      DOUBLE PRECISION XYRNK(NUMOBS,NUMVAR)
      LOGICAL WTVAR
      DOUBLE PRECISION WTS(NUMOBS)
      DOUBLE PRECISION VMEAN(*)
      DOUBLE PRECISION STDEV(*)
      DOUBLE PRECISION DIAG(*)
      DOUBLE PRECISION CORR(*)

      INTEGER IDEP
      INTEGER IVDEP
      INTEGER IXDEP
      INTEGER MAXRSQ

      INTEGER, ALLOCATABLE :: IVAR(:)
      INTEGER, ALLOCATABLE :: IXIVS(:)
      INTEGER, ALLOCATABLE :: IXRSQ(:)
      DOUBLE PRECISION, ALLOCATABLE :: TIME(:) 
      DOUBLE PRECISION, ALLOCATABLE :: YHAT(:) 
      DOUBLE PRECISION, ALLOCATABLE :: RESI(:) 
      DOUBLE PRECISION, ALLOCATABLE :: YHARAW(:) 
      DOUBLE PRECISION, ALLOCATABLE :: RESRAW(:) 
      DOUBLE PRECISION, ALLOCATABLE :: PSTP(:) 
      DOUBLE PRECISION, ALLOCATABLE :: RANK(:) 
      DOUBLE PRECISION, ALLOCATABLE :: YSORT(:) 
      DOUBLE PRECISION, ALLOCATABLE :: BETAS(:) 
      DOUBLE PRECISION, ALLOCATABLE :: BETA(:) 
      DOUBLE PRECISION, ALLOCATABLE :: PRESS(:) 
      DOUBLE PRECISION, ALLOCATABLE :: VMEANB(:) 
      DOUBLE PRECISION, ALLOCATABLE :: STDEVB(:) 
      DOUBLE PRECISION, ALLOCATABLE :: DIAGB(:) 
      DOUBLE PRECISION, ALLOCATABLE :: CORRB(:) 
      DOUBLE PRECISION, ALLOCATABLE :: CORDVB(:) 
      DOUBLE PRECISION, ALLOCATABLE :: XINVER(:) 
      DOUBLE PRECISION, ALLOCATABLE :: RSQ(:) 

      CHARACTER*8          :: INPUT_NAMES(NUMVAR)
      DOUBLE PRECISION     :: COEFF_DETERM(NUMVAR)
      DOUBLE PRECISION     :: SRC_VALUES(NUMVAR)
      INTEGER              :: NBSTEPS   

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D


      ALLOCATE (TIME(NUMOBS))
      ALLOCATE (YHAT(NUMOBS))
      ALLOCATE (RESI(NUMOBS))
      IF (DATTYP .EQ. 'RANK') THEN
         ALLOCATE (YHARAW(NUMOBS))
         ALLOCATE (RESRAW(NUMOBS))
      ELSE
         ALLOCATE (YHARAW(1))
         ALLOCATE (RESRAW(1))
      END IF
      IF (DATTYP .NE. 'RANK') THEN
         ALLOCATE (PSTP(NUMOBS))
      ELSE
         ALLOCATE (PSTP(1))
      END IF
      IF (DATTYP .EQ. 'RANK') THEN
         ALLOCATE (RANK(NUMOBS))
         ALLOCATE (YSORT(NUMOBS))
      ELSE
         ALLOCATE (RANK(1))
         ALLOCATE (YSORT(1))
      END IF
      ALLOCATE (BETAS(NIV))
      ALLOCATE (BETA(NIV))
      IF (PRPRES) THEN
         ALLOCATE (PRESS(NIV))
      ELSE
         ALLOCATE (PRESS(1))
      END IF
      ALLOCATE (IVAR(NIV))
      ALLOCATE (IXIVS(NIV))
      ALLOCATE (VMEANB(NIV))
      ALLOCATE (STDEVB(NIV))
      ALLOCATE (DIAGB(NIV))
      ALLOCATE (CORRB(IXSYM(NIV,NIV)))
      ALLOCATE (CORDVB(NIV))
      ALLOCATE (XINVER(IXSYM(NIV,NIV)))
      MAXRSQ = 2 * NIV
      ALLOCATE (IXRSQ(MAXRSQ))
      ALLOCATE (RSQ(0:MAXRSQ))

      DO 100 IDEP = 1, NDV
         IVDEP = NIV + IDEP
         IXDEP = IXMODL(IVDEP)

         CALL QAMESSAG (0, '+', 'Performing regression analysis on '
     &      // 'dependent variable ' // LABEL(IXDEP))
! add numvar in REGRVAR call - C.S. 07/27/2010
         CALL REGRVAR (
     &      (IDEP .EQ. 1), IUNOUT, NUMOBS, NUMVAR,
     &      VMEAN(IVDEP), DIAG(IVDEP),
     &      XYRAW(1,IXDEP), XYRNK(1,IXDEP), LABEL(IXDEP),
     &      NIV, IXMODL, LABEL, XYRAW, XYRNK, WTVAR, WTS,
     &      VMEAN, STDEV, DIAG, CORR, CORR(IXSYM(IVDEP,1)),
     &      TIME, YHAT, RESI, YHARAW, RESRAW,
     &      PSTP,
     &      RANK, YSORT, BETAS, BETA, PRESS,
     &      IVAR, IXIVS, VMEANB, STDEVB, DIAGB,
     &      CORRB, CORDVB, XINVER,
     &      MAXRSQ, IXRSQ, RSQ, INPUT_NAMES, SRC_VALUES, COEFF_DETERM,
     &      NBSTEPS )

  100 CONTINUE

      DEALLOCATE (TIME)
      DEALLOCATE (YHAT)
      DEALLOCATE (RESI)
      DEALLOCATE (YHARAW)
      DEALLOCATE (RESRAW)
      DEALLOCATE (PSTP)
      DEALLOCATE (RANK)
      DEALLOCATE (YSORT)
      DEALLOCATE (BETAS)
      DEALLOCATE (BETA)
      DEALLOCATE (PRESS)
      DEALLOCATE (IVAR)
      DEALLOCATE (IXIVS)
      DEALLOCATE (VMEANB)
      DEALLOCATE (STDEVB)
      DEALLOCATE (DIAGB)
      DEALLOCATE (CORRB)
      DEALLOCATE (CORDVB)
      DEALLOCATE (XINVER)
      DEALLOCATE (IXRSQ)
      DEALLOCATE (RSQ)

  110 CONTINUE
      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_REGRANAL.FOR
C *4    25-NOV-1996 09:40:45 APGILKE "Fix R-square table"
C *3    10-APR-1996 10:00:28 APGILKE "Add plot text file capability"
C *2     3-MAR-1996 11:36:21 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:10 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_REGRANAL.FOR
