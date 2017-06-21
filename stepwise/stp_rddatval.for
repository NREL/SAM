C=======================================================================
      SUBROUTINE RDDATVAL (IUNDAT, DATFIL, IXVAR0, NUMOBS,
     &   NDROP, IXDROP, XYRAW, *)
C=======================================================================

C   --*** RDDATVAL *** (STEPWISE) Read data file data values
C   --   Modified by Amy Gilkey - revised 04/05/96
C   --
C   --RDDATVAL reads the data file variable values.  The data file is opened
C   --and closed in this routine and the header values are scanned (but not
C   --saved).  Some observations may be dropped.
C   --
C   --Parameters:
C   --   IUNDAT - IN - the data file unit number
C   --   DATFIL - IN - the data file name
C   --   IXVAR0 - IN/OUT - the index of the first variable to be read - 1
C   --   NUMOBS - IN - the number of observations to be saved
C   --   NDROP - IN - the number of observations to drop
C   --   IXDROP - IN - the numbers of the observations to drop
C   --   XYRAW - OUT - the variable data
C   --   * - error return, message printed

      IMPLICIT NONE

C     define maximum number of variables (2000) for RDUM
C      PARAMETER MAXVAR=2000
      INTEGER, PARAMETER :: MAXVAR=2000

      INTEGER IUNDAT
      CHARACTER*(*) DATFIL
      INTEGER IXVAR0
      INTEGER NUMOBS
      INTEGER NDROP
      INTEGER IXDROP(*)
      REAL XYRAW(NUMOBS,*)

      INTEGER LOCINT
      INTEGER ISTRLEN

      INTEGER IERR
      INTEGER NVAR
      INTEGER NSTE
      INTEGER NOBS
      INTEGER IOBS
      INTEGER IOBSIN
      INTEGER IV
      INTEGER IOSTAT
      INTEGER IDUM
!     give a dimension of RDUM
      REAL    RDUM(MAXVAR)
      CHARACTER*80 TXTMSG

C   --Open the data file
      CALL TRNIOPEN (IUNDAT, DATFIL, IERR)
      IF (IERR .NE. 0) GOTO 120

C   --Read the number of variables and observations
      CALL TRNISIZES (IUNDAT, NVAR, NSTE, NOBS, IERR)
      IF (IERR .NE. 0) GOTO 120
      IF (NVAR .LE. 0) GOTO 110

C   --Scan by the step data to get to variable values
      CALL TRNITIMES (IUNDAT, NSTE, RDUM, IERR)
      IF (IERR .NE. 0) GOTO 120

C   --Read in the variable values
      IOBS = 0
      DO 100 IOBSIN = 1, NOBS
         IF (LOCINT (IOBSIN, NDROP, IXDROP) .LE. 0) THEN
            IOBS = IOBS + 1
            READ (IUNDAT, *, IOSTAT=IOSTAT, ERR=130, END=130)
     &         (XYRAW(IOBS,IXVAR0+IV), IV=1,NVAR)
         ELSE
C           as RDUM has a dimension, read the values of RDUM(1)         
            READ (IUNDAT, *, IOSTAT=IOSTAT, ERR=130, END=130)
     &         (RDUM(1), IV=1,NVAR)
         END IF
  100 CONTINUE

      IXVAR0 = IXVAR0 + NVAR

  110 CONTINUE
C   --Close the data file
      CALL TRNICLOSE (IUNDAT, IDUM)

      RETURN

  120 CONTINUE
C   --Error encountered in reading data file(s)
      CALL QAMESSAG (-1, 'ERROR',
     &   'Error reading header of ' // DATFIL(:ISTRLEN(DATFIL)))
      RETURN 1

  130 CONTINUE
C   --Error encountered in reading data file(s)
      CALL QAMESSAG (-1, '+ERROR',
     &   'Error reading variable data from '
     &   // DATFIL(:ISTRLEN(DATFIL)))
      WRITE (TXTMSG, 10000, IOSTAT=IDUM) IOBSIN, IV, IOSTAT
10000  FORMAT ('Reading Vector', I4, 3X, 'Variable', I4,
     &   3X, 'Error status', I4)
      CALL QAMESSAG (-1, ' ', TXTMSG(:ISTRLEN(TXTMSG)))
      IF (IOSTAT .GE. 0) THEN
         CALL QAMESSAG (-1, ' ',
     &      'Error was flagged at the following line:')
         BACKSPACE (IUNDAT, IOSTAT=IDUM)
         READ (IUNDAT, '(A)', IOSTAT=IOSTAT) TXTMSG
         CALL QAMESSAG (-1, ' ', TXTMSG(:ISTRLEN(TXTMSG)))
      ELSE
         CALL QAMESSAG (-1, ' ', 'Premature end-of-file')
      END IF
      RETURN 1
      END
C CMS REPLACEMENT HISTORY, Element STP_RDDATVAL.FOR
C *2    10-APR-1996 10:00:22 APGILKE "Add error checking"
C *1     1-NOV-1995 11:24:08 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_RDDATVAL.FOR
