C=======================================================================
      SUBROUTINE DUMPVAR (IUNOUT, TITLE, DATTYP,
     &   NUMOBS, NDUMPVAR, IXDUMP, LABEL, XYRNK)
C=======================================================================

C   --*** DUMPVAR *** (STEPWISE) Dump raw data values
C   --   Modified by Amy Gilkey - revised 04/09/96
C   --
C   --DUMPVAR prints the data values to a file for debugging.
C   --
C   --Parameters:
C   --   IUNOUT - IN - the listing file unit number
C   --   TITLE - IN - the title for the listing
C   --   DATTYP - IN - the type of the data being printed (RAW, RANK, STAND-01)
C   --   NUMOBS - IN - the number of observations
C   --   NDUMPVAR - IN - the number of independent and dependent variables
C   --      to be dumped
C   --   IXDUMP - IN - the indices of the independent and dependent variables
C   --      to be dumped
C   --   LABEL - IN - the variable labels
C   --   XYRNK - IN - the variable data (rank if 'RANK')

      IMPLICIT NONE

      INTEGER IUNOUT
      CHARACTER*(*) TITLE
      CHARACTER*(*) DATTYP
      INTEGER NUMOBS
      INTEGER NDUMPVAR
      INTEGER IXDUMP(*)
      CHARACTER*8 LABEL(*)
      REAL XYRNK(NUMOBS,*)

      INTEGER IVAR
      INTEGER ISV, IEV, IV
      INTEGER IOBS
      INTEGER IDUM

      CALL QAPAGE (IUNOUT, 'PAGE')
      CALL QAMESSAG (IUNOUT, 'CENTER', TITLE)

      IF (DATTYP .EQ. 'RANK') THEN
         WRITE (IUNOUT, 10000, IOSTAT=IDUM) 'RANK'
      ELSE IF (DATTYP .EQ. 'STD01') THEN
         WRITE (IUNOUT, 10000, IOSTAT=IDUM) 'STANDARD 0-1'
      ELSE
         WRITE (IUNOUT, 10000, IOSTAT=IDUM) 'INPUT'
      END IF

      DO 110 IVAR = 1, NDUMPVAR, 6
         ISV = IVAR
         IEV = MIN (NDUMPVAR, ISV+6-1)
         WRITE (IUNOUT, 10010, IOSTAT=IDUM)
     &      (LABEL(IXDUMP(IV)), IV=ISV,IEV)
         DO 100 IOBS = 1, NUMOBS
            IF (DATTYP .EQ. 'RANK') THEN
               WRITE (IUNOUT, 10020, IOSTAT=IDUM)
     &            IOBS, (XYRNK(IOBS,IXDUMP(IV)), IV=ISV,IEV)
            ELSE
               WRITE (IUNOUT, 10030, IOSTAT=IDUM)
     &            IOBS, (XYRNK(IOBS,IXDUMP(IV)), IV=ISV,IEV)
            END IF
  100    CONTINUE
  110 CONTINUE

      RETURN
10000  FORMAT (/, 1X, '*** ', A,
     &   ' Selected Variables', ' ***')
10010  FORMAT (/, 1X, 'Obs', 1X, 6 (4X, A8, :))
10020  FORMAT (1X, I3, 1X, 6F12.3)
10030  FORMAT (1X, I3, 1X, 1P6E12.4)
      END
C CMS REPLACEMENT HISTORY, Element PCC_DUMPRAWVAR.FOR
C *1     1-NOV-1995 11:18:37 APGILKE "Initial load - Source file"
C CMS REPLACEMENT HISTORY, Element PCC_DUMPRAWVAR.FOR
C CMS REPLACEMENT HISTORY, Element STP_DUMPVAR.FOR
C *2    10-APR-1996 09:59:51 APGILKE "Select variables to be dumped"
C *1     4-DEC-1995 19:12:51 APGILKE "DUMP variable for debugging"
C CMS REPLACEMENT HISTORY, Element STP_DUMPVAR.FOR
