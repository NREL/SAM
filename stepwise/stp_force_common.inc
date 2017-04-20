C *** stp_force_common.inc ***

      INTEGER MAXFRC
      PARAMETER (MAXFRC = 10)

      INTEGER NUMFRC
C      --NUMFRC - the number of forced variables
      INTEGER IXFRC(MAXFRC)
C      --IXFRC - the indices of the forced variables
      DOUBLE PRECISION SIGF(2)
C      --SIGF - the significance levels for stepwise regression or backward
C      --   elimination
      COMMON /FRCSIG/ SIGF, NUMFRC, IXFRC
C CMS REPLACEMENT HISTORY, Element STP_FORCE_COMMON.INC
C *2     3-MAR-1996 11:37:07 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:40 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_FORCE_COMMON.INC
