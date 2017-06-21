C=======================================================================
      DOUBLE PRECISION FUNCTION FSUB (F, J, K)
C=======================================================================

C   --*** FSUB *** (STEPWISE)
C   --   Modified by Amy Gilkey - revised 11/14/91

      IMPLICIT NONE

      DOUBLE PRECISION F
      INTEGER J
      INTEGER K

      DOUBLE PRECISION ALGAMA

      DOUBLE PRECISION XA, XB
      DOUBLE PRECISION TEMP
      DOUBLE PRECISION XX
      DOUBLE PRECISION XC
      DOUBLE PRECISION AB
      DOUBLE PRECISION CON
      DOUBLE PRECISION SGN
      DOUBLE PRECISION TOP, BOT
      DOUBLE PRECISION XSUM
      DOUBLE PRECISION TERM

      IF (F .LE. 1000.) THEN
         XA = 0.5 * J
         XB = 0.5 * K
         TEMP = XB + XA*F
         XX = XA * F / TEMP
         IF ((F .GT. 0.0) .AND. (XX .GT. 0.0)) THEN
            XC = XB / TEMP
            AB = XA + XB
            IF (F .LT. 1.0) THEN
               TEMP = XA
               XA = XB
               XB = TEMP
               TEMP = XC
               XC = XX
               XX = TEMP
               CON = 1.0
               SGN = -1.0
            ELSE
               CON = 0.0
               SGN = 1.0
            END IF
            TOP = AB
            BOT = XB + 1.0
            XSUM = 1.0
            TERM = 1.0
  100       CONTINUE
            TEMP = XSUM
            TERM = TERM * XC * TOP / BOT
            XSUM = XSUM + TERM
            TOP = TOP + 1.0
            BOT = BOT + 1.0
            IF (XSUM .GT. TEMP) GOTO 100
            FSUB = CON + SGN * EXP (XA*LOG(XX) + XB*LOG(XC)
     &         + ALGAMA(AB) - ALGAMA(XA) - ALGAMA(XB)) * XSUM / XB
         ELSE
            FSUB = 1.0
         END IF
      ELSE
         IF (K .EQ. 1) THEN
            FSUB = 1.0E35
         ELSE
            FSUB = 0.0
         END IF
      END IF

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_FSUB.FOR
C *2     3-MAR-1996 11:36:08 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:04 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_FSUB.FOR
