C=======================================================================
      LOGICAL FUNCTION QAMAXERR (DOABRT, INTNAM, MAXNAM, INTVAL, MAXVAL)
C=======================================================================

C   --*** QAMAXERR *** (CAMCON_LIB) Check integer value against maximum
C   --   Modified by Amy Gilkey - revised 01/06/96
C   --   Written by Jonathan S. Rath - 11/10/88
C   --
C   --QAMAXERR checks an integer value against a maximum value.  It returns
C   --.TRUE. if and only if the value is greater than the maximum.  If the
C   --value is over the maximum, an error message is printed and QAABORT is
C   --called (if requested).
C   --
C   --Parameters:
C   --   DOABRT - IN logical - QAABORT is called when the check fails if
C   --      DOABRT is .TRUE.
C   --   INTNAM - IN char*(*) - the description of the value being checked
C   --   MAXNAM - IN char*(*) - the variable name of the maximum value
C   --   INTVAL - IN/OUT integer - the value
C   --   MAXVAL - IN integer - the maximum value

      IMPLICIT NONE

C   --Parameter Definitions:
      LOGICAL DOABRT
      INTEGER INTVAL, MAXVAL
      CHARACTER*(*) INTNAM, MAXNAM

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER IDUM
      INTEGER LA, LB
      CHARACTER STR10A*10, STR10B*10
      CHARACTER*128 TXTMSG

      QAMAXERR = (INTVAL .GT. MAXVAL)

      IF (QAMAXERR) THEN
         WRITE (STR10A, 10000, IOSTAT=IDUM) INTVAL
         WRITE (STR10B, 10000, IOSTAT=IDUM) MAXVAL
10000     FORMAT ('(', I8, ')')
         CALL STRPACK (STR10A, LA)
         CALL STRPACK (STR10B, LB)
         WRITE (TXTMSG, 10010, IOSTAT=IDUM)
     &      INTNAM, STR10A(:LA), STR10B(:LB)
10010     FORMAT (A, ' ', A, ' exceeds maximum value ', A)
         LA = ISTRLEN (TXTMSG)
         IF (MAXNAM .NE. ' ') TXTMSG (LA+1:LA+1) = ';'
         CALL QAMESSAG (-1, '+ERROR', TXTMSG)
         IF (MAXNAM .NE. ' ') THEN
            TXTMSG = 'Change ' // MAXNAM // ' parameter'
            CALL QAMESSAG (-1, 'CONT', TXTMSG)
         END IF

         IF (DOABRT) THEN
            TXTMSG = INTNAM // ' exceeded maximum value'
            CALL QAABORT (TXTMSG)
         END IF
      END IF

      RETURN
      END
