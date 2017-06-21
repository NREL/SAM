C=======================================================================
      SUBROUTINE QAPRTSTR (IUCOUT, MSG)
C=======================================================================

C   --*** QAPRTSTR *** (CAMCON_LIB) Print message
C   --   Written by Amy Gilkey - revised 08/17/98
C   --
C   --QAPRTSTR prints a message to the terminal or an output file.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   MSG - IN char*(*) - the message

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER IUCOUT
      CHARACTER*(*) MSG

C   --Definitions for functions called:
      INTEGER IQAERRUNI
      INTEGER ISTRLEN

      INTEGER IUNOUT
      INTEGER LMSG
      INTEGER IDUM

      IUNOUT = IUCOUT
      IF (IUNOUT .LT. 0) THEN
       IUNOUT = IQAERRUNI (IUCOUT)
	ENDIF

      IF (MSG .NE. ' ') THEN
         LMSG = ISTRLEN(MSG)
         IF (IUNOUT .LE. 0) THEN
            IF (LMSG .GT. 132) LMSG = 132
            WRITE (*, 10000, IOSTAT=IDUM) MSG(:LMSG)
         ELSE
            WRITE (IUNOUT, 10000, IOSTAT=IDUM) MSG(:LMSG)
         END IF
10000     FORMAT (1X, A)
      ELSE
         IF (IUNOUT .LE. 0) THEN
            WRITE (*, *, IOSTAT=IDUM)
         ELSE
            WRITE (IUNOUT, *, IOSTAT=IDUM)
         END IF
      END IF

      RETURN
      END
