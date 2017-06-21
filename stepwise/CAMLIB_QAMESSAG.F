C=======================================================================
      SUBROUTINE QAMESSAG (IUCOUT, MSGTYP, MSG)
C=======================================================================

C   --*** QAMESSAG *** (CAMCON_LIB) Print message of a specified type
C   --   Written by Amy Gilkey - revised 01/06/96
C   --
C   --QAMESSAG prints a message to the terminal or an output file.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   MSGTYP - IN char*(*) - the type of message:
C   --      (case is unimportant)
C   --      'FATAL' prints a fatal error message
C   --      'PROGRAM' prints a program error message with instructions to
C   --         call the code sponsor
C   --      'ERROR' prints an error message
C   --      'WARNING' prints a warning
C   --      'CMDERR' prints an error message in response to user input
C   --      'CMDWARN' prints a warning in response to user input
C   --      'CMD' prints a message in response to user input
C   --      'CENTER' prints a centered message
C   --      'CONT' prints a line assuming it is a continuation of the previous
C   --         line
C   --      ' ' prints a message
C   --      If message type preceded by '+', a blank line is printed before
C   --         the message is printed
C   --   MSG - IN char*(*) - the message

C   --Common Variables:
C   --   Uses SPONSR of CAMCON_LIB common

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

C   --Parameter Definitions:
      INTEGER IUCOUT
      CHARACTER*(*) MSGTYP
      CHARACTER*(*) MSG

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER NB
      CHARACTER*8 TMPTYP

      INTEGER NCEN
      INTEGER LEN

      CHARACTER*80 BLANK
      SAVE BLANK
      INTEGER IBCONT
      SAVE IBCONT
      LOGICAL CONTERR
      SAVE CONTERR

      DATA BLANK / ' ' /
      DATA IBCONT / 0 /
      DATA CONTERR / .FALSE. /

      NCEN(LEN) = MAX (0, (80 - LEN) / 2)

C   --Ensure the QA common is initialized
      CALL QA0INIT

      IF (MSGTYP(1:1) .EQ. '+') THEN
         TMPTYP = MSGTYP(2:)
         CALL QAPRTSTR (IUCOUT, ' ')
      ELSE
         TMPTYP = MSGTYP
      END IF
      CALL STRUPCASE (TMPTYP)

      IF (TMPTYP .EQ. 'FATAL') THEN
         CALL QAPRTSTR (IUCOUT, '%%% FATAL ERROR - ' // MSG)
         IBCONT = 18
         CONTERR = .TRUE.
      ELSE IF (TMPTYP .EQ. 'PROGRAM') THEN
         CALL QAPRTSTR (IUCOUT, '%%% PROGRAM ERROR - ' // MSG)
         IBCONT = 20
         CONTERR = .TRUE.
         IF (SPONSR .NE. ' ') THEN
            IF (IBCONT .GT. 4) THEN
               CALL QAPRTSTR (IUCOUT, '%%% ' // BLANK(:IBCONT-4)
     &            // 'Call ' // SPONSR)
            ELSE
               CALL QAPRTSTR (IUCOUT, '%%% '
     &            // 'Call ' // SPONSR)
            END IF
         ELSE
            IF (IBCONT .GT. 4) THEN
               CALL QAPRTSTR (IUCOUT, '%%% ' // BLANK(:IBCONT-4)
     &            // 'Call the code sponsor')
            ELSE
               CALL QAPRTSTR (IUCOUT, '%%% '
     &            // 'Call the code sponsor')
            END IF
         END IF

      ELSE IF (TMPTYP .EQ. 'ERROR') THEN
         CALL QAPRTSTR (IUCOUT, '%%% ERROR - ' // MSG)
         IBCONT = 12
         CONTERR = .TRUE.
      ELSE IF (TMPTYP .EQ. 'WARNING') THEN
         CALL QAPRTSTR (IUCOUT, '%%% WARNING - ' // MSG)
         IBCONT = 14
         CONTERR = .TRUE.

      ELSE IF (TMPTYP .EQ. 'CMDERR') THEN
         CALL QAPRTSTR (IUCOUT, '%%% ERROR - ' // MSG)
         IBCONT = 12
         CONTERR = .TRUE.
      ELSE IF (TMPTYP .EQ. 'CMDWARN') THEN
         CALL QAPRTSTR (IUCOUT, '%%% WARNING - ' // MSG)
         IBCONT = 14
         CONTERR = .TRUE.
      ELSE IF (TMPTYP .EQ. 'CMD') THEN
         CALL QAPRTSTR (IUCOUT, '%%% ' // MSG)
         IBCONT = 4
         CONTERR = .TRUE.

      ELSE IF (TMPTYP .EQ. 'CENTER') THEN
         NB = NCEN(ISTRLEN(MSG))
         IF (NB .GT. 0) THEN
            CALL QAPRTSTR (IUCOUT, BLANK(:NB) // MSG)
         ELSE
            CALL QAPRTSTR (IUCOUT, MSG)
         END IF
         IBCONT = NB
         CONTERR = .FALSE.

      ELSE IF (TMPTYP .EQ. 'CONT') THEN
         IF (CONTERR) THEN
            IF (IBCONT .GT. 4) THEN
               CALL QAPRTSTR (IUCOUT, '%%% ' // BLANK(:IBCONT-4) // MSG)
            ELSE
               CALL QAPRTSTR (IUCOUT, '%%% ' // MSG)
            END IF
         ELSE
            IF (IBCONT .GT. 0) THEN
               CALL QAPRTSTR (IUCOUT, BLANK(:IBCONT) // MSG)
            ELSE
               CALL QAPRTSTR (IUCOUT, MSG)
            END IF
         END IF

      ELSE
         CALL QAPRTSTR (IUCOUT, MSG)
         IBCONT = 0
         CONTERR = .FALSE.
      END IF

      RETURN
      END
