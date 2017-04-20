C=======================================================================
      SUBROUTINE QABANNER (IUCOUT, LINE1, LINE2, LINE3)
C=======================================================================

C   --*** QABANNER *** (CAMCON_LIB) Print program banner
C   --   Written by Amy Gilkey - revised 11/02/98
C   --
C   --QABANNER prints the program banner to the terminal or an output file.
C   --The banner includes the program name in large letters, the program
C   --description, the version information, the author, the sponsor, the
C   --current run information, and information about the executable.
C   --
C   --Routine QASETUP must be called to set up the CAMCON_LIB common before
C   --QABANNER is called.
C   --
C   --Parameters:
C   --   IUCOUT - IN integer - the output file unit number:
C   --      >0 for output file unit number IUCOUT
C   --      =0 for the standard output device (terminal)
C   --      <0 for the defined diagnostics file
C   --   LINE1, LINE2, LINE3 - IN char*(*) - the three-line program description

C   --Common Variables:
C   --   Uses PROGRM, EXEVERS, EXEDAT, EXENAM, EXETYP, EXECLASS,
C   --      RUNDAT, RUNTIM, AUTHOR, SPONSR of CAMCON_LIB common
C
C   --Routines Called:
C   --   QABIGLET - (CAMCON_LIB) Print banner in big letters
C   --   QAMESSAG - (CAMCON_LIB) Print message to output file

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

C   --Parameter Definitions:
      INTEGER IUCOUT
      CHARACTER*(*) LINE1, LINE2, LINE3

C   --Definitions for functions called:
      INTEGER ISTRLEN

      LOGICAL DOBIG
      INTEGER NLIN
      INTEGER L
      CHARACTER*80 TLINE

C   --Ensure the QA common is initialized
c      CALL QA0INIT

      CALL QAMESSAG (IUCOUT, ' ', ' ')

C   --Check if program name should be printed in big letters;
C   --Check first banner line for "<NO>"
      DOBIG = .TRUE.
      L = 1
      IF (LINE1 .NE. ' ') THEN
         IF ((LINE1(1:4) .EQ. '<NO>')
     &      .OR. (LINE1(1:4) .EQ. '<no>')) THEN
            DOBIG = .FALSE.
            DO 100 L = 5, ISTRLEN(LINE1)
               IF (LINE1(L:L) .NE. ' ') GOTO 110
  100       CONTINUE
  110       CONTINUE
         END IF
      END IF

C   --Write the program name in big letters
      IF (PROGRM .NE. ' ') THEN
         IF (DOBIG) THEN
            CALL QABIGLET (IUCOUT, 80, PROGRM)
         ELSE
            CALL QAMESSAG (IUCOUT, 'CENTER',
     &         '*** ' // PROGRM(:ISTRLEN(PROGRM)) // ' ***')
         END IF
         CALL QAMESSAG (IUCOUT, ' ', ' ')
      END IF

C   --Write the up to 3-line program description
      NLIN = 0
      IF (LINE1 .NE. ' ') NLIN = 1
      IF (LINE2 .NE. ' ') NLIN = 2
      IF (LINE3 .NE. ' ') NLIN = 3
C   --Note: Start index L set above
      IF (NLIN .GE. 1) CALL QAMESSAG (IUCOUT, 'CENTER', LINE1(L:))
      IF (NLIN .GE. 2) CALL QAMESSAG (IUCOUT, 'CENTER', LINE2)
      IF (NLIN .GE. 3) CALL QAMESSAG (IUCOUT, 'CENTER', LINE3)
      IF (NLIN .GE. 1) CALL QAMESSAG (IUCOUT, ' ', ' ')

C   --Write the program version information
      TLINE = ' '
      L = 0
      IF (PROGRM .NE. ' ') THEN
         TLINE(L+1:) = PROGRM
         L = ISTRLEN(TLINE) + 1
      END IF
      IF (EXEVERS .NE. ' ') THEN
         TLINE(L+1:) = 'Version ' // EXEVERS
         L = ISTRLEN(TLINE) + 2
      END IF
      IF (TLINE .NE. ' ') CALL QAMESSAG (IUCOUT, 'CENTER', TLINE)
C   --Write the executable information
      IF (EXETYP .NE. ' ') THEN
         CALL QAMESSAG (IUCOUT, 'CENTER',
     &      EXETYP(:ISTRLEN(EXETYP))
     &      // ' ' // EXECLASS(:ISTRLEN(EXECLASS))
     &      // ' Built ' // EXEDAT10(:ISTRLEN(EXEDAT10)))
      ELSE
         CALL QAMESSAG (IUCOUT, 'CENTER',
     &      '* Not QA Approved *')
         IF (EXEDAT10 .NE. ' ') CALL QAMESSAG (IUCOUT, 'CENTER',
     &      ' Built ' // EXEDAT10(:ISTRLEN(EXEDAT10)))
      END IF

C   --Write the author and sponsor names
      IF (AUTHOR .NE. ' ') CALL QAMESSAG (IUCOUT, 'CENTER',
     &   'Written by ' // AUTHOR)
      IF (SPONSR .NE. ' ') CALL QAMESSAG (IUCOUT, 'CENTER',
     &   'Sponsored by ' // SPONSR)
      CALL QAMESSAG (IUCOUT, ' ', ' ')

C   --Write the current run information
      IF (RUNDAT10 .NE. ' ') CALL QAMESSAG (IUCOUT, 'CENTER',
     &   'Run on ' // RUNDAT10 // ' at ' // RUNTIM)
      IF (RUNMAC .NE. ' ') CALL QAMESSAG (IUCOUT, 'CENTER',
     &   'Run on ' // RUNMAC(:ISTRLEN(RUNMAC))
     &   // ' ' // RUNSYS(:ISTRLEN(RUNSYS)))

      RETURN
      END
