C=======================================================================
      LOGICAL FUNCTION QAHELP (PROG, TYPE, OPTION)
C=======================================================================

C   --*** QAHELP *** (CAMCON_LIB) Display HELP for a program
C   --   Written by Amy Gilkey - revised 06/01/95
C   --
C   --QAHELP displays help for a program in a system dependent fashion.
C   --
C   --It assumes that a help command has been established in CAMCON$HELP
C   --with an entry {PROG} {TYPE}.
C   --
C   --Parameters:
C   --   QAHELP - OUT logical - returns true iff the system has help for the
C   --      program
C   --   PROG - IN char*(*) - the program name; ' ' to use the program name
C   --      from QASETUP
C   --   TYPE - IN char*(*) - the HELP type (e.g., COMMANDS)
C   --   OPTION - IN char*(*) - the TYPE option (e.g., specific command)

C   --Common Variables:
C   --   Uses PROGRM of CAMCON_LIB common

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

      CHARACTER*(*) PROG
      CHARACTER*(*) TYPE
      CHARACTER*(*) OPTION

      LOGICAL EXHELP
      INTEGER ISTRLEN

      INTEGER LSTR
      INTEGER IDUM
      CHARACTER*16 QAPROG
      CHARACTER STR80*80

C   --Ensure the QA common is initialized
      CALL QA0INIT

      QAPROG = PROG
      IF (QAPROG .EQ. ' ') QAPROG = PROGRM

      STR80 = QAPROG//' '//TYPE//' '//OPTION
      CALL STRCMPRS (STR80, LSTR)

      QAHELP = EXHELP ('CAMCON$HELP', STR80(:LSTR))

      IF (.NOT. QAHELP) THEN
         WRITE (*, 10000, IOSTAT=IDUM)
     &      'Help for ', STR80(:ISTRLEN(STR80)), ' is not available'
      END IF

      RETURN

10000  FORMAT (/, 1X, 5A)
      END
