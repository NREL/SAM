C=======================================================================
      SUBROUTINE QASETUP (xPROG, xVERS, xDATE, INAUTH, INSPON)
C=======================================================================

C   --*** QASETUP *** (CAMCON_LIB) Startup program
C   --   Written by Amy Gilkey - revised 08/17/98
C   --
C   --QASETUP should be called at the start of any program.  It performs
C   --initialization details common to all programs.  Specifically, it
C   --   Initializes the CPU time
C   --   Fetches information from the executable
C   --   Sets the current run date and time
C   --   Sets the execution machine and operating system
C   --   Starts a new page on the terminal
C   --
C   --Parameters:
C   --   xPROG - IN char*(*) - the program name (not used if normal build)
C   --   xVERS - IN char*(*) - the program version (not used if normal build)
C   --   xDATE - IN char*(*) - not used
C   --   INAUTH - IN char*(*) - the program author (may be omitted if same as
C   --      sponsor)
C   --   INSPON - IN char*(*) - the program sponsor

C   --Common Variables:
C   --   Sets PROGRM, EXEVERS, EXEDAT,
C   --      EXEFULL, EXENAM, EXEDAT, EXETYP, EXECLASS, EXEVERS,
C   --      RUNDAT, RUNTIM, RUNMAC, RUNSYS, AUTHOR, SPONSR of CAMCON_LIB common

C   --Routines Called:
C   --   ISTRLEN - (CAMCON_LIB) Find string length
C   --   STRPACK - (CAMCON_LIB) Delete blanks from string
C   --   STRUPCASE - (CAMCON_LIB) Convert string to upper case

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_PROGRAM_COMMON.H'

C   --Parameter Definitions:
      CHARACTER*(*) xPROG, xVERS, xDATE, INAUTH, INSPON

      INTEGER ISTRLEN

      INTEGER IDUM
      REAL RDUM
      CHARACTER*32 NAME
      CHARACTER*128 TXTMSG

C   --Ensure the QA common is initialized
c      CALL QA0INIT

C   --Initialize the CPU time
      CALL EXCPUS (RDUM)

C   --Save the input information
      AUTHOR = INAUTH
      SPONSR = INSPON

C   --Set the system hardware and software
C      CALL EXPARM (RUNMAC, RUNSYS, IDUM, IDUM, IDUM, IDUM)
      RUNMAC='i586'
	RUNSYS='LINUX'

C   --Set the run date and time
      CALL EXDATE (RUNDAT10)
      CALL EXTIME (RUNTIM)
      RUNDAT8 = RUNDAT10(1:6) // RUNDAT10(9:10)

C   --Set information about the executable
      EXEFULL = ' '
      CALL EXEXEINFO (EXEFULL,
     &   EXENAM, EXEDAT10, EXETYP, EXECLASS, EXEVERS, IDUM)
      EXEDAT8 = EXEDAT10(1:6) // EXEDAT10(9:10)

C   --Set the name from the build
      PROGRM = EXENAM

C   --Set program name and version from passed parameters if code not linked
C   --with normal build (non-QA program)
      IF ((EXETYP .EQ. ' ') .AND. (EXEVERS .EQ. ' ')) THEN
         IF (PROGRM .EQ. ' ') PROGRM = xPROG
         EXEVERS = xVERS
      END IF

      RETURN
      END
